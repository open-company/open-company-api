(ns oc.storage.representations.activity
  "Resource representations for OpenCompany activity."
  (:require [cheshire.core :as json]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.api.access :as access]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.urls.org :as org-urls]
            [oc.storage.representations.entry :as entry-rep]
            [oc.storage.resources.reaction :as reaction-res]
            [oc.storage.urls.activity :as activity-urls]
            [oc.storage.config :as config]))

(defn- is-contributions? [collection-type]
  (= collection-type "contributions"))

(defn- is-replies? [collection-type]
  (= collection-type "replies"))

(defn- is-bookmarks? [collection-type]
  (= collection-type "bookmarks"))

(defn- refresh-link
  "Add a single link to reload the whole set of posts loaded until this point."
  [org collection-type {:keys [author-uuid following unfollowing]} data]
  (when (seq (:activity data))
    (let [resources-list (:activity data)
          last-resource (last resources-list)
          last-resource-date (:sort-value last-resource)
          replies? (is-replies? collection-type)
          contributions? (is-contributions? collection-type)
          bookmarks? (is-bookmarks? collection-type)
          no-collection-type? (and (not replies?)
                                   (not contributions?)
                                   (not bookmarks?)
                                   (not following)
                                   (not unfollowing))
          refresh-url (cond->> {:refresh true :direction :after :start last-resource-date :following following :unfollowing unfollowing}
                       replies?            (activity-urls/replies org)
                       bookmarks?          (activity-urls/bookmarks org)
                       contributions?      (activity-urls/contributions org author-uuid)
                       following           (activity-urls/following collection-type org)
                       unfollowing         (activity-urls/unfollowing collection-type org)
                       ;; else
                       no-collection-type? (activity-urls/activity collection-type org))]
      (when refresh-url
        (hateoas/link-map "refresh" hateoas/GET refresh-url {:accept mt/entry-collection-media-type})))))

(defn- pagination-link
  "Add `next` and/or `prior` links for pagination as needed."
  [org collection-type {:keys [direction author-uuid following unfollowing]} data]
  (when (seq (:activity data))
    (let [replies? (is-replies? collection-type)
          contributions? (is-contributions? collection-type)
          bookmarks? (is-bookmarks? collection-type)
          no-collection-type? (and (not replies?)
                                   (not contributions?)
                                   (not is-bookmarks?)
                                   (not following)
                                   (not unfollowing))
          resources-list (:activity data)
          last-resource (last resources-list)
          last-resource-date (:sort-value last-resource)
          next? (= (:next-count data) config/default-activity-limit)
          next-url (when next?
                     (cond->> {:direction direction :start last-resource-date :following following :unfollowing unfollowing}
                       replies?            (activity-urls/replies org)
                       bookmarks?          (activity-urls/bookmarks org)
                       contributions?      (activity-urls/contributions org author-uuid)
                       following           (activity-urls/following collection-type org)
                       unfollowing         (activity-urls/unfollowing collection-type org)
                       ;; else
                       no-collection-type? (activity-urls/activity collection-type org)))]
      (when next-url
        (hateoas/link-map "next" hateoas/GET next-url {:accept mt/entry-collection-media-type})))))

(defn render-activity-for-collection
  "Create a map of the activity for use in a collection in the API"
  [org activity comments reactions access-level user-id]
  (entry-rep/render-entry-for-collection org {:slug (:board-slug activity)
                                              :access (:board-access activity)
                                              :uuid (:board-uuid activity)
                                              :publisher-board (:publisher-board activity)}
    activity comments reactions access-level user-id))

(defn render-activity-list
  "
  Given an org and a sequence of entry maps, create a JSON representation of a list of
  activity for the API.
  "
  [params org collection-type activity boards user]
  (let [following? (:following params)
        unfollowing? (:unfollowing params)
        bookmarks? (is-bookmarks? collection-type)
        replies? (is-replies? collection-type)
        contributions? (is-contributions? collection-type)
        collection-url (cond
                        replies?
                        (activity-urls/replies org)
                        bookmarks?
                        (activity-urls/bookmarks org)
                        contributions?
                        (activity-urls/contributions org (:author-uuid params))
                        following?
                        (activity-urls/following collection-type org)
                        unfollowing?
                        (activity-urls/unfollowing collection-type org)
                        :else
                        (activity-urls/activity collection-type org))
        collection-rel (cond
                         following?
                         "following"
                         unfollowing?
                         "unfollowing"
                         replies?
                         "replies"
                         bookmarks?
                         "bookmarks"
                         :else
                         "self")
        links (remove nil?
               [(hateoas/link-map collection-rel hateoas/GET collection-url {:accept mt/entry-collection-media-type} {})
                (hateoas/up-link (org-urls/org org) {:accept mt/org-media-type})
                (pagination-link org collection-type params activity)
                (refresh-link org collection-type params activity)])
        base-response (as-> {} b
                        (if (seq (:last-seen-at params))
                          (assoc b :last-seen-at (:last-seen-at params))
                          b)
                        (if (seq (:next-seen-at params))
                          (assoc b :next-seen-at (:next-seen-at params))
                          b)
                        (if contributions?
                          (assoc b :author-uuid (:author-uuid params))
                          b)
                        (if (seq (:container-id params))
                          (assoc b :container-id (:container-id params))
                          b))]
    (json/generate-string
      {:collection (merge base-response
                    {:version hateoas/json-collection-version
                     :href collection-url
                     :links links
                     :direction (:direction params)
                     :start (:start params)
                     :total-count (:total-count activity)
                     :items (map (fn [entry]
                                   (let [board (first (filterv #(= (:slug %) (:board-slug entry)) boards))
                                         access-level (access/access-level-for org board user)]
                                    (render-activity-for-collection org entry
                                      (entry-rep/comments entry)
                                      (reaction-res/aggregate-reactions (entry-rep/reactions entry))
                                      access-level (:user-id user)))) (:activity activity))})}
      {:pretty config/pretty?})))