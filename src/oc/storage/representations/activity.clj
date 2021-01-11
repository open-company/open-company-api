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

(defn- is-inbox? [collection-type]
  (= collection-type "inbox"))

(defn- is-contributions? [collection-type]
  (= collection-type "contributions"))

(defn- is-replies? [collection-type]
  (= collection-type "replies"))

(defn- refresh-link
  "Add `next` and/or `prior` links for pagination as needed."
  [org collection-type {:keys [sort-type author-uuid following unfollowing]} data]
  (when (seq (:activity data))
    (let [resources-list (:activity data)
          last-resource (last resources-list)
          last-resource-date (:sort-value last-resource)
          replies? (is-replies? collection-type)
          contributions? (is-contributions? collection-type)
          inbox? (is-inbox? collection-type)
          no-collection-type? (and (not replies?)
                                   (not inbox?)
                                   (not contributions?)
                                   (not following)
                                   (not unfollowing))
          refresh-url (cond->> {:direction :after :start last-resource-date :following following :unfollowing unfollowing}
                       replies?            (activity-urls/replies org)
                       inbox?              (activity-urls/collection collection-type org)
                       contributions?      (activity-urls/contributions org author-uuid sort-type)
                       following           (activity-urls/following collection-type org sort-type)
                       unfollowing         (activity-urls/unfollowing collection-type org sort-type)
                       ;; else
                       no-collection-type? (activity-urls/activity collection-type org sort-type))]
      (when refresh-url
        (hateoas/link-map "refresh" hateoas/GET refresh-url {:accept mt/entry-collection-media-type})))))

(defn- pagination-link
  "Add `next` and/or `prior` links for pagination as needed."
  [org collection-type {:keys [direction sort-type author-uuid following unfollowing]} data]
  (when (seq (:activity data))
    (let [replies? (is-replies? collection-type)
          inbox? (is-inbox? collection-type)
          contributions? (is-contributions? collection-type)
          no-collection-type? (and (not replies?)
                                   (not inbox?)
                                   (not contributions?)
                                   (not following)
                                   (not unfollowing))
          resources-list (:activity data)
          last-resource (last resources-list)
          last-resource-date (:sort-value last-resource)
          next? (= (:next-count data) config/default-activity-limit)
          next-url (when next?
                     (cond->> {:direction direction :start last-resource-date :following following :unfollowing unfollowing}
                       replies?            (activity-urls/replies org)
                       inbox?              (activity-urls/collection collection-type org)
                       contributions?      (activity-urls/contributions org author-uuid sort-type)
                       following           (activity-urls/following collection-type org sort-type)
                       unfollowing         (activity-urls/unfollowing collection-type org sort-type)
                       ;; else
                       no-collection-type? (activity-urls/activity collection-type org sort-type)))]
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
  (let [sort-type (:sort-type params)
        following? (:following params)
        unfollowing? (:unfollowing params)
        inbox? (is-inbox? collection-type)
        replies? (is-replies? collection-type)
        contributions? (is-contributions? collection-type)
        collection-url (cond
                        replies?
                        (org-urls/replies org)
                        inbox?
                        (activity-urls/collection collection-type org {:following following? :unfollowing unfollowing?})
                        contributions?
                        (activity-urls/contributions org (:author-uuid params) (:sort-type params))
                        following?
                        (activity-urls/following collection-type org sort-type)
                        unfollowing?
                        (activity-urls/unfollowing collection-type org sort-type)
                        :else
                        (activity-urls/activity collection-type org sort-type))
        recent-activity-sort? (= sort-type :recent-activity)
        other-sort-url (cond
                        (or inbox? replies?)
                        nil
                        contributions?
                        (activity-urls/contributions org (:author-uuid params) (if recent-activity-sort? :recently-posted :recent-activity))
                        following?
                        (activity-urls/following collection-type org (if recent-activity-sort? :recently-posted :recent-activity))
                        unfollowing?
                        (activity-urls/unfollowing collection-type org (if recent-activity-sort? :recently-posted :recent-activity))
                        :else
                        (activity-urls/activity collection-type org (if recent-activity-sort? :recently-posted :recent-activity)))
        collection-rel (cond
                         following?
                         "following"
                         unfollowing?
                         "unfollowing"
                         replies?
                         "replies"
                         recent-activity-sort?
                         "activity"
                         :else
                         "self")
        other-sort-rel (if recent-activity-sort? "self" "activity")
        links (remove nil?
               [(hateoas/link-map collection-rel hateoas/GET collection-url {:accept mt/entry-collection-media-type} {})
                (if inbox? ;; Inbox has no sort
                  (hateoas/link-map "dismiss-all" hateoas/POST (activity-urls/inbox-dismiss-all org) {:accept mt/entry-media-type
                                                                                                      :content-type "text/plain"} {})
                  (when-not replies?
                    (hateoas/link-map other-sort-rel hateoas/GET other-sort-url {:accept mt/entry-collection-media-type} {})))
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