(ns oc.storage.representations.activity
  "Resource representations for OpenCompany activity."
  (:require [cheshire.core :as json]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.api.access :as access]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.org :as org-rep]
            [oc.storage.representations.entry :as entry-rep]
            [oc.storage.resources.reaction :as reaction-res]
            [oc.storage.config :as config]))

(defn url [{slug :slug} sort-type {start :start direction :direction}]
  (let [sort-path (if (= sort-type "recent-activity") "recent-activity" "activity")]
    (str "/orgs/" slug "/" sort-path "?start=" start "&direction=" (name direction))))

(defn- pagination-links
  "Add `next` and/or `prior` links for pagination as needed."
  [org sort-type {:keys [start start? direction]} data]
  (let [activity (:activity data)
        activity? (not-empty activity)
        last-activity (last activity)
        first-activity (first activity)
        last-activity-date (when activity? (or (:published-at last-activity) (:created-at last-activity)))
        first-activity-date (when activity? (or (:published-at first-activity) (:created-at first-activity)))
        next? (or (= (:direction data) :previous)
                  (= (:next-count data) config/default-activity-limit))
        next-url (when next? (url org sort-type {:start last-activity-date :direction :before}))
        next-link (when next-url (hateoas/link-map "next" hateoas/GET next-url {:accept mt/activity-collection-media-type}))
        prior? (and start?
                    (or (= (:direction data) :next)
                        (= (:previous-count data) config/default-activity-limit)))
        prior-url (when prior? (url org sort-type {:start first-activity-date :direction :after}))
        prior-link (when prior-url (hateoas/link-map "previous" hateoas/GET prior-url {:accept mt/activity-collection-media-type}))]
    (remove nil? [next-link prior-link])))

(defn render-activity-for-collection
  "Create a map of the activity for use in a collection in the API"
  [org activity comments reactions access-level user-id]
  (entry-rep/render-entry-for-collection org {:slug (:board-slug activity)
                                              :access (:board-access activity)
                                              :uuid (:board-uuid activity)}
    activity comments reactions access-level user-id))

(defn render-activity-list
  "
  Given an org and a sequence of entry maps, create a JSON representation of a list of
  activity for the API.
  "
  [params org sort-type activity boards user]
  (let [collection-url (url org sort-type params)
        other-sort-url (url org (if (= sort-type :recent-activity) :recently-posted :recent-activity) params)
        links [(hateoas/self-link collection-url {:accept mt/activity-collection-media-type})
               (hateoas/self-link other-sort-url {:accept mt/activity-collection-media-type})
               (hateoas/up-link (org-rep/url org) {:accept mt/org-media-type})]
        full-links (concat links (pagination-links org sort-type params activity))]
    (json/generate-string
      {:collection {:version hateoas/json-collection-version
                    :href collection-url
                    :links full-links
                    :items (map (fn [entry]
                                  (let [board (first (filterv #(= (:slug %) (:board-slug entry)) boards))
                                        access-level (access/access-level-for org board user)]
                                   (render-activity-for-collection org entry
                                     (entry-rep/comments entry)
                                     (reaction-res/aggregate-reactions (entry-rep/reactions entry))
                                     (:access-level access-level) (:user-id user)))) (:activity activity))}}
      {:pretty config/pretty?})))
