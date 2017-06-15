(ns oc.storage.representations.entry
  "Resource representations for OpenCompany entries."
  (:require [defun.core :refer (defun defun-)]
            [cheshire.core :as json]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.config :as config]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.board :as board-rep]))

(def representation-props [:uuid :topic-slug :title :headline :body :body-placeholder :image-url :image-height :image-width
                           :chart-url :attachments :author :created-at :updated-at])

(defun url
  
  ([org-slug board-slug topic-slug :guard string?]
  (str "/orgs/" org-slug "/boards/" board-slug "/topics/" (name topic-slug)))
  
  ([org-slug board-slug entry :guard map?] (url org-slug board-slug (name (:topic-slug entry))))

  ([org-slug board-slug topic-slug :guard string? entry-uuid]
  (str "/orgs/" org-slug "/boards/" board-slug "/topics/" (name topic-slug) "/entries/" entry-uuid))

  ([org-slug board-slug entry :guard map? entry-uuid] (url org-slug board-slug (name (:topic-slug entry)) entry-uuid)))

(defun interaction-url

  ([org-uuid board-uuid topic-slug entry-uuid]
  (str config/interaction-server-url (url org-uuid board-uuid topic-slug entry-uuid) "/comments"))

  ([org-uuid board-uuid topic-slug entry-uuid reaction]
  (str config/interaction-server-url (url org-uuid board-uuid topic-slug entry-uuid) "/reactions/" reaction "/on")))

(defn- self-link [org-slug board-slug entry entry-uuid]
  (hateoas/self-link (url org-slug board-slug entry entry-uuid) {:accept mt/entry-media-type}))

(defn- item-link [org-slug board-slug entry entry-uuid]
  (hateoas/item-link (url org-slug board-slug entry entry-uuid) {:accept mt/entry-media-type}))

(defn- create-link [org-slug board-slug topic-slug]
  (hateoas/create-link (str (url org-slug board-slug topic-slug) "/") {:content-type mt/entry-media-type
                                                                       :accept mt/entry-media-type}))

(defn- partial-update-link [org-slug board-slug topic-slug entry-uuid]
  (hateoas/partial-update-link (url org-slug board-slug topic-slug entry-uuid) {:content-type mt/entry-media-type
                                                                                :accept mt/entry-media-type}))

(defn- delete-link [org-slug board-slug topic-slug entry-uuid]
  (hateoas/delete-link (url org-slug board-slug topic-slug entry-uuid)))

(defn- archive-link [org-slug board-slug topic-slug]
  (hateoas/archive-link (url org-slug board-slug topic-slug)))

(defn- collection-link [org-slug board-slug topic-slug entry-count]
  (hateoas/collection-link (url org-slug board-slug topic-slug) {:accept mt/entry-collection-media-type}
                                                                {:count (or entry-count 1)}))

(defn- up-link [org-slug board-slug topic-slug] (hateoas/up-link 
                                        (url org-slug board-slug topic-slug) {:accept mt/entry-collection-media-type}))

(defn- comment-link [org-uuid board-uuid topic-slug entry-uuid]
  (let [comment-url (str (interaction-url org-uuid board-uuid topic-slug entry-uuid) "/")]
    (hateoas/link-map "comment" hateoas/POST comment-url {:content-type mt/comment-media-type
                                                          :accept mt/comment-media-type})))

(defn- comments-link [org-uuid board-uuid topic-slug entry-uuid comment-count]
  (let [comment-url (interaction-url org-uuid board-uuid topic-slug entry-uuid)]
    (hateoas/link-map "comments" hateoas/GET comment-url {:accept mt/comment-collection-media-type}
                                                          {:count comment-count})))

(defn- react-link [org-uuid board-uuid topic-slug entry-uuid reaction]
  (let [react-url (interaction-url org-uuid board-uuid topic-slug entry-uuid reaction)]
    (hateoas/link-map "react" hateoas/PUT react-url {})))

(defn- unreact-link [org-uuid board-uuid topic-slug entry-uuid reaction]
  (let [react-url (interaction-url org-uuid board-uuid topic-slug entry-uuid reaction)]
    (hateoas/link-map "react" hateoas/DELETE react-url {})))

(defn- entry-collection-links
  "Given an entry, return the entry with the appropriate `:links` added for the specified access level."
  [entry entry-count entry-uuid board-slug org-slug access-level]
  (let [topic-slug (name (:topic-slug entry))
        links [(collection-link org-slug board-slug entry entry-count)
               (item-link org-slug board-slug entry entry-uuid)]
        full-links (if (= access-level :author)
                      (concat links [(partial-update-link org-slug board-slug topic-slug entry-uuid)
                                     (delete-link org-slug board-slug topic-slug entry-uuid)
                                     (create-link org-slug board-slug topic-slug)
                                     (archive-link org-slug board-slug topic-slug)])
                      links)]
    (assoc entry :links full-links)))

(defn- map-kv
  "Utility function to do an operation on the value of every key in a map."
  [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn- reaction-and-link
  "Given the parts of a reaction URL, return a map representation of the reaction for use in the API."
  [org-uuid board-uuid topic-slug entry-uuid reaction reaction-count user?]
  {:reaction reaction
   :reacted (if user? true false)
   :count reaction-count
   :links [(if user?
              (unreact-link org-uuid board-uuid topic-slug entry-uuid reaction)
              (react-link org-uuid board-uuid topic-slug entry-uuid reaction))]})

(defn- reactions-and-links
  "
  Given a sequence of reactions and the parts of a reaction URL, return a representation of the reactions
  for use in the API.
  "
  [org-uuid board-uuid topic-slug entry-uuid reactions user-id]
  (let [grouped-reactions (merge (apply hash-map (interleave config/default-reactions (repeat []))) ; defaults
                                 (group-by :reaction reactions)) ; reactions grouped by unicode character
        counted-reactions-map (map-kv count grouped-reactions) ; how many for each character?
        counted-reactions (map #(vec [% (get counted-reactions-map %)]) (keys counted-reactions-map)) ; map -> sequence
        top-three-reactions (take 3 (reverse (sort-by last counted-reactions)))] ; top 3 unicode characters by how many
    (map #(reaction-and-link org-uuid board-uuid topic-slug entry-uuid (first %) (last %)
            (some (fn [reaction] (= user-id (-> reaction :author :user-id))) ; did the user leave one of this reaction?
              (get grouped-reactions (first %)))) 
      top-three-reactions)))

(defn entry-and-links
  "
  Given an entry and all the metadata about it, render an access level appropriate rendition of the entry
  for use in an API response.
  "
  ([entry entry-uuid board-slug org-slug comment-count reactions access-level user-id]
    (let [topic-slug (name (:topic-slug entry))
          org-uuid (:org-uuid entry)
          board-uuid (:board-uuid entry)
          reactions (if (= access-level :public)
                      []
                      (reactions-and-links org-uuid board-uuid topic-slug entry-uuid reactions user-id))
          links [(self-link org-slug board-slug (name topic-slug) entry-uuid)
                 (up-link org-slug board-slug topic-slug)]
          full-links (cond
                      (= access-level :author)
                      (concat links [(partial-update-link org-slug board-slug entry entry-uuid)
                                     (delete-link org-slug board-slug entry entry-uuid)
                                     (create-link org-slug board-slug topic-slug)
                                     (archive-link org-slug board-slug topic-slug)
                                     (comment-link org-uuid board-uuid topic-slug entry-uuid)
                                     (comments-link org-uuid board-uuid topic-slug entry-uuid comment-count)])

                      (= access-level :viewer)
                      (concat links [(comment-link org-slug board-slug topic-slug entry-uuid)
                                     (comments-link org-uuid board-uuid topic-slug entry-uuid comment-count)])

                      :else links)]
      (-> (select-keys entry representation-props)
        (assoc :reactions reactions)
        (assoc :links full-links))))

  ([entry entry-uuid board-slug org-slug entry-count comment-count reactions access-level user-id]
    (let [topic-slug (name (:topic-slug entry))
          org-uuid (:org-uuid entry)
          board-uuid (:board-uuid entry)
          reactions (if (= access-level :public)
                      []
                      (reactions-and-links org-uuid board-uuid topic-slug entry-uuid reactions user-id))
          links [(self-link org-slug board-slug (name topic-slug) entry-uuid)
                 (up-link org-slug board-slug topic-slug)]
          full-links (cond
                      (= access-level :author)
                      (concat links [(collection-link org-slug board-slug entry entry-count)
                                     (partial-update-link org-slug board-slug entry entry-uuid)
                                     (delete-link org-slug board-slug entry entry-uuid)
                                     (create-link org-slug board-slug topic-slug)
                                     (archive-link org-slug board-slug topic-slug)
                                     (comment-link org-uuid board-uuid topic-slug entry-uuid)
                                     (comments-link org-uuid board-uuid topic-slug entry-uuid comment-count)])

                      (= access-level :viewer)
                      (concat links [(comment-link org-slug board-slug topic-slug entry-uuid)
                                     (comments-link org-uuid board-uuid topic-slug entry-uuid comment-count)])

                      :else links)]
      (-> (select-keys entry representation-props)
        (assoc :reactions reactions)
        (assoc :links full-links)))))

(defn render-entry-for-collection
  "Create a map of the entry for use in a collection in the API"
  [org-slug board-slug entry entry-count comment-count reactions access-level user-id]
    (let [entry-uuid (:uuid entry)]
      (entry-and-links entry entry-uuid board-slug org-slug entry-count comment-count reactions access-level user-id)))

(defn render-entry
  "Create a JSON representation of the entry for the API"
  [org-slug board-slug entry comment-count reactions access-level user-id]
  (let [entry-uuid (:uuid entry)]
    (json/generate-string
      (entry-and-links entry entry-uuid board-slug org-slug comment-count reactions access-level user-id)
      {:pretty config/pretty?})))

(defn render-entry-list
  "
  Given a org and board slug and a sequence of entry maps, create a JSON representation of a list of
  entries for the API.
  "
  [org-slug board-slug topic-slug entries interactions access-level user-id]
  (let [collection-url (url org-slug board-slug topic-slug)
        links [(hateoas/self-link collection-url {:accept mt/entry-collection-media-type})
               (hateoas/up-link (board-rep/url org-slug board-slug) {:accept mt/board-media-type})]
        full-links (if (= access-level :author)
                      (concat links [(create-link org-slug board-slug topic-slug)
                                     (archive-link org-slug board-slug topic-slug)])
                      links)]
    (json/generate-string
      {:collection {:version hateoas/json-collection-version
                    :href collection-url
                    :links full-links
                    :items (map #(entry-and-links % (:uuid %) board-slug org-slug
                                    (count (or (filter :body (get interactions (:uuid %))) []))  ; comments only
                                    (or (filter :reaction (get interactions (:uuid %))) []) ; reactions only
                                    access-level user-id)
                             entries)}}
      {:pretty config/pretty?})))