(ns oc.storage.resources.common
  "Resources are any thing stored in the open company platform: orgs, boards, topics, updates"
  (:require [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.slugify :as slug]
            [oc.storage.config :as config]))

;; ----- RethinkDB metadata -----

(def org-table-name "orgs")
(def board-table-name "boards")
(def entry-table-name "entries")
(def update-table-name "updates")
(def interaction-table-name "interactions")

;; ----- Properties common to all resources -----

(def reserved-properties
  "Properties of a resource that can't be specified during a create and are ignored during an update."
  #{:id :slug :uuid :board-uuid :org-uuid :author :links :created-at :updated-at})

;; ----- Topic definitions -----

(def topic-slugs "All topic slugs as a set of keywords" (:topics config/topics))

(def topics-by-slug "All topic templates as a map from their name" (:templates config/topics))

(def custom-topic-slug "Regex that matches properly named custom topics" #"^custom-.{4}$")

(defn topic-slug? 
  "Return true if the argument is a valid named or custom topic slug."
  [topic-slug]
  (and
    (or (string? topic-slug) (keyword? topic-slug))
    (or (topic-slugs (keyword topic-slug))
        (re-matches custom-topic-slug (name topic-slug)))))

;; ----- Data Schemas -----

(def TopicSlug "Known topic slugs and custom topics." (schema/pred topic-slug?))

(def Slug "Valid slug used to uniquely identify a resource in a visible URL." (schema/pred slug/valid-slug?))

(def TopicOrder "A sequence of topic slugs."
  (schema/pred #(and
    (sequential? %) ; it is sequential
    (every? topic-slug? %) ; everything in it is a topic name
    (= (count (set %)) (count %))))) ; there are no duplicates

(def Attachment {
  :file-name lib-schema/NonBlankStr
  :file-type lib-schema/NonBlankStr
  :file-size schema/Num
  :file-url lib-schema/NonBlankStr
  :created-at lib-schema/ISO8601})

(def EntryAuthor
  (merge lib-schema/Author {:updated-at lib-schema/ISO8601}))

(def UpdateEntry {
  :uuid lib-schema/UniqueID
  :topic-slug TopicSlug
  :title lib-schema/NonBlankStr
  :headline schema/Str
  :body schema/Str
  (schema/optional-key :image-url) (schema/maybe schema/Str)
  (schema/optional-key :image-height) schema/Num
  (schema/optional-key :image-width) schema/Num
  
  ;; Attachments
  (schema/optional-key :attachments) [Attachment]
  ;; Charts
  (schema/optional-key :chart-url) (schema/maybe schema/Str)
  
  :author [EntryAuthor]
  :created-at lib-schema/ISO8601
  :updated-at lib-schema/ISO8601})

(def Entry
  (merge UpdateEntry {
    :org-uuid lib-schema/UniqueID
    :board-uuid lib-schema/UniqueID
    :body-placeholder lib-schema/NonBlankStr}))

(def AccessLevel (schema/pred #(#{:private :team :public} (keyword %))))

(def Board {
  :uuid lib-schema/UniqueID
  :slug Slug
  :name lib-schema/NonBlankStr
  :org-uuid lib-schema/UniqueID
  :access AccessLevel
  :authors [lib-schema/UniqueID]
  :viewers [lib-schema/UniqueID]
  :topics TopicOrder
  :author lib-schema/Author
  :created-at lib-schema/ISO8601
  :updated-at lib-schema/ISO8601})

(def Org {
  :uuid lib-schema/UniqueID
  :slug Slug
  :name lib-schema/NonBlankStr
  :team-id lib-schema/UniqueID
  :currency schema/Str
  (schema/optional-key :logo-url) (schema/maybe schema/Str)
  (schema/optional-key :logo-width) schema/Int
  (schema/optional-key :logo-height) schema/Int
  :promoted schema/Bool
  :authors [lib-schema/UniqueID]
  :author lib-schema/Author
  :created-at lib-schema/ISO8601
  :updated-at lib-schema/ISO8601})

(def ShareMedium (schema/pred #(#{:legacy :link :email :slack} (keyword %))))

(def ShareRequest {
  :title (schema/maybe schema/Str)
  :medium ShareMedium
  :entries [{:topic-slug TopicSlug :created-at lib-schema/ISO8601}]
  ;; Email medium
  (schema/optional-key :to) [lib-schema/EmailAddress]
  (schema/optional-key :subject) (schema/maybe schema/Str)
  (schema/optional-key :note) (schema/maybe schema/Str)
  ;; Slack medium
  (schema/optional-key :channel) lib-schema/NonBlankStr
  (schema/optional-key :slack-org-id) lib-schema/NonBlankStr})

(def Update 
  (merge ShareRequest {
    (schema/optional-key :id) lib-schema/UUIDStr
    :slug Slug ; slug of the update, made from the slugified title and a short UUID fragment
    :org-uuid lib-schema/UniqueID
    :org-name lib-schema/NonBlankStr
    :currency schema/Str
    (schema/optional-key :logo-url) (schema/maybe schema/Str)
    (schema/optional-key :logo-width) schema/Int
    (schema/optional-key :logo-height) schema/Int          
    :entries [UpdateEntry]
    :author lib-schema/Author ; user that created the update
    :created-at lib-schema/ISO8601
    :updated-at lib-schema/ISO8601}))

;; ----- Utility functions -----

(defn clean
  "Remove any reserved properties from the resource."
  [resource]
  (apply dissoc resource reserved-properties))