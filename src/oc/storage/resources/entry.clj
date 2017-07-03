(ns oc.storage.resources.entry
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (if-let* when-let*)]
            [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.db.common :as db-common]
            [oc.lib.slugify :as slugify]
            [oc.storage.resources.common :as common]
            [oc.storage.resources.board :as board-res]))

;; ----- RethinkDB metadata -----

(def table-name common/entry-table-name)
(def primary-key :uuid)

;; ----- Metadata -----

(def reserved-properties
  "Properties of a resource that can't be specified during a create and are ignored during an update."
  #{:topic-slug})

;; ----- Utility functions -----

(defn clean
  "Remove any reserved properties from the org."
  [entry]
  (apply dissoc (common/clean entry) reserved-properties))

(defn timestamp-attachments
  "Add a `:created-at` timestamp with the specified value to any attachment that's missing it."
  ([attachments] (timestamp-attachments attachments (db-common/current-timestamp)))
  
  ([attachments timestamp]
  (map #(if (:created-at %) % (assoc % :created-at timestamp)) attachments)))

;; ----- Entry CRUD -----

(schema/defn ^:always-validate ->entry :- common/Entry
  "
  Take an org UUID, a board UUID, a minimal map describing a Entry, and a user (as the author) and
  'fill the blanks' with any missing properties.
  "
  [conn board-uuid :- lib-schema/UniqueID entry-props user :- lib-schema/User]
  {:pre [(db-common/conn? conn)
         (map? entry-props)]}
  (if-let [board (board-res/get-board conn board-uuid)]
    (let [topic-name (:topic-name entry-props)
          topic-slug (slugify/slugify topic-name)
          ts (db-common/current-timestamp)]
      (-> entry-props
        keywordize-keys
        clean
        (assoc :uuid (db-common/unique-id))
        (assoc :topic-slug topic-slug)
        (update :headline #(or % ""))
        (update :body #(or % ""))
        (update :attachments #(timestamp-attachments % ts))
        (assoc :org-uuid (:org-uuid board))
        (assoc :board-uuid board-uuid)
        (assoc :author [(assoc (lib-schema/author-for-user user) :updated-at ts)])
        (assoc :created-at ts)
        (assoc :updated-at ts)))
    false)) ; no board

(schema/defn ^:always-validate create-entry!
  "Create an entry for the board. Throws a runtime exception if the entry doesn't conform to the common/Entry schema.

  Returns the newly created entry, or false if the board specified in the entry can't be found."
  [conn entry :- common/Entry]
  {:pre [(db-common/conn? conn)]}
  (if-let* [board-uuid (:board-uuid entry)
            board (board-res/get-board conn board-uuid)]
    (let [ts (db-common/current-timestamp)
          author (assoc (first (:author entry)) :updated-at ts)] ; update initial author timestamp
      (db-common/create-resource conn table-name (assoc entry :author [author]) ts)) ; create the entry
    ;; No board
    false))

(schema/defn ^:always-validate get-entry
  "
  Given the UUID of the entry, retrieve the entry, or return nil if it doesn't exist.

  Or given the UUID of the org and board, and the UUID of the entry,
  retrieve the entry, or return nil if it doesn't exist.
  "
  ([conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resource conn table-name uuid))

  ([conn org-uuid :- lib-schema/UniqueID board-uuid :- lib-schema/UniqueID uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (first (db-common/read-resources conn table-name :uuid-board-uuid-org-uuid [[uuid board-uuid org-uuid]]))))

(schema/defn ^:always-validate update-entry! :- (schema/maybe common/Entry)
  "
  Given the ID of the entry, an updated entry property map, and a user (as the author), update the entry and
  return the updated entry on success.

  Throws an exception if the merge of the prior entry and the updated entry property map doesn't conform
  to the common/Entry schema.
  "
  [conn uuid :- lib-schema/UniqueID entry user :- lib-schema/User]
  {:pre [(db-common/conn? conn)         
         (map? entry)]}
  (if-let [original-entry (get-entry conn uuid)]
    (let [authors (:author original-entry)
          merged-entry (merge original-entry (clean entry))
          ts (db-common/current-timestamp)
          updated-authors (conj authors (assoc (lib-schema/author-for-user user) :updated-at ts))
          updated-author (assoc merged-entry :author updated-authors)
          attachments (:attachments merged-entry)
          updated-entry (assoc updated-author :attachments (timestamp-attachments attachments ts))]
      (schema/validate common/Entry updated-entry)
      (db-common/update-resource conn table-name primary-key original-entry updated-entry ts))))

(schema/defn ^:always-validate delete-entry!
  "
  Given the entry map, or the UUID of the board, the slug of the topic and the created-at timestamp, delete the entry
  and return `true` on success.
  "
  ([conn entry :- common/Entry] (delete-entry! conn (:board-uuid entry) (:uuid entry)))

  ([conn board-uuid :- lib-schema/UniqueID uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (if-let [board (board-res/get-board conn board-uuid)]
    (do
      ;; Delete interactions
      (db-common/delete-resource conn common/interaction-table-name :entry-uuid uuid)
      (db-common/delete-resource conn table-name uuid))
    ;; No board
    false)))

(schema/defn ^:always-validate get-comments-for-entry
  "Given the UUID of the entry, return a list of the comments for the entry."
  [conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (filter :body (db-common/read-resources conn common/interaction-table-name "entry-uuid" uuid [:uuid :author :body])))

(schema/defn ^:always-validate get-reactions-for-entry
  "Given the UUID of the entry, return a list of the reactions for the entry."
  [conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (filter :reaction (db-common/read-resources conn common/interaction-table-name "entry-uuid" uuid [:uuid :author :reaction])))

;; ----- Collection of entries -----

(schema/defn ^:always-validate get-entries-by-board
  "Given the UUID of the board, return the latest entry (by :created-at) for each topic."
  [conn board-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resources-in-group conn table-name :board-uuid board-uuid :topic-slug :created-at))

(schema/defn ^:always-validate count-entries-by-board
  "Given the UUID of the board, return a count of the entries for each topic."
  [conn board-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resources-in-group conn table-name :board-uuid board-uuid :topic-slug :count))

(schema/defn ^:always-validate get-entries-by-topic
  "Given the UUID of the board, and a topic slug, return all the entries for the topic slug, ordered by `created-at`."
  [conn board-uuid :- lib-schema/UniqueID topic-slug :- common/Slug]
  {:pre [(db-common/conn? conn)]}
  (vec (sort-by :created-at
    (db-common/read-resources conn table-name :topic-slug-board-uuid [[topic-slug board-uuid]]))))

(schema/defn ^:always-validate get-interactions-by-topic
  "
  Given the UUID of the board, and a topic slug, return all the comments for entries of the topic slug,
  grouped by `entry-uuid`.
  "
  [conn org-uuid :- lib-schema/UniqueID board-uuid :- lib-schema/UniqueID topic-slug :- common/Slug]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resources-in-group conn
    common/interaction-table-name
    :topic-slug-board-uuid-org-uuid
    [topic-slug board-uuid org-uuid] "entry-uuid"))

;; ----- Armageddon -----

(defn delete-all-entries!
  "Use with caution! Failure can result in partial deletes. Returns `true` if successful."
  [conn]
  {:pre [(db-common/conn? conn)]}
  ;; Delete all interactions and entries
  (db-common/delete-all-resources! conn common/interaction-table-name)
  (db-common/delete-all-resources! conn table-name))