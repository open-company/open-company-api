(ns oc.storage.resources.board
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (when-let*)]
            [defun.core :refer (defun)]
            [schema.core :as schema]
            [taoensso.timbre :as timbre]
            [clojure.set :as clj-set]
            [oc.lib.user :as user-lib]
            [oc.lib.schema :as lib-schema]
            [oc.lib.slugify :as slug]
            [oc.lib.db.common :as db-common]
            [oc.lib.text :as str]
            [oc.storage.config :as config]
            [oc.storage.resources.common :as common]
            [oc.storage.resources.org :as org-res]
            [oc.storage.async.notification :as notification]))

;; ----- RethinkDB metadata -----

(def table-name common/board-table-name)
(def primary-key :uuid)

(def publisher-board-slug-prefix "publisher-board-")

;; ----- Metadata -----

(def access #{:private :team :public})

(def reserved-properties
  "Properties of a resource that can't be specified during a create and are ignored during an update."
  (clj-set/union common/reserved-properties #{:authors :viewers}))

(def ignored-properties
  "Properties of a resource that are ignored during an update."
  reserved-properties)

;; ----- Data Defaults -----

(def default-access :team)

(def default-drafts-board {
  :uuid "0000-0000-0000"
  :name "Drafts"
  :slug "drafts"
  :viewers []
  :access :private})

;; ----- Utility functions -----

(defn clean
  "Remove any reserved properties from the board."
  [board]
  (apply dissoc board reserved-properties))

(defn ignore-props
  "Remove any ignored properties from the board."
  [board]
  (apply dissoc board ignored-properties))

;; We don't validate that the board returned from this function because there's a NUX
;; case where the user retrieving the org hasn't yet provided their name and so are not a valid
;; user per the schema. For the same reason we don't validate the user argument here.
(schema/defn ^:always-validate drafts-board
  "Return a draft board for the specified org and author."
  [org-uuid :- lib-schema/UniqueID user]
  (let [now (db-common/current-timestamp)]
    (merge default-drafts-board {
      :org-uuid org-uuid
      :author (lib-schema/author-for-user user)
      :authors [(:user-id user)]
      :created-at now
      :updated-at now})))

;; ----- Board Slug -----

(def reserved-slugs #{"create-board" "settings" "boards" "all-posts" "drafts" "must-see" "bookmarks" "inbox" "new" "following" "unfollowing" "home" "topics" "activity"})

(declare list-boards-by-org)
(defn taken-slugs
  "Return all board slugs which are in use as a set."
  [conn org-uuid]
  {:pre [(db-common/conn? conn)
         (schema/validate lib-schema/UniqueID org-uuid)]}
  (into reserved-slugs (map :slug (list-boards-by-org conn org-uuid))))

(defn slug-available?
  "Return true if the slug is not used by any board in the org."
  [conn org-uuid slug]
  {:pre [(db-common/conn? conn)
         (schema/validate lib-schema/UniqueID org-uuid)
         (slug/valid-slug? slug)]}
  (not (contains? (taken-slugs conn org-uuid) slug)))

;; ----- Org CRUD -----

(schema/defn ^:always-validate ->board :- common/NewBoard
  "
  Take an org UUID, a minimal map describing a board, a user and an optional slug and 'fill the blanks' with
  any missing properties.
  "
  ([org-uuid board-props user]
  {:pre [(schema/validate lib-schema/NonBlankStr (:name board-props))]}
  (->board org-uuid (or (:slug board-props) (slug/slugify (:name board-props))) board-props user))

  ([org-uuid slug board-props user :- lib-schema/User]
  {:pre [(schema/validate lib-schema/UniqueID org-uuid)
         (slug/valid-slug? slug)
         (map? board-props)]}
  (let [ts (db-common/current-timestamp)]
    (-> board-props
        keywordize-keys
        clean
        (assoc :uuid (db-common/unique-id))
        (assoc :slug slug)
        (update :name #(str/strip-xss-tags %))
        (assoc :org-uuid org-uuid)
        (update :access #(or % default-access))
        (update :entries #(or % []))
        (assoc :authors [(:user-id user)])
        (assoc :viewers [])
        (assoc :author (lib-schema/author-for-user user))
        (assoc :created-at ts)
        (assoc :updated-at ts)))))

(schema/defn ^:always-validate create-board!
  "
  Create a board in the system. Throws a runtime exception if the board doesn't conform to the common/Board schema.

  Check the slug in the response as it may change if there is a conflict with another board for the org.
  "
  [conn board :- common/NewBoard]
  {:pre [(db-common/conn? conn)]}
  (db-common/create-resource conn table-name
    (update (dissoc board :entries) :slug #(slug/find-available-slug % (taken-slugs conn (:org-uuid board))))
    (db-common/current-timestamp)))

(defn- publisher-board-slug [taken-slugs user-id]
  (slug/find-available-slug (str publisher-board-slug-prefix user-id) taken-slugs))

(schema/defn ^:always-validate create-publisher-board!
  [conn org-uuid :- lib-schema/UniqueID user :- lib-schema/User]
  {:pre [(db-common/conn? conn)]}
  (let [taken-slugs (taken-slugs conn org-uuid)
        board-map {:slug (publisher-board-slug taken-slugs (:user-id user))
                   :publisher-board true
                   :access :team
                   :name (user-lib/name-for user)}
        new-board (->board org-uuid board-map user)]
   (create-board! conn new-board)))

(schema/defn ^:always-validate get-board :- (schema/maybe common/Board)
  "
  Given the uuid of the board, or the uuid of the org and slug of the board, retrieve the board,
  or return nil if it doesn't exist.
  "
  ([conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resource conn table-name uuid))

  ([conn org-uuid :- lib-schema/UniqueID slug-or-uuid]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug-or-uuid)]}
  (or (first (db-common/read-resources conn table-name "slug-org-uuid" [[slug-or-uuid org-uuid]]))
      (db-common/read-resource conn table-name slug-or-uuid))))

(schema/defn ^:always-validate uuid-for :- (schema/maybe lib-schema/UniqueID)
  "Given an org slug, and a board slug, return the UUID of the board, or nil if it doesn't exist."
  [conn org-slug slug]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? org-slug)
         (slug/valid-slug? slug)]}
  (when-let [org-uuid (org-res/uuid-for conn org-slug)]
    (:uuid (get-board conn org-uuid slug))))

(schema/defn ^:always-validate update-board! :- (schema/maybe common/Board)
  "
  Given the board UUID and an updated board property map, update the board and return the updated board on success.

  Throws an exception if the merge of the prior board and the updated board property map doesn't conform
  to the common/Board schema.
  
  NOTE: doesn't update authors, see: `add-author`, `remove-author`
  NOTE: doesn't update viewers, see: `add-viewer`, `remove-viewer`
  NOTE: doesn't handle case of slug change.
  "
  [conn uuid :- lib-schema/UniqueID board]
  {:pre [(db-common/conn? conn)
         (map? board)]}
  (when-let [original-board (get-board conn uuid)]
    (let [updated-board (merge original-board (ignore-props board))]
      (schema/validate common/Board updated-board)
      (db-common/update-resource conn table-name primary-key original-board updated-board))))

(defun delete-board!
  "
  Given the uuid of the org, and slug of the board, delete the board and all its entries, and updates,
  and return `true` on success.
  "
  ([conn :guard db-common/conn? board :guard map? entries :guard sequential?]
  ;; Delete interactions
  (db-common/delete-resource conn common/interaction-table-name :board-uuid (:uuid board))
  (let [published-entries (filterv #(= (keyword (:status %)) :published) entries)]
    ;; Delete all published entries
    (doseq [entry published-entries]
      (timbre/info "Delete entry:" (:uuid entry))
      (db-common/delete-resource conn common/entry-table-name :uuid (:uuid entry)))
    (if (= (count entries) (count published-entries))
      ;; Delete the board itself
      (do
        (timbre/info "Actually deleting board" (:uuid board))
        (db-common/delete-resource conn table-name (:uuid board)))
      ;; Set back the draft on the board
      (do
        (timbre/info "Updating to draft board" (:uuid board))
        (update-board! conn (:uuid board) (assoc board :draft true))))))

  ([conn :guard db-common/conn? org-uuid :guard #(schema/validate lib-schema/UniqueID %) slug :guard slug/valid-slug? entries :guard sequential?]
  (when-let [board (get-board conn org-uuid slug)]
    (delete-board! conn board entries)))

  ([conn :guard db-common/conn? uuid :guard #(schema/validate lib-schema/UniqueID %) entries :guard sequential?]
  (when-let [board (get-board conn uuid)]
    (delete-board! conn board entries))))

;; ----- Board's set operations -----

(schema/defn ^:always-validate add-author :- (schema/maybe common/Board)
  "
  Given the unique ID of the org, the slug of the board, and the user-id of the user, add the user as an author of
  the board if it exists.
  
  Returns the updated board on success, nil on non-existence, and a RethinkDB error map on other errors.
  "
  [conn org-uuid :- lib-schema/UniqueID slug user-id :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (when-let* [board (get-board conn org-uuid slug)]
    (db-common/remove-from-set conn table-name (:uuid board) "viewers" user-id)
    (db-common/add-to-set conn table-name (:uuid board) "authors" user-id)))

(schema/defn ^:always-validate remove-author :- (schema/maybe common/Board)
  "
  Given the unique ID of the org, the slug of the board, and the user-id of the user, remove the user as an author of
  the board if it exists.
  
  Returns the updated board on success, nil on non-existence, and a RethinkDB error map on other errors.
  "
  [conn org-uuid :- lib-schema/UniqueID slug user-id :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (when-let* [board (get-board conn org-uuid slug)]
    (db-common/remove-from-set conn table-name (:uuid board) "authors" user-id)))

(schema/defn ^:always-validate add-viewer :- (schema/maybe common/Board)
  "
  Given the unique ID of the org, the slug of the board, and the user-id of the user, add the user as a viewer of
  the board if it exists.
  
  Returns the updated board on success, nil on non-existence, and a RethinkDB error map on other errors.
  "
  [conn org-uuid :- lib-schema/UniqueID slug user-id :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (when-let* [board (get-board conn org-uuid slug)]
    (db-common/remove-from-set conn table-name (:uuid board) "authors" user-id)
    (db-common/add-to-set conn table-name (:uuid board) "viewers" user-id)))

(schema/defn ^:always-validate remove-viewer :- (schema/maybe common/Board)
  "
  Given the unique ID of the org, the slug of the board, and the user-id of the user, remove the user as a viewer of
  the board if it exists.
  
  Returns the updated board on success, nil on non-existence, and a RethinkDB error map on other errors.
  "
  [conn org-uuid :- lib-schema/UniqueID slug user-id :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (when-let* [board (get-board conn org-uuid slug)]
    (db-common/remove-from-set conn table-name (:uuid board) "viewers" user-id)))

;; ----- Draft board delete on emptyness ------

(defn maybe-delete-draft-board
  "Check if a board is actually a draft board and if it has no more entries remove it."
  [conn org board remaining-entries user]
  (when (and ;; if it's a draft board
             (:draft board)
             ;; and has no more entries
             (zero? (count remaining-entries)))
    (timbre/info "Deleting board:" (:uuid board) "because last draft was removed.")
    ;; Remove also the board
    (delete-board! conn (:uuid board) remaining-entries)
    (timbre/info "Deleted board:" (:uuid board))
    (notification/send-trigger! (notification/->trigger :delete org {:old board} user))));)

;; ----- Collection of boards -----

(defn list-boards
  "
  Return a sequence of boards with slugs, UUIDs, names, type, and org-uuid, sorted by slug.
  
  Note: if additional-keys are supplied, they will be included in the map, and only boards
  containing those keys will be returned.
  "
  ([conn] (list-boards conn []))

  ([conn additional-keys]
  {:pre [(db-common/conn? conn)
         (sequential? additional-keys)
         (every? #(or (string? %) (keyword? %)) additional-keys)]}
  (->> (into [primary-key :slug :name :org-uuid] additional-keys)
    (db-common/read-resources conn table-name)
    (sort-by :slug)
    vec)))

(defn list-boards-by-org
  "
  Return a sequence of boards with slugs, UUIDs, type and names, sorted by slug.
  
  Note: if additional-keys are supplied, they will be included in the map, and only boards
  containing those keys will be returned.
  "
  ([conn org-uuid] (list-boards-by-org conn org-uuid []))

  ([conn org-uuid additional-keys]
  {:pre [(db-common/conn? conn)
         (schema/validate lib-schema/UniqueID org-uuid)
         (sequential? additional-keys)
         (every? #(or (string? %) (keyword? %)) additional-keys)]}
  (let [read-resources (if config/publisher-board-enabled?
                         (partial db-common/read-resources conn table-name :org-uuid org-uuid)
                         (partial db-common/filter-resources conn table-name :org-uuid org-uuid [{:fn :ne :value true :field :publisher-board}]))]
    (->> (into [primary-key :slug :name :draft] additional-keys)
      read-resources
      (sort-by :slug)
      vec))))

(defn list-boards-by-index
  "
  Given the name of a secondary index and a value, retrieve all matching boards
  as a sequence of maps with slugs, UUIDs and names, sorted by slug.
  
  Secondary indexes:
  :uuid
  :slug
  :authors
  :viewers

  Note: if additional-keys are supplied, they will be included in the map, and only boards
  containing those keys will be returned.
  "
  ([conn index-key index-value] (list-boards-by-index conn index-key index-value []))

  ([conn index-key index-value additional-keys]
  {:pre [(db-common/conn? conn)
         (or (keyword? index-key) (string? index-key))
         (sequential? additional-keys)
         (every? #(or (string? %) (keyword? %)) additional-keys)]}

  (->> (into [primary-key :slug :name] additional-keys)
    (db-common/read-resources conn table-name index-key index-value)
    (sort-by :slug)
    vec)))

;; ----- Armageddon -----

(defn delete-all-boards!
  "Use with caution! Failure can result in partial deletes. Returns `true` if successful."
  [conn]
  {:pre [(db-common/conn? conn)]}
  ;; Delete all interactions, entries, and boards
  (db-common/delete-all-resources! conn common/interaction-table-name)
  (db-common/delete-all-resources! conn common/entry-table-name)
  (db-common/delete-all-resources! conn table-name))