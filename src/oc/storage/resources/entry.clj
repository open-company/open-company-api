(ns oc.storage.resources.entry
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (if-let* when-let*)]
            [schema.core :as schema]
            [taoensso.timbre :as timbre]
            [oc.lib.schema :as lib-schema]
            [oc.lib.db.common :as db-common]
            [oc.lib.text :as oc-str]
            [rethinkdb.query :as r]
            [oc.storage.resources.common :as common]
            [oc.storage.resources.board :as board-res]
            [oc.storage.config :as config]))

(def temp-uuid "9999-9999-9999")

;; ----- RethinkDB metadata -----

(def table-name common/entry-table-name)
(def versions-table-name (str "versions_" common/entry-table-name))
(def versions-primary-key :version-uuid)
(def primary-key :uuid)

;; ----- Metadata -----

(def reserved-properties
  "Properties of a resource that can't be specified during a create and are ignored during an update."
  (clojure.set/union common/reserved-properties #{:board-slug :published-at :publisher :secure-uuid :user-visibility}))

(def ignored-properties
  "Properties of a resource that are ignored during an update."
  (disj reserved-properties :board-uuid :status :user-visibility))

(def list-properties
  "Set of properties we want when listing entries."
  ["uuid" "headline" "body" "reaction" "author" "created-at" "updated-at"])

(def list-comment-properties
  "Set of peroperties we want when retrieving comments"
  ["uuid" "body" "reaction" "author" "parent-uuid" "created-at" "updated-at"])

;; ----- Utility functions -----

(defn clean
  "Remove any reserved properties from the entry."
  [entry]
  (apply dissoc entry reserved-properties))

(defn ignore-props
  "Remove any ignored properties from the entry."
  [entry]
  (apply dissoc entry ignored-properties))

(defn- publish-props
  "Provide properties for an initially published entry."
  [entry timestamp author]
  (if (= (keyword (:status entry)) :published)
    (-> entry
      (assoc :published-at timestamp)
      (assoc :publisher author)
      (assoc-in [:user-visibility (keyword (:user-id author))] {:follow true :dismiss-at timestamp}))
    entry))

(defn timestamp-attachments
  "Add a `:created-at` timestamp with the specified value to any attachment that's missing it."
  ([attachments] (timestamp-attachments attachments (db-common/current-timestamp)))
 
  ([attachments timestamp]
  (map #(if (:created-at %) % (assoc % :created-at timestamp)) attachments)))

;; ----- Entry CRUD -----

(schema/defn ^:always-validate ->entry :- common/Entry
  "
  Take a board UUID, a minimal map describing an Entry, and a user (as the author) and
  'fill the blanks' with any missing properties.

  Throws an exception if the board specified in the entry can't be found.
  "
  [conn board-uuid :- lib-schema/UniqueID entry-props user :- lib-schema/User]
  {:pre [(db-common/conn? conn)
         (map? entry-props)]}
  (if-let [board (if (= board-uuid temp-uuid) 
                    {:org-uuid temp-uuid} ; board doesn't exist yet, we're checking if this entry will be valid
                    (board-res/get-board conn board-uuid))]
    (let [ts (db-common/current-timestamp)
          author (lib-schema/author-for-user user)]
      (-> entry-props
          keywordize-keys
          clean
          (assoc :uuid (db-common/unique-id))
          (assoc :secure-uuid (db-common/unique-id))
          (update :status #(or % "draft"))
          (update :headline #(or (oc-str/strip-xss-tags %) ""))
          (update :body #(or (oc-str/strip-xss-tags %) ""))
          (update :abstract #(or (oc-str/strip-xss-tags %) ""))
          (update :attachments #(timestamp-attachments % ts))
          (assoc :org-uuid (:org-uuid board))
          (assoc :board-uuid board-uuid)
          (assoc :author [(assoc author :updated-at ts)])
          (assoc :revision-id 0)
          (assoc :created-at ts)
          (assoc :updated-at ts)
          (publish-props ts author)))
    (throw (ex-info "Invalid board uuid." {:board-uuid board-uuid})))) ; no board

(declare update-entry)

(defn- create-version [conn updated-entry original-entry]
  (let [ts (db-common/current-timestamp)
        revision-id (:revision-id original-entry)
        revision-id-new (inc revision-id)
        revision (-> original-entry
                     (assoc :version-uuid (str (:uuid original-entry)
                                               "-v" revision-id))
                     (assoc :revision-date ts)
                     (assoc :revision-author (first (:author original-entry))))]
    (let [updated-entry (if-not (:deleted original-entry)
                          (update-entry conn
                            (assoc updated-entry :revision-id revision-id-new)
                            updated-entry
                            ts)
                          updated-entry)]
      (db-common/create-resource conn versions-table-name revision ts)
      updated-entry)))

(defn- remove-version [conn entry version]
  (let [version-uuid (str (:uuid entry) "-v" version)]
    (try
      (when (db-common/read-resource conn versions-table-name version-uuid)
        (db-common/delete-resource conn versions-table-name version-uuid))
      (catch Exception e (timbre/error e)))))

(defn delete-versions [conn entry-data]
  (let [entry (if (:delete-entry entry-data)
                ;; increment one to remove all versions when deleting a draft
                (update-in entry-data [:revision-id] inc)
                entry-data)]
    (if (and (= 1 (:revision-id entry))
             (:delete-entry entry))
      (remove-version conn entry 1) ;; single entry with deleted draft
      (when (pos? (:revision-id entry))
        (doseq [version (range (:revision-id entry))]
          (remove-version conn entry version))))))

(declare get-entry)

(defn- delete-version [conn uuid]
  (let [entry (get-entry conn uuid)
        revision-id (if (zero? (:revision-id entry))
                      (inc (:revision-id entry))
                      (:revision-id entry))]
    (create-version conn entry (-> entry
                                   (assoc :revision-id revision-id)
                                   (assoc :deleted true)))))

;; Sample content handling

(defn sample-entries-count [conn org-uuid]
  (count (db-common/read-resources conn table-name "org-uuid-sample" [[org-uuid true]])))

(defn get-sample-entries [conn org-uuid]
  (db-common/read-resources conn table-name "org-uuid-sample" [[org-uuid true]]))

(schema/defn ^:always-validate create-entry! :- (schema/maybe common/Entry)
  "
  Create an entry for the board. Returns the newly created entry.

  Throws a runtime exception if the provided entry doesn't conform to the
  common/Entry schema. Throws an exception if the board specified in the entry can't be found.
  "
  ([conn entry :- common/Entry] (create-entry! conn entry (db-common/current-timestamp)))

  ([conn entry :- common/Entry ts :- lib-schema/ISO8601]
  {:pre [(db-common/conn? conn)]}
  (if-let* [board-uuid (:board-uuid entry)
            board (board-res/get-board conn board-uuid)]
    (let [stamped-entry (if (= (keyword (:status entry)) :published)
                              (assoc entry :published-at ts)
                              entry)
          author (assoc (first (:author entry)) :updated-at ts)] ; update initial author timestamp
      ;; create the entry
      (db-common/create-resource conn table-name (assoc stamped-entry :author [author]) ts))
    (throw (ex-info "Invalid board uuid." {:board-uuid (:board-uuid entry)}))))) ; no board

(schema/defn ^:always-validate get-entry :- (schema/maybe common/Entry)
  "
  Given the UUID of the entry, retrieve the entry, or return nil if it doesn't exist.

  Or given the UUID of the org, board, entry, retrieve the entry, or return nil if it doesn't exist. This variant 
  is used to confirm that the entry belongs to the specified org and board.
  "
  ([conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resource conn table-name uuid))

  ([conn org-uuid :- lib-schema/UniqueID board-uuid :- lib-schema/UniqueID uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (first (db-common/read-resources conn table-name :uuid-board-uuid-org-uuid [[uuid board-uuid org-uuid]]))))

(schema/defn ^:always-validate get-entry-by-secure-uuid :- (schema/maybe common/Entry)
  "
  Given the secure UUID of the entry, retrieve the entry, or return nil if it doesn't exist.

  Or given the UUID of the org, and entry, retrieve the entry, or return nil if it doesn't exist. This variant 
  is used to confirm that the entry belongs to the specified org.
  "
  ([conn secure-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (first (db-common/read-resources conn table-name :secure-uuid secure-uuid)))

  ([conn org-uuid :- lib-schema/UniqueID secure-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (first (db-common/read-resources conn table-name :secure-uuid-org-uuid [[secure-uuid org-uuid]]))))

(defn get-version
  "
  Given the UUID of the entry and revision number, retrieve the entry, or return nil if it doesn't exist.
  "
  [conn uuid revision-id]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resource conn
                           versions-table-name
                           (str uuid "-v" revision-id)))

(defn- update-entry [conn entry original-entry ts]
  (let [merged-entry (merge original-entry (ignore-props entry))
        attachments (:attachments merged-entry)
        authors-entry (assoc merged-entry :author (:author entry))
        updated-entry (assoc authors-entry :attachments (timestamp-attachments attachments ts))]
    (schema/validate common/Entry updated-entry)
    (db-common/update-resource conn table-name primary-key original-entry updated-entry ts)))

(defn- add-author-to-entry
  [original-entry entry user]
  (let [authors (:author original-entry)
        ts (db-common/current-timestamp)
        updated-authors (concat authors [(assoc (lib-schema/author-for-user user) :updated-at ts)])]
    (assoc entry :author updated-authors)))

(schema/defn ^:always-validate update-entry-no-version! :- (schema/maybe common/Entry)
  [conn uuid :- lib-schema/UniqueID entry user :- lib-schema/User]
  {:pre [(db-common/conn? conn)
         (map? entry)]}
  (if-let [original-entry (get-entry conn uuid)]
   (let [updated-entry (add-author-to-entry original-entry entry user)]
     (update-entry conn updated-entry original-entry (db-common/current-timestamp)))))

(schema/defn ^:always-validate update-entry-no-user! :- (schema/maybe common/Entry)
  "
  Given the UUID of the entry, an updated entry property map, update the entry
  and return the updated entry on success.

  Throws an exception if the merge of the prior entry and the updated entry property map doesn't conform
  to the common/Entry schema.
  "
  [conn uuid :- lib-schema/UniqueID entry]
  {:pre [(db-common/conn? conn)
         (map? entry)]}
  (if-let [original-entry (get-entry conn uuid)]
    (update-entry conn entry original-entry (db-common/current-timestamp))))

(schema/defn ^:always-validate update-entry! :- (schema/maybe common/Entry)
  "
  Given the UUID of the entry, an updated entry property map, and a user (as the author), update the entry and
  return the updated entry on success.

  Throws an exception if the merge of the prior entry and the updated entry property map doesn't conform
  to the common/Entry schema.
  "
  [conn uuid :- lib-schema/UniqueID entry user :- lib-schema/User]
  {:pre [(db-common/conn? conn)
         (map? entry)]}
  (when-let [original-entry (get-entry conn uuid)]
    (let [ts (db-common/current-timestamp)
          authors-entry (add-author-to-entry original-entry entry user)
          updated-entry (update-entry conn authors-entry original-entry ts)]
        ;; copy current version to versions table, increment revision uuid
        (create-version conn updated-entry original-entry))))

(defn upsert-entry!
  "
  If entry is found update otherwise create the new entry.
  "
  [conn entry user]
  (if-let [original-entry (get-entry conn (:uuid entry))]
    (update-entry! conn (:uuid entry) entry user)
    (create-entry! conn entry)))

(schema/defn ^:always-validate publish-entry! :- (schema/maybe common/Entry)
  "
  Given the UUID of the entry, an optional updated entry map, and a user (as the publishing author),
  publish the entry and return the updated entry on success.
  "
  ([conn uuid :- lib-schema/UniqueID user :- lib-schema/User] (publish-entry! conn uuid {} user))

  ([conn uuid :- lib-schema/UniqueID entry-props user :- lib-schema/User]
  {:pre [(db-common/conn? conn)
         (map? entry-props)]}
  (if-let [original-entry (get-entry conn uuid)]
    (let [authors (:author original-entry)
          ts (db-common/current-timestamp)
          publisher (lib-schema/author-for-user user)
          old-user-visibility (:user-visibility original-entry)
          merged-entry (merge original-entry entry-props {:status :published
                                                          :published-at ts
                                                          :publisher publisher
                                                          :secure-uuid (db-common/unique-id)
                                                          :user-visibility (assoc old-user-visibility
                                                                            (keyword (:user-id user))
                                                                            {:follow true
                                                                             :dismiss-at ts})})
          updated-authors (conj authors (assoc publisher :updated-at ts))
          entry-update (assoc merged-entry :author updated-authors)]
      (schema/validate common/Entry entry-update)
      (let [updated-entry (db-common/update-resource conn table-name primary-key original-entry entry-update ts)
            ;; copy current version to versions table, increment revision uuid
            versioned-entry (create-version conn updated-entry entry-update)]
        ;; Delete the draft entry's interactions
        (db-common/delete-resource conn common/interaction-table-name :resource-uuid uuid)
        versioned-entry)))))

(schema/defn ^:always-validate delete-entry!
  "Given the UUID of the entry, delete the entry and all its interactions. Return `true` on success."
  [conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/delete-resource conn common/interaction-table-name :resource-uuid uuid)
  ;; update versions table as deleted (logical delete)
  (delete-version conn uuid)
  (db-common/delete-resource conn table-name uuid))

(schema/defn ^:always-validate revert-entry!
  "Given the UUID of the entry and revision, replace current revision with specified. Return `true` on success."
  [conn entry entry-version user]
  {:pre [(db-common/conn? conn)]}
  (let [new-entry (dissoc entry-version
                          :revision-id
                          :version-uuid
                          :revision-date
                          :revision-author)]
    ;; if version number is 0 delete the actual entry
    (if (= -1 (:revision-id entry-version))
      (do
        (delete-entry! conn (:uuid entry-version))
        {:uuid (:uuid entry-version) :deleted true})
      (update-entry! conn (:uuid new-entry) new-entry user))))

(schema/defn ^:always-validate list-comments-for-entry
  "Given the UUID of the entry, return a list of the comments for the entry."
  [conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (filter :body (db-common/read-resources conn common/interaction-table-name "resource-uuid" uuid list-comment-properties)))

(schema/defn ^:always-validate list-reactions-for-entry
  "Given the UUID of the entry, return a list of the reactions for the entry."
  [conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (filter :reaction (db-common/read-resources conn common/interaction-table-name "resource-uuid" uuid [:uuid :author :reaction :created-at])))

;; ----- Collection of entries -----

(schema/defn ^:always-validate list-entries-by-org
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a direction, one of `:before` or `:after`, return the published entries for the org with any interactions.
  "
  ([conn org-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resources conn table-name :org-uuid org-uuid))

  ([conn org-uuid :- lib-schema/UniqueID order start :- lib-schema/ISO8601 direction allowed-boards :- [lib-schema/UniqueID] {:keys [must-see count] :or {must-see false count false}}]
  {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)]}
  (let [filter-map (if-not must-see
                     [{:fn :contains :value allowed-boards :field :board-uuid}]
                     [{:fn :contains :value allowed-boards :field :board-uuid}
                      {:fn :eq :field :must-see :value (boolean (#{true "true"} must-see))}]
                     )]
    (db-common/read-all-resources-and-relations conn table-name
      :status-org-uuid [[:published org-uuid]]
      "published-at" order start direction
      filter-map
      :interactions common/interaction-table-name :uuid :resource-uuid
      list-comment-properties {:count count}))))


(schema/defn ^:always-validate paginated-entries-by-board
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a direction, one of `:before` or `:after`, return the published entries for the org with any interactions.
  "
  [conn board-uuid :- lib-schema/UniqueID order start :- lib-schema/ISO8601 direction {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)]}
  (db-common/read-all-resources-and-relations conn table-name
    :status-board-uuid [[:published board-uuid]]
    "published-at" order start direction
    :interactions common/interaction-table-name :uuid :resource-uuid
    list-comment-properties {:count count}))

(schema/defn ^:always-validate list-entries-by-org-author
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a direction, one of `:before` or `:after`, and an optional status of `:draft` or `:published` (the default)
  return the entries by the author with any interactions.
  "
  ([conn org-uuid :- lib-schema/UniqueID user-id {:keys [count] :or {count false}}]
    (list-entries-by-org-author conn org-uuid user-id :published {:count count}))

  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID status {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)
         (#{:published :draft} status)]}
  (db-common/read-resources-and-relations conn table-name :status-org-uuid-author-id [[status org-uuid user-id]]
                                          :interactions common/interaction-table-name :uuid :resource-uuid
                                          list-comment-properties {:count count})))

(schema/defn ^:always-validate list-entries-by-board
  "Given the UUID of the board, return the published entries for the board with any interactions."
  ([conn board-uuid :- lib-schema/UniqueID] (list-entries-by-board conn board-uuid {:count false}))
  
  ([conn board-uuid :- lib-schema/UniqueID {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resources-and-relations conn table-name :status-board-uuid [[:published board-uuid]]
                                          :interactions common/interaction-table-name :uuid :resource-uuid
                                          list-comment-properties {:count count})))

(schema/defn ^:always-validate list-all-entries-by-board
  "Given the UUID of the board, return all the entries for the board."
  [conn board-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/read-resources conn table-name :board-uuid [board-uuid] ["uuid" "status"]))

(schema/defn ^:always-validate list-all-entries-by-follow-ups
  "Given the UUID of the user, return all the published entries with incomplete follow-ups for the user."
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID order start :- lib-schema/ISO8601 direction]
    (list-all-entries-by-follow-ups conn org-uuid user-id order start direction {:count false}))
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID order start :- lib-schema/ISO8601 direction {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)
         (#{:desc :asc} order)
         (#{:before :after} direction)]}
  (db-common/read-all-resources-and-relations conn table-name
      :org-uuid-status-follow-ups-completed?-assignee-user-id-map-multi [[org-uuid :published false user-id]]
      "published-at" order start direction
      :interactions common/interaction-table-name :uuid :resource-uuid
      list-comment-properties {:count count})))

(schema/defn ^:always-validate list-all-entries-for-inbox
  "Given the UUID of the user, return all the entries publoshed at most 30 days before the minimum allowed date.
   Filter by user-visibility on the remaining.
   FIXME: move the filter in the query to avoid loading all entries to filter and then apply the count."
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID order start :- lib-schema/ISO8601 direction]
    (list-all-entries-for-inbox conn org-uuid user-id order start direction {:count false}))
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID order start :- lib-schema/ISO8601 direction {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)
         (#{:desc :asc} order)
         (#{:before :after} direction)]}
  (let [filter-map [{:fn :ge :value config/inbox-minimum-date :field :published-at}]
        all-entries (db-common/read-all-resources-and-relations conn table-name
                     :status-org-uuid [[:published org-uuid]]
                     "published-at" order start direction
                     filter-map
                     :interactions common/interaction-table-name :uuid :resource-uuid
                     list-comment-properties {})
        filtered-entries (remove nil?
                          (filterv
                           (fn [entry]
                            (let [sorted-comments (sort-by :created-at
                                                   (filterv #(and (contains? % :body)
                                                                  (not= (-> % :author :user-id) user-id))
                                                    (:interactions entry)))
                                  last-activity-timestamp (when (seq sorted-comments)
                                                            (:created-at (last sorted-comments)))
                                  user-visibility (some (fn [[k v]] (when (= k (keyword user-id)) v)) (:user-visibility entry))]
                              (or ;; User has never dismissed/followed/unfollowed so he needs to see it
                                  (empty? user-visibility)
                                  ;; User is following the post: sees it only if he has never dismissed or has
                                  ;; dismissed before the last comment created-at
                                  (and last-activity-timestamp
                                       (:follow user-visibility)
                                       (pos? (compare last-activity-timestamp (:dismiss-at user-visibility))))
                                  ;; There are no comments on post, user has dismissed but before published-at
                                  (and (not last-activity-timestamp)
                                       (pos? (compare (:published-at entry) (:dismiss-at user-visibility)))))))
                           all-entries))]
    (if count
      (clojure.core/count filtered-entries)
      filtered-entries))))

;; ----- Entry follow-up manipulation -----

(schema/defn ^:always-validate add-follow-ups! :- (schema/maybe common/Entry)
  "Add a follow-up for the give entry uuid"
  ([conn original-entry :- common/Entry follow-ups :- [common/FollowUp] user :- lib-schema/User]
   {:pre [(db-common/conn? conn)]}
   (let [old-follow-ups (:follow-ups original-entry)
         ;; List the user-ids of the assignees that can't be replaced
         cant-replace-follow-ups (remove nil? (map #(when ;; Cant' replace the follow-ups that
                                                          (and ;; are not assigned to current user
                                                               (not= (-> % :assignee :user-id) (:user-id user))
                                                               ;; and
                                                               (or ;; or is completed
                                                                   (:completed? %)
                                                                   ;; or was created by the user himself
                                                                   (= (-> % :author :user-id) (-> % :assignee :user-id))))
                                                      (-> % :assignee :user-id))
                                  old-follow-ups))
         ;; filter out the new follow-ups that can't be overridden
         filtered-new-follow-ups (filterv #(not ((set cant-replace-follow-ups) (-> % :assignee :user-id))) follow-ups)
         ;; Remove the old follow-ups that are going to be overridden
         keep-old-follow-ups (filterv #((set cant-replace-follow-ups) (-> % :assignee :user-id)) old-follow-ups)
         ;; New follow-ups
         new-follow-ups (vec (concat keep-old-follow-ups filtered-new-follow-ups))
         final-entry (assoc original-entry :follow-ups new-follow-ups)]
    (update-entry-no-version! conn (:uuid original-entry) final-entry user))))

(schema/defn ^:always-validate complete-follow-up!
  "Complete a follow-up item"
  [conn original-entry :- common/Entry follow-up :- common/FollowUp user :- lib-schema/User]
  {:pre [(db-common/conn? conn)]}
  (let [completed-follow-up (merge follow-up {:completed? true
                                              :completed-at (db-common/current-timestamp)})
        other-follow-ups (filterv #(not= (:uuid %) (:uuid follow-up)) (:follow-ups original-entry))
        final-follow-ups (vec (conj other-follow-ups completed-follow-up))
        updated-entry (assoc original-entry :follow-ups final-follow-ups)]
    (update-entry-no-version! conn (:uuid original-entry) updated-entry user)))

;; ----- Data about entries -----

(schema/defn ^:always-validate entry-months-by-org
  "
  Given the UUID of the org, return an ordered sequence of all the months that have at least one entry.

  Response:

  [['2017' '06'] ['2017' '01'] [2016 '05']]

  Sequence is ordered, newest to oldest.
  "
  [conn org-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (db-common/months-with-resource conn table-name :org-uuid org-uuid :published-at))

;; ----- Armageddon -----

(defn delete-all-entries!
  "Use with caution! Failure can result in partial deletes. Returns `true` if successful."
  [conn]
  {:pre [(db-common/conn? conn)]}
  ;; Delete all interactions and entries
  (db-common/delete-all-resources! conn common/interaction-table-name)
  (db-common/delete-all-resources! conn versions-table-name)
  (db-common/delete-all-resources! conn table-name))