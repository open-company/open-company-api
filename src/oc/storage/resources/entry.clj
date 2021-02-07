(ns oc.storage.resources.entry
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (if-let*)]
            [clojure.set :as clj-set]
            [schema.core :as schema]
            [taoensso.timbre :as timbre]
            [oc.lib.schema :as lib-schema]
            [oc.lib.db.common :as db-common]
            [oc.storage.db.common :as storage-db-common]
            [oc.lib.text :as oc-str]
            [oc.storage.resources.common :as common]
            [oc.storage.resources.board :as board-res]))

(defn- log-query-time
  "Log the current time, the timestamp is in nanoseconds, 1/1000000000 to get seconds."
  [step]
  (timbre/debugf "StorageQueryTiming, %s, %d" step (. System (nanoTime))))

(def temp-uuid "9999-9999-9999")

;; ----- RethinkDB metadata -----

(def table-name common/entry-table-name)
(def versions-table-name (str "versions_" common/entry-table-name))
(def versions-primary-key :version-uuid)
(def primary-key :uuid)

;; ----- Helpers -----

; (defn- long? [n]
;   (try
;     (instance? java.lang.Long n)
;     (catch Exception e
;       false)))

(def LongNumber (schema/pred #(instance? java.lang.Long %)))

(def AllowedBoard
  {:uuid lib-schema/UniqueID
   :access common/AccessLevel
   schema/Keyword schema/Any})

;; ----- Metadata -----

(def reserved-properties
  "Properties of a resource that can't be specified during a create and are ignored during an update."
  (clj-set/union common/reserved-properties #{:board-slug :published-at :publisher :secure-uuid :user-visibility}))

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

(defn- publisher-user-visibility [author timestamp]
  {(keyword (:user-id author)) {:dismiss-at timestamp
                                :follow true}})

(defn- publish-props
  "Provide properties for an initially published entry."
  [entry timestamp author]
  (if (= (keyword (:status entry)) :published)
    (-> entry
      (assoc :published-at timestamp)
      (assoc :publisher author)
      (update :user-visibility merge (publisher-user-visibility author timestamp)))
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
                     (assoc :revision-author (first (:author original-entry))))
        updated-entry (if-not (:deleted original-entry)
                        (update-entry conn
                                      (assoc updated-entry :revision-id revision-id-new)
                                      updated-entry
                                      ts)
                        updated-entry)]
    (db-common/create-resource conn versions-table-name revision ts)
    updated-entry))

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
  ([conn entry :- common/Entry]
   (let [ts (if (and (seq (:published-at entry))
                      (= (keyword (:status entry)) :published))
               (:published-at entry)
               (db-common/current-timestamp))]
     (create-entry! conn entry ts)))

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
  (log-query-time "update-entry, start")
  (let [merged-entry (merge original-entry (ignore-props entry))
        attachments (:attachments merged-entry)
        authors-entry (assoc merged-entry :author (:author entry))
        updated-entry (assoc authors-entry :attachments (timestamp-attachments attachments ts))
        _valid? (schema/validate common/Entry updated-entry)
        result (db-common/update-resource conn table-name primary-key original-entry updated-entry ts)]
    (log-query-time "update-entry, finish")
    result))

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
  (log-query-time "update-entry-no-version!")
  (when-let [original-entry (get-entry conn uuid)]
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
  (log-query-time "update-entry-no-user!")
  (when-let [original-entry (get-entry conn uuid)]
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
  (log-query-time "update-entry!")
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
  (if (get-entry conn (:uuid entry))
    (update-entry! conn (:uuid entry) entry user)
    (create-entry! conn entry)))

(schema/defn ^:always-validate publish-entry! :- (schema/maybe common/Entry)
  "
  Given the UUID of the entry, an optional updated entry map, and a user (as the publishing author),
  publish the entry and return the updated entry on success.
  "
  ([conn uuid :- lib-schema/UniqueID org :- common/Org user :- lib-schema/User]
   (publish-entry! conn uuid {} org user))

  ([conn uuid :- lib-schema/UniqueID entry-props _org :- common/Org user :- lib-schema/User]
  {:pre [(db-common/conn? conn)
         (map? entry-props)]}
  (when-let [original-entry (get-entry conn uuid)]
    (log-query-time "publish-entry!, start")
    (let [authors (:author original-entry)
          ts (db-common/current-timestamp)
          publisher (lib-schema/author-for-user user)
          old-user-visibility (or (:user-visibility original-entry) {})
          next-user-visibility (merge old-user-visibility (publisher-user-visibility publisher ts))
          merged-entry (merge original-entry entry-props {:status :published
                                                          :published-at ts
                                                          :publisher publisher
                                                          :secure-uuid (db-common/unique-id)
                                                          :user-visibility next-user-visibility})
          updated-authors (conj authors (assoc publisher :updated-at ts))
          entry-update (assoc merged-entry :author updated-authors)]
      (schema/validate common/Entry entry-update)
      (let [updated-entry (db-common/update-resource conn table-name primary-key original-entry entry-update ts)
            ;; copy current version to versions table, increment revision uuid
            versioned-entry (create-version conn updated-entry entry-update)]
        ;; Delete the draft entry's interactions
        (db-common/delete-resource conn common/interaction-table-name :resource-uuid uuid)
        (log-query-time "publish-entry!, finish")
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
  [conn _entry entry-version user]
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
  (log-query-time "list-comments-for-entry, start")
  (let [comments (filter :body (db-common/read-resources conn common/interaction-table-name "resource-uuid" uuid list-comment-properties))]
    (log-query-time "list-comments-for-entry, finish")
    comments))

(schema/defn ^:always-validate list-reactions-for-entry
  "Given the UUID of the entry, return a list of the reactions for the entry."
  [conn uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (log-query-time "list-reactions-for-entry, start")
  (let [reactions (filter :reaction (db-common/read-resources conn common/interaction-table-name "resource-uuid" uuid [:uuid :author :reaction :created-at]))]
    (log-query-time "list-reactions-for-entry, finish")
    reactions))

;; ----- Collection of entries -----

(schema/defn ^:always-validate list-entries-by-org
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a direction, one of `:before` or `:after`, return the published entries for the org with any interactions.
  "
  ([conn org-uuid :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)]}
  (log-query-time "list-entries-by-org(2), start")
  (let [entry (db-common/read-resources conn table-name :org-uuid org-uuid)]
    (log-query-time "list-entries-by-org(2), finish")
    entry))

  ([conn org-uuid :- lib-schema/UniqueID order start :- LongNumber
    direction allowed-boards :- (schema/maybe [AllowedBoard]) {:keys [must-see count container-id] :or {must-see false count false container-id nil}}]
  {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)]}
  (log-query-time "list-entries-by-org(7), start")
  (let [index-name (if allowed-boards
                     :status-board-uuid
                     :status-org-uuid)
        index-value (if allowed-boards
                      (map #(vec [:published (:uuid %)]) allowed-boards)
                      [[:published org-uuid]])
        filter-map (if-not must-see
                     [{:fn :contains :value (map :uuid allowed-boards) :field :board-uuid}]
                     [{:fn :contains :value (map :uuid allowed-boards) :field :board-uuid}
                      {:fn :eq :field :must-see :value (boolean (#{true "true"} must-see))}])
        result (db-common/read-all-resources-and-relations conn table-name index-name index-value "published-at" order
                                                           start direction filter-map :interactions common/interaction-table-name
                                                           :uuid :resource-uuid list-comment-properties
                                                           {:count count :container-id container-id})]
    (log-query-time "list-entries-by-org(7), finish")
    result)))

(schema/defn ^:always-validate paginated-entries-by-org
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a number of results, return the published entries for the org with any interactions.
  "
  ([conn org-uuid :- lib-schema/UniqueID order start :- LongNumber direction limit sort-type
    allowed-boards :- (schema/maybe [AllowedBoard]) {:keys [count unseen container-id] :or {count false unseen false container-id nil}}]
   (paginated-entries-by-org conn org-uuid order start direction limit sort-type allowed-boards nil nil {:count count :unseen unseen :container-id container-id}))

  ([conn org-uuid :- lib-schema/UniqueID order start :- LongNumber direction limit sort-type
    allowed-boards :- (schema/maybe [AllowedBoard]) follow-data {:keys [count unseen container-id] :or {count false unseen false container-id nil}}]
   (paginated-entries-by-org conn org-uuid order start direction limit sort-type allowed-boards follow-data nil {:count count :unseen unseen :container-id container-id}))

  ([conn org-uuid :- lib-schema/UniqueID order start :- LongNumber direction limit sort-type
    allowed-boards :- (schema/maybe [AllowedBoard]) follow-data container-last-seen-at {:keys [count unseen container-id] :or {count false unseen false container-id nil}}]
   {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)
          (integer? limit)
          (#{:recent-activity :recently-posted :digest} sort-type)]}
   (log-query-time "paginated-entries-by-org, start")
   (let [index-name (if allowed-boards
                      :status-board-uuid
                      :status-org-uuid)
         index-value (if allowed-boards
                       (map #(vec [:published (:uuid %)]) allowed-boards)
                       [[:published org-uuid]])
         result (storage-db-common/read-paginated-entries conn table-name index-name index-value order start direction
                                                          limit sort-type common/interaction-table-name allowed-boards follow-data
                                                          container-last-seen-at list-comment-properties nil
                                                          {:count count :unseen unseen :container-id container-id})]
     (log-query-time "paginated-entries-by-org, finish")
     result)))

(schema/defn ^:always-validate paginated-entries-by-org-old
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a number of results, return the published entries for the org with any interactions.
  "
  ([conn org-uuid :- lib-schema/UniqueID order start :- LongNumber direction limit sort-type
    allowed-boards :- [AllowedBoard] {:keys [count unseen container-id] :or {count false unseen false container-id nil}}]
   (paginated-entries-by-org-old conn org-uuid order start direction limit sort-type allowed-boards nil nil {:count count :unseen unseen :container-id container-id}))

  ([conn org-uuid :- lib-schema/UniqueID order start :- LongNumber direction limit sort-type
    allowed-boards :- [AllowedBoard] follow-data {:keys [count unseen container-id] :or {count false unseen false container-id nil}}]
   (paginated-entries-by-org-old conn org-uuid order start direction limit sort-type allowed-boards follow-data nil {:count count :unseen unseen :container-id container-id}))

  ([conn org-uuid :- lib-schema/UniqueID order start :- LongNumber direction limit sort-type
    allowed-boards :- [AllowedBoard] follow-data container-last-seen-at {:keys [count unseen container-id] :or {count false unseen false container-id nil}}]
   {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)
          (integer? limit)
          (#{:recent-activity :recently-posted :digest} sort-type)]}
   (log-query-time "paginated-entries-by-org-old, start")
   (let [result (storage-db-common/read-paginated-entries-old conn table-name :status-org-uuid [:published org-uuid] order start direction
                                                              limit sort-type common/interaction-table-name allowed-boards follow-data
                                                              container-last-seen-at list-comment-properties nil
                                                              {:count count :unseen unseen :container-id container-id})]
     (log-query-time "paginated-entries-by-org-old, finish")
     result)))

(schema/defn ^:always-validate paginated-entries-by-board
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a limit, return the published entries for the org with any interactions.
  "
  [conn allowed-board :- AllowedBoard order start :- LongNumber direction limit sort-type {:keys [count status] :or {count false status :published}}]
  {:pre [(db-common/conn? conn)
         (#{:desc :asc} order)
         (#{:before :after} direction)
         (integer? limit)
         (#{:recent-activity :recently-posted} sort-type)]}
  (log-query-time "paginated-entries-by-board, start")
  (let [index-name (if (#{:draft :published} status) :status-board-uuid :board-uuid)
        index-value (if (#{:draft :published} status)
                      [[status (:uuid allowed-board)]]
                      [[(:uuid allowed-board)]])
        result (storage-db-common/read-paginated-entries conn table-name index-name index-value order start
                                                         direction limit sort-type common/interaction-table-name
                                                         [allowed-board] nil nil list-comment-properties nil {:count count})]
    (log-query-time "paginated-entries-by-board, finish")
    result))

(schema/defn ^:always-validate paginated-entries-by-board-old
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a limit, return the published entries for the org with any interactions.
  "
  [conn allowed-board :- AllowedBoard order start :- LongNumber direction limit sort-type {:keys [count status] :or {count false status :published}}]
  {:pre [(db-common/conn? conn)
         (#{:desc :asc} order)
         (#{:before :after} direction)
         (integer? limit)
         (#{:recent-activity :recently-posted} sort-type)]}
  (log-query-time "paginated-entries-by-board-old, start")
  (let [index-name (if (#{:draft :published} status) :status-board-uuid :board-uuid)
        index-value (if (#{:draft :published} status)
                      [[status (:uuid allowed-board)]]
                      [[(:uuid allowed-board)]])
        result (storage-db-common/read-paginated-entries-old conn table-name index-name index-value order start
                                                             direction limit sort-type common/interaction-table-name
                                                             [allowed-board] nil nil list-comment-properties nil {:count count})]
    (log-query-time "paginated-entries-by-board-old, finish")
    result))

(schema/defn ^:always-validate last-entry-of-board
  "
  Given the UUID of the org, an order, one of `:asc` or `:desc`, a start date as an ISO8601 timestamp,
  and a limit, return the published entries for the org with any interactions.
  "
  [conn board-uuid]
  {:pre [(db-common/conn? conn)]}
  (storage-db-common/last-entry-of-board conn board-uuid))

(schema/defn ^:always-validate list-entries-by-org-author

  ([conn org-uuid :- lib-schema/UniqueID author-uuid :- lib-schema/UniqueID order start :- LongNumber
    direction limit sort-type allowed-boards :- (schema/maybe [AllowedBoard])]
   (list-entries-by-org-author conn org-uuid author-uuid order start direction limit sort-type allowed-boards nil {:count false}))

  ([conn org-uuid :- lib-schema/UniqueID author-uuid :- lib-schema/UniqueID order start :- LongNumber
    direction limit sort-type allowed-boards :- (schema/maybe [AllowedBoard]) container-last-seen-at]
   (list-entries-by-org-author conn org-uuid author-uuid order start direction limit sort-type allowed-boards container-last-seen-at {:count false}))

  ([conn org-uuid :- lib-schema/UniqueID author-uuid :- lib-schema/UniqueID order start :- LongNumber
    direction limit sort-type allowed-boards :- (schema/maybe [AllowedBoard]) container-last-seen-at {:keys [count container-id] :or {count false}}]
   {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)
          (integer? limit)
          (#{:recent-activity :recently-posted} sort-type)]}
   (log-query-time "list-entries-by-org-author, start")
   (let [index-name (if allowed-boards
                      :status-board-uuid-author-id
                      :status-org-uuid-author-id)
         index-value (if allowed-boards
                       (map #(vec [:published (:uuid %) author-uuid]) allowed-boards)
                       [[:published org-uuid author-uuid]])
         result (storage-db-common/read-paginated-entries conn table-name index-name index-value order start direction
                                                          limit sort-type common/interaction-table-name allowed-boards nil container-last-seen-at
                                                          list-comment-properties nil {:count count :container-id container-id})]
     (log-query-time "list-entries-by-org-author, finish")
     result)))

(schema/defn ^:always-validate list-entries-by-org-author-old

  ([conn org-uuid :- lib-schema/UniqueID author-uuid :- lib-schema/UniqueID order start :- LongNumber
    direction limit sort-type allowed-boards :- (schema/maybe [AllowedBoard])]
   (list-entries-by-org-author-old conn org-uuid author-uuid order start direction limit sort-type allowed-boards nil {:count false}))

  ([conn org-uuid :- lib-schema/UniqueID author-uuid :- lib-schema/UniqueID order start :- LongNumber
    direction limit sort-type allowed-boards :- (schema/maybe [AllowedBoard]) container-last-seen-at]
   (list-entries-by-org-author-old conn org-uuid author-uuid order start direction limit sort-type allowed-boards container-last-seen-at {:count false}))

  ([conn org-uuid :- lib-schema/UniqueID author-uuid :- lib-schema/UniqueID order start :- LongNumber
    direction limit sort-type allowed-boards :- (schema/maybe [AllowedBoard]) container-last-seen-at {:keys [count container-id] :or {count false}}]
   {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)
          (integer? limit)
          (#{:recent-activity :recently-posted} sort-type)]}
   (log-query-time "list-entries-by-org-author-old, start")
   (let [result (storage-db-common/read-paginated-entries-old conn table-name :status-org-uuid-author-id [:published org-uuid author-uuid] order start direction
                                                              limit sort-type common/interaction-table-name allowed-boards nil container-last-seen-at
                                                              list-comment-properties nil {:count count :container-id container-id})]
     (log-query-time "list-entries-by-org-author-old, finish")
     result)))

(schema/defn ^:always-validate list-drafts-by-org-author
  "
  Given the UUID of the org and a user-id return the entries by the author with any interactions.
  "
  [conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)]}
  (log-query-time "list-drafts-by-org-author, start")
  (let [result (db-common/read-resources-and-relations conn table-name :status-org-uuid-author-id [[:draft org-uuid user-id]]
                                                       :interactions common/interaction-table-name :uuid :resource-uuid
                                                       list-comment-properties {:count count})]
    (log-query-time "list-drafts-by-org-author, finish")
    result))

(schema/defn ^:always-validate list-entries-by-board
  "Given the UUID of the board, return the published entries for the board with any interactions."
  ([conn allowed-board :- AllowedBoard] (list-entries-by-board conn allowed-board {:count false}))

  ([conn allowed-board :- AllowedBoard {:keys [count] :or {count false}}]
   {:pre [(db-common/conn? conn)]}
   (log-query-time (str "list-entries-by-board(" (:uuid allowed-board) "), start"))
   (let [result (db-common/read-resources-and-relations conn table-name :status-board-uuid [[:published (:uuid allowed-board)]]
                                                        :interactions common/interaction-table-name :uuid :resource-uuid
                                                        list-comment-properties {:count count})]
     (log-query-time (str "list-entries-by-board(" (:uuid allowed-board) "), finish"))
     result)))

(schema/defn ^:always-validate list-all-entries-by-board
  "Given the UUID of the board, return all the entries for the board."
  [conn allowed-board :- AllowedBoard]
  {:pre [(db-common/conn? conn)]}
  (log-query-time "list-all-entries-by-board, start")
  (let [result (db-common/read-resources conn table-name :board-uuid [(:uuid allowed-board)] ["uuid" "status"])]
    (log-query-time "list-all-entries-by-board, finish")
    result))

(schema/defn ^:always-validate list-all-entries-for-inbox
  "Given the UUID of the user, return all the entries the user has access to that have been published
   or have had activity in the last config/unread-days-limit days, then filter by user-visibility on the remaining."
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID order start :- LongNumber limit
    allowed-boards :- (schema/maybe [AllowedBoard])]
   (list-all-entries-for-inbox conn org-uuid user-id order start limit allowed-boards nil {:count false}))

  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID order start :- LongNumber limit
    allowed-boards :- (schema/maybe [AllowedBoard]) {:keys [count] :or {count false}}]
   (list-all-entries-for-inbox conn org-uuid user-id order start limit allowed-boards nil {:count count}))

  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID order start :- LongNumber limit
    allowed-boards :- (schema/maybe [AllowedBoard]) follow-data {:keys [count] :or {count false}}]
   {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (integer? limit)]}
   (log-query-time "list-all-entries-for-inbox, start")
   (let [index-name (if allowed-boards
                      :status-board-uuid
                      :status-org-uuid)
         index-value (if allowed-boards
                       (map #(vec [:published (:uuid %)]) allowed-boards)
                       [[:published org-uuid]])
         result (storage-db-common/read-all-inbox-for-user conn table-name index-name index-value order start limit
                                                           common/interaction-table-name allowed-boards follow-data user-id
                                                           list-comment-properties {:count count})]
     (log-query-time "list-all-entries-for-inbox, finish")
     result)))

(schema/defn ^:always-validate list-entries-for-user-replies
  "List all activities with at least one comment where the user-id has been active part."
  [conn org-uuid :- lib-schema/UniqueID allowed-boards :- (schema/maybe [AllowedBoard]) user-id :- lib-schema/UniqueID
   order start direction limit follow-data container-last-seen-at {:keys [count unseen] :or {count false unseen false}}]
  {:pre [(db-common/conn? conn)
         (#{:desc :asc} order)
         (#{:before :after} direction)
         (integer? limit)]}
  (log-query-time "list-entries-for-user-replies, start")
  (let [result (time (storage-db-common/read-paginated-entries-for-replies conn org-uuid allowed-boards user-id order start
                                                                           direction limit follow-data container-last-seen-at
                                                                           list-comment-properties {:count count :unseen unseen}))]
    (log-query-time "list-entries-for-user-replies, finish")
    result))

;; @FIXME: remove below fn
(schema/defn ^:always-validate list-entries-for-user-replies-old
  ""
  [conn org-uuid :- lib-schema/UniqueID allowed-boards :- (schema/maybe [AllowedBoard]) user-id :- lib-schema/UniqueID
   order start direction limit follow-data container-last-seen-at {:keys [count unseen] :or {count false unseen false}}]
  {:pre [(db-common/conn? conn)
         (#{:desc :asc} order)
         (#{:before :after} direction)
         (integer? limit)]}
  (log-query-time "list-entries-for-user-replies-old, start")
  (let [result (time (storage-db-common/read-paginated-entries-for-replies-old conn org-uuid allowed-boards user-id order start direction limit
                                                                     follow-data container-last-seen-at list-comment-properties
                                                                     {:count count :unseen unseen}))]
    (log-query-time "list-entries-for-user-replies-old, finish")
    result))

;; ----- Entry Bookmarks manipulation -----

(schema/defn ^:always-validate list-all-bookmarked-entries
  "Given the UUID of the user, return all the published entries with a bookmark for the given user."
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID allowed-boards :- (schema/maybe [AllowedBoard]) order start :- LongNumber direction limit]
    (list-all-bookmarked-entries conn org-uuid user-id allowed-boards order start direction limit {:count false}))
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID allowed-boards :- (schema/maybe [AllowedBoard]) order start :- LongNumber direction limit {:keys [count] :or {count false}}]
   {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)]}
   (log-query-time "list-all-bookmarked-entries, start")
   (let [index-name (if allowed-boards ;; empty array means user has 0 boards access
                      :status-board-uuid-bookmark-user-id-multi
                      :org-uuid-status-bookmark-user-id-map-multi)
         index-value (if allowed-boards
                       (map #(vec [:published (:uuid %) user-id]) allowed-boards)
                       [[:published org-uuid user-id]])
         result (storage-db-common/read-paginated-entries conn table-name index-name index-value order
                                                          start direction limit :bookmarked-at
                                                          common/interaction-table-name allowed-boards
                                                          nil nil list-comment-properties user-id {:count count})]
     (log-query-time "list-all-bookmarked-entries, finish")
     result)))

(schema/defn ^:always-validate list-all-bookmarked-entries-old
  "Given the UUID of the user, return all the published entries with a bookmark for the given user."
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID allowed-boards :- (schema/maybe [AllowedBoard]) order start :- LongNumber direction limit]
    (list-all-bookmarked-entries-old conn org-uuid user-id allowed-boards order start direction limit {:count false}))
  ([conn org-uuid :- lib-schema/UniqueID user-id :- lib-schema/UniqueID allowed-boards :- (schema/maybe [AllowedBoard]) order start :- LongNumber direction limit {:keys [count] :or {count false}}]
   {:pre [(db-common/conn? conn)
          (#{:desc :asc} order)
          (#{:before :after} direction)]}
   (log-query-time "list-all-bookmarked-entries-old, start")
   (let [result (storage-db-common/read-paginated-entries-old conn table-name :org-uuid-status-bookmark-user-id-map-multi [:published org-uuid user-id] order
                                                              start direction limit :bookmarked-at
                                                              common/interaction-table-name allowed-boards
                                                              nil nil list-comment-properties user-id {:count count})]
     (log-query-time "list-all-bookmarked-entries-old, finish")
     result)))

(schema/defn ^:always-validate add-bookmark! :- (schema/maybe common/Entry)
  "Add a bookmark for the give entry and user"
  ([conn entry-uuid :- lib-schema/UniqueID user :- lib-schema/User]
   {:pre [(db-common/conn? conn)]}
   (let [original-entry (get-entry conn entry-uuid)]
     (if (seq (filter #(= (:user-id %) (:user-id user)) (:bookmarks original-entry)))
       ;; User has the bookmark already set
       original-entry
       ;; Add the user's bookmark
       (let [updated-bookmarks (vec (conj (:bookmarks original-entry) {:user-id (:user-id user)
                                                                       :bookmarked-at (db-common/current-timestamp)}))]
         (update-entry conn (assoc original-entry :bookmarks updated-bookmarks) original-entry (db-common/current-timestamp)))))))

(schema/defn ^:always-validate remove-bookmark! :- (schema/maybe common/Entry)
  "Remove a bookmark for the give entry uuid and user"
  ([conn entry-uuid :- lib-schema/UniqueID user :- lib-schema/User]
   {:pre [(db-common/conn? conn)]}
   (let [original-entry (get-entry conn entry-uuid)
         updated-entry (update original-entry :bookmarks #(filterv (fn[bm] (not= (:user-id bm) (:user-id user))) %))]
     (update-entry conn updated-entry original-entry (db-common/current-timestamp)))))

;; ----- Polls ------

(schema/defn ^:always-validate get-poll :- (schema/maybe common/Poll)
  ([conn entry-uuid :- lib-schema/UniqueID poll-uuid :- lib-schema/UniqueID]
   {:pre [(db-common/conn? conn)]}
   (get-poll conn (get-entry conn entry-uuid) poll-uuid))
  ([conn _entry-uuid :- lib-schema/UniqueID entry :- common/Entry poll-uuid :- lib-schema/UniqueID]
   {:pre [(db-common/conn? conn)]}
   (get-in entry [:polls (keyword poll-uuid)])))

(schema/defn ^:always-validate get-poll-reply :- (schema/maybe common/PollReply)
  ([conn entry-uuid :- lib-schema/UniqueID poll-uuid :- lib-schema/UniqueID reply-id :- lib-schema/UniqueID]
   {:pre [(db-common/conn? conn)]}
   (let [entry (get-entry conn entry-uuid)]
     (get-poll-reply conn entry-uuid entry poll-uuid (get-poll conn entry-uuid entry poll-uuid) reply-id)))
  ([conn entry-uuid :- lib-schema/UniqueID entry :- common/Entry poll-uuid :- lib-schema/UniqueID reply-id :- lib-schema/UniqueID]
   {:pre [(db-common/conn? conn)]}
   (get-poll-reply conn entry-uuid entry poll-uuid (get-poll conn entry-uuid entry poll-uuid) reply-id))
  ([conn _entry-uuid :- lib-schema/UniqueID _entry :- common/Entry _poll-uuid :- lib-schema/UniqueID poll :- common/Poll reply-id :- lib-schema/UniqueID]
   {:pre [(db-common/conn? conn)]}
   (get-in poll [:replies (keyword reply-id)])))

(schema/defn ^:always-validate poll-reply-vote! :- (schema/maybe common/Entry)
  [conn entry-uuid :- lib-schema/UniqueID poll-uuid :- lib-schema/UniqueID
   reply-id :- lib-schema/UniqueID user-id :- lib-schema/UniqueID add?]
  (storage-db-common/update-poll-vote conn table-name entry-uuid poll-uuid reply-id user-id add?))

;; ----- Pins -----

(defn- pin-for [user ts]
  {:author (lib-schema/author-for-user user)
   :pinned-at ts})

(schema/defn ^:always-validate toggle-pin! :- common/Entry
  [conn entry-uuid :- lib-schema/UniqueID pin-container-uuid :- lib-schema/UniqueID user :- lib-schema/User]
  (when-let [original-entry (get-entry conn entry-uuid)]
    (let [ts (db-common/current-timestamp)
          current-pins (or (:pins original-entry) {})
          pin-container-kw (keyword pin-container-uuid)
          remove-pin? (contains? current-pins pin-container-kw)
          updated-pins (if remove-pin?
                         (dissoc current-pins pin-container-kw)
                         (assoc current-pins pin-container-kw (pin-for user ts)))
          updated-entry (assoc original-entry :pins updated-pins)]
      (schema/validate common/Entry updated-entry)
      (db-common/update-resource conn table-name primary-key original-entry updated-entry ts))))

;; ----- Armageddon -----

(defn delete-all-entries!
  "Use with caution! Failure can result in partial deletes. Returns `true` if successful."
  [conn]
  {:pre [(db-common/conn? conn)]}
  ;; Delete all interactions and entries
  (db-common/delete-all-resources! conn common/interaction-table-name)
  (db-common/delete-all-resources! conn versions-table-name)
  (db-common/delete-all-resources! conn table-name))
