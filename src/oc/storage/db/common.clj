(ns oc.storage.db.common
  "CRUD function to retrieve entries from RethinkDB with pagination."
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [rethinkdb.query :as r]
            [oc.lib.schema :as lib-schema]
            [oc.lib.time :as lib-time]
            [oc.storage.config :as config]
            [oc.lib.db.common :as db-common]))

(defn read-paginated-entries
 ([conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
   relation-fields {:keys [count unseen] :or {count false unseen false}}]
 (read-paginated-entries conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
  nil nil relation-fields nil {:count count :unseen false}))

 ([conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
   relation-fields user-id {:keys [count unseen] :or {count false unseen false}}]
  (read-paginated-entries conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
  nil nil relation-fields user-id {:count count :unseen false}))

 ([conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
  follow-data container-last-seen-at relation-fields user-id {:keys [count unseen] :or {count false unseen false}}]
 {:pre [(db-common/conn? conn)
        (db-common/s-or-k? table-name)
        (db-common/s-or-k? index-name)
        (or (string? index-value) (sequential? index-value))
        (db-common/s-or-k? relation-table-name)
        (#{:desc :asc} order)
        (not (nil? start))
        (#{:after :before} direction)
        (integer? limit)
        (or (#{:recent-activity :recently-posted} sort-type)
            (and (= sort-type :bookmarked-at)
                 (seq user-id)))
        (or (nil? container-last-seen-at)
            (string? container-last-seen-at))
        (or (nil? follow-data)
            (coll? follow-data))
        (sequential? relation-fields)
        (every? db-common/s-or-k? relation-fields)
        (boolean? count)
        (boolean? unseen)]}
  (let [index-values (if (sequential? index-value) index-value [index-value])
        order-fn (if (= order :desc) r/desc r/asc)
        unseen-cap-ms (if (zero? config/unseen-cap-days)
                        (* 60 60 24 365 50 1000) ;; default is 50 years cap if no config is set
                        (* 60 60 24 config/unseen-cap-days 1000))]
    (db-common/with-timeout db-common/default-timeout
      (as-> (r/table table-name) query
            (r/get-all query index-values {:index index-name})
            (r/filter query (r/fn [post-row]
              (r/and ;; Filter on allowed-boards if necessary (if not ISequential means no filter needed)
                     (r/or (not (sequential? allowed-boards))
                           (r/contains allowed-boards (r/get-field post-row :board-uuid)))
                     ;; and filter on follow data:
                     (r/or ;; no filter if it's nil
                           (not (map? follow-data))
                           ;; filter on followed authors and on not unfollowed boards
                           (r/and (:following follow-data)
                                  (r/or (r/contains (vec (:follow-publisher-uuids follow-data)) (r/get-field post-row [:publisher :user-id]))
                                        (r/not (r/contains (vec (:unfollow-board-uuids follow-data)) (r/get-field post-row :board-uuid)))))
                           ;; filter on not followed authors and on unfollowed boards
                           (r/and (:unfollowing follow-data)
                                  (r/not (r/contains (vec (:follow-publisher-uuids follow-data)) (r/get-field post-row [:publisher :user-id])))
                                  (r/contains (vec (:unfollow-board-uuids follow-data)) (r/get-field post-row :board-uuid)))))))
            ;; Merge in last-activity-at, last-seen-at and sort-value
            (r/merge query (r/fn [post-row]
              (let [last-activity-at (-> (r/table relation-table-name)
                                      (r/get-all [[(r/get-field post-row :uuid) true]] {:index :resource-uuid-comment})
                                      (r/max :created-at)
                                      (r/get-field :created-at)
                                      (r/default (r/get-field post-row :published-at))
                                      (r/default (r/get-field post-row :created-at)))
                    sort-field (cond
                                (= sort-type :recent-activity)
                                last-activity-at
                                (= sort-type :bookmarked-at)
                                (-> (r/get-field post-row [:bookmarks])
                                 (r/filter {:user-id user-id})
                                 (r/nth 0)
                                 (r/get-field :bookmarked-at)
                                 (r/default (r/get-field post-row :published-at))
                                 (r/default (r/get-field post-row :created-at)))
                                :else
                                (r/default
                                 (r/get-field post-row :published-at)
                                 (r/get-field post-row :created-at)))
                    sort-value-base (-> sort-field
                                     (r/iso8601)
                                     (r/to-epoch-time)
                                     (r/mul 1000)
                                     (r/round))
                    unseen-entry? (r/and (seq container-last-seen-at)
                                         (r/gt (r/get-field post-row :published-at) container-last-seen-at))
                    unseen-with-cap? (r/and unseen-entry?
                                            (r/gt sort-value-base
                                                  (-> (r/now) (r/to-epoch-time) (r/mul 1000) (r/round) (r/sub unseen-cap-ms))))
                    sort-value (r/branch unseen-with-cap?
                                ;; If the item is unseen and was published (for recently posted) or bookmarked
                                ;; (for bookmarks) or last activity was (for recent activity) in the cap window
                                ;; let's add the cap window to the publish timestamp so it will sort before the seen ones
                                (r/add sort-value-base unseen-cap-ms)
                                ;; Or use the plain sort value in case it's seen or it's out of the cap window
                                sort-value-base)]
                {;; Date of the last added comment on this entry
                 :last-activity-at last-activity-at
                 :publish-time sort-value-base
                 :sort-value sort-value
                 :unseen unseen-with-cap?
                })))
            ;; Filter out:
            (r/filter query (r/fn [row]
              ;; All records after/before the start
              (r/and ;; Filter out seen entries if unseen flag is on
                     (r/or (r/not unseen)
                           (r/default (r/get-field row :unseen) false))
                     (r/or (r/and (= direction :before)
                                  (r/gt start (r/get-field row :publish-time)))
                           (r/and (= direction :after)
                                  (r/le start (r/get-field row :publish-time)))))))
            ;; Merge in all the interactions
            (if-not count
              (r/merge query (r/fn [post-row]
                {:interactions (-> (r/table relation-table-name)
                                   (r/get-all [(r/get-field post-row :uuid)] {:index :resource-uuid})
                                   (r/pluck relation-fields)
                                   (r/coerce-to :array))}))
              query)
            (if-not count
              (r/order-by query (order-fn :sort-value))
              query)
            ;; Apply count if needed
            (if count (r/count query) query)
            ;; Apply limit
            (if (and (pos? limit)
                     (not count))
              (r/limit query limit)
              query)
            ;; Run!
            (r/run query conn)
            (if (= (type query) rethinkdb.net.Cursor)
              (seq query)
              query))))))

(defn read-all-inbox-for-user

 ([conn table-name index-name index-value order start limit relation-table-name allowed-boards user-id
   relation-fields {:keys [count] :or {count false}}]
  (read-all-inbox-for-user conn table-name index-name index-value order start limit relation-table-name allowed-boards nil
   user-id relation-fields {:count count}))

 ([conn table-name index-name index-value order start limit relation-table-name allowed-boards follow-data
   user-id relation-fields {:keys [count] :or {count false}}]
 {:pre [(db-common/conn? conn)
        (db-common/s-or-k? table-name)
        (db-common/s-or-k? index-name)
        (or (string? index-value) (sequential? index-value))
        (db-common/s-or-k? relation-table-name)
        (#{:desc :asc} order)
        (not (nil? start))
        (integer? limit)
        (string? user-id)
        (sequential? relation-fields)
        (every? db-common/s-or-k? relation-fields)]}
  (let [index-values (if (sequential? index-value) index-value [index-value])
        order-fn (if (= order :desc) r/desc r/asc)
        minimum-date-timestamp (f/unparse lib-time/timestamp-format (t/minus (t/now) (t/days config/unread-days-limit)))]
    (db-common/with-timeout db-common/default-timeout
      (as-> (r/table table-name) query
            (r/get-all query index-values {:index index-name})
            ;; Filter out:
            (r/filter query (r/fn [post-row]
              (r/and ;; All records in boards the user has no access
                     (r/or (not (sequential? allowed-boards))
                           (r/contains allowed-boards (r/get-field post-row :board-uuid)))
                     ;; that have unfollow (or :unfollow is not specified)
                     (r/not (r/default (r/get-field post-row [:user-visibility user-id :unfollow]) false))

                     (r/or ;; No follow data are passed
                           (not (map? follow-data))
                           ;; or we are requesting the unfollowed posts
                           (r/and (:unfollowing follow-data)
                                  (r/not (r/contains (vec (:follow-publisher-uuids follow-data)) (r/get-field post-row [:publisher :user-id])))
                                  (r/contains (vec (:unfollow-board-uuids follow-data)) (r/get-field post-row :board-uuid)))
                           (r/and (:following follow-data)
                                  (r/or (r/contains (vec (:follow-publisher-uuids follow-data)) (r/get-field post-row [:publisher :user-id]))
                                        (r/not (r/contains (vec (:unfollow-board-uuids follow-data)) (r/get-field post-row :board-uuid)))))))))
            ;; Merge in a last-activity-at date for each post (last comment created-at, fallback to published-at)
            (r/merge query (r/fn [post-row]
              (let [last-activity-at (-> (r/table relation-table-name)
                                      (r/get-all [[(r/get-field post-row :uuid) true]] {:index :resource-uuid-comment})
                                      (r/filter (r/fn [interaction-row]
                                        (r/ne (r/get-field interaction-row [:author :user-id]) user-id)))
                                      (r/max :created-at)
                                      (r/get-field [:created-at])
                                      (r/default (r/get-field post-row :published-at))
                                      (r/default (r/get-field post-row :created-at)))
                    sort-value (-> last-activity-at
                                (r/iso8601)
                                (r/to-epoch-time)
                                (r/mul 1000)
                                (r/round))]
                {:last-activity-at last-activity-at
                 :sort-value sort-value})))
            ;; Filter out:
            (r/filter query (r/fn [post-row]
              (r/and ;; Leave in only posts whose last activity is within a certain amount of time
                     (r/gt (r/get-field post-row :last-activity-at) minimum-date-timestamp)
                     ;; All records that have a dismiss-at later or equal than the last activity
                     (r/gt (r/get-field post-row :last-activity-at)
                           (r/default (r/get-field post-row [:user-visibility user-id :dismiss-at]) "")))))
            ;; Merge in all the interactions
            (if-not count
              (r/merge query (r/fn [post-row]
                {:interactions (-> (r/table relation-table-name)
                                   (r/get-all [(r/get-field post-row :uuid)] {:index :resource-uuid})
                                   (r/pluck relation-fields)
                                   (r/coerce-to :array))}))
              query)
            ;; Apply a filter on the last-activity-at date
            (r/filter query (r/fn [row]
             (r/gt start (r/get-field row :sort-value))))
            ;; Sort records when not counting
            (if-not count (r/order-by query (order-fn :sort-value)) query)
            ;; Apply count if needed
            (if count (r/count query) query)
            ;; Apply limit
            (if (and (pos? limit)
                     (not count))
              (r/limit query limit)
              query)
            ;; Run!
            (r/run query conn)
            (if (= (type query) rethinkdb.net.Cursor)
              (seq query)
              query))))))

(defn update-poll-vote
  "
  Atomic update of poll vote to avoid race conditions while multiple
  users are voting together.
  `add-vote?` can be true if the user is casting his vote or false if he's
  removing it.
  "
  [conn table-name entry-uuid poll-uuid reply-id user-id add-vote?]
  {:pre [(db-common/conn? conn)
         (db-common/s-or-k? table-name)
         (boolean? add-vote?)]}
  (let [ts (db-common/current-timestamp)
        set-operation (if add-vote? r/set-insert r/set-difference)
        user-id-value (if add-vote? user-id [user-id])
        update (db-common/with-timeout db-common/default-timeout
                  (-> (r/table table-name)
                      (r/get entry-uuid)
                      (r/update (r/fn [entry]
                       {:polls {poll-uuid {:replies
                        (-> entry
                         (r/get-field [:polls poll-uuid :replies])
                         (r/values)
                         (r/map (r/fn [reply-data]
                          (r/branch
                           (r/eq (r/get-field reply-data [:reply-id]) reply-id)
                           (r/object (r/get-field reply-data :reply-id)
                            (r/merge reply-data
                             {:votes (-> reply-data
                                      (r/get-field [:votes])
                                      (r/default [])
                                      (set-operation user-id-value))}))
                           (r/object (r/get-field reply-data :reply-id)
                            (r/merge reply-data
                             {:votes (-> reply-data
                                      (r/get-field [:votes])
                                      (r/default [])
                                      (r/set-difference [user-id]))})))))
                         (r/reduce (r/fn [a b]
                          (r/merge a b))))}}}))
                      (r/run conn)))]
    (if (or (= 1 (:replaced update)) (= 1 (:unchanged update)))
      (db-common/read-resource conn table-name entry-uuid)
      (throw (RuntimeException. (str "RethinkDB update failure: " update))))))

(defn create-version [conn versions-table-name updated-entry author]
  (let [version (atom nil)
        insert (db-common/with-timeout db-common/default-timeout
                (-> (r/table versions-table-name)
                 (r/insert
                  (r/merge updated-entry
                   (let [rev-id (as-> (r/table versions-table-name) q
                                 (r/get-all q [(:uuid updated-entry)] {:index :uuid})
                                 (r/max q :revision-id)
                                 (r/get-field q :revision-id)
                                 (r/default q -1)
                                 (r/add q 1)
                                 (r/run q conn))
                         versioned-uuid (str (:uuid updated-entry) "-v" rev-id)
                         ts (db-common/current-timestamp)]
                     (reset! version versioned-uuid)
                     {:revision-id rev-id
                      :version-uuid versioned-uuid
                      :revision-date ts
                      :revision-author author})))
                 (r/run conn)))]
    (if (and @version
             (= 1 (:inserted insert)))
      (db-common/read-resource conn versions-table-name @version)
      (throw (RuntimeException. (str "RethinkDB insert failure: " insert))))))

(defn delete-versions
  ([conn versions-table-name entry-uuid]
   (delete-versions conn versions-table-name entry-uuid {:force-delete-all? false}))
  ([conn versions-table-name entry-uuid
    {force-delete-all? :force-delete-all? ;; By default or if not specified, it deletes only the drafts records
     :as options}]
   (db-common/with-timeout db-common/default-timeout
    (-> (r/table versions-table-name)
     (r/get-all [entry-uuid] {:index :uuid})
     (r/filter (r/fn [v]
      (r/eq (r/get-field v :status) :draft)))
     (r/delete)
     (r/run conn)))))

(defn read-paginated-entries-for-replies
  "Read all entries with at least one comment the user has access to. Filter out those not activily followed
   by the current user. Sort those with unseen content at the top and sort everything by last activity descendant."
  [conn org-uuid allowed-boards user-id order start direction limit follow-data container-last-seen-at relation-fields {:keys [count unseen] :or {count false unseen false}}]
  {:pre [(db-common/conn? conn)
         (lib-schema/unique-id? org-uuid)
         (or (sequential? allowed-boards)
             (nil? allowed-boards))
         (lib-schema/unique-id? user-id)
         (or (nil? follow-data)
             (map? follow-data))
         (or (nil? container-last-seen-at)
             (string? container-last-seen-at))
         (#{:desc :asc} order)
         (not (nil? start))
         (#{:after :before} direction)
         (or (zero? limit) ;; means all
             (pos? limit))
         (or (nil? relation-fields)
             (coll? relation-fields))
         (boolean? count)
         (boolean? unseen)]}
  (let [order-fn (if (= order :desc) r/desc r/asc)
        unseen-cap-ms (if (zero? config/unseen-cap-days)
                        (* 60 60 24 365 50 1000) ;; 50 years cap
                        (* 60 60 24 config/unseen-cap-days 1000))]
    (db-common/with-timeout db-common/default-timeout
      (as-> (r/table "entries") query
       (r/get-all query [[:published org-uuid]] {:index :status-org-uuid})
       ;; Make an initial filter to select only posts the user has access to
       (r/filter query (r/fn [row]
        (r/or (not (sequential? allowed-boards))
              (r/contains allowed-boards (r/get-field row :board-uuid)))))
       ;; Merge in last-activity-at and entry
       (r/merge query (r/fn [row]
        (let [interactions-base (-> (r/table "interactions")
                                 (r/get-all [[(r/get-field row :uuid) true]] {:index :resource-uuid-comment}))
              last-activity-at (-> interactions-base
                                 (r/max :created-at)
                                 (r/get-field :created-at)
                                 (r/default (r/get-field row [:published-at])))
              sort-value-base (-> last-activity-at
                               (r/iso8601)
                               (r/to-epoch-time)
                               (r/mul 1000)
                               (r/round))
              published-ms (-> (r/get-field row [:published-at]) (r/iso8601) (r/to-epoch-time) (r/mul 1000) (r/round))
              has-seen-at? (seq container-last-seen-at)
              container-seen-ms (when has-seen-at?
                                  (-> container-last-seen-at
                                   (r/iso8601)
                                   (r/to-epoch-time)
                                   (r/mul 1000)
                                   (r/round)))
              last-activity-ms (-> last-activity-at
                                (r/iso8601)
                                (r/to-epoch-time)
                                (r/mul 1000)
                                (r/round))
              unseen-entry? (r/or (r/not has-seen-at?)
                                  (r/gt published-ms container-seen-ms))
              unseen-cap-ms (-> (r/now) (r/to-epoch-time) (r/mul 1000) (r/round) (r/sub unseen-cap-ms))
              unseen-activity? (r/or unseen-entry?
                                     (r/gt last-activity-ms container-seen-ms))
              unseen-with-cap? (r/and unseen-activity?
                                      (r/gt last-activity-ms unseen-cap-ms))
              sort-value (r/branch unseen-with-cap?
                           ;; If the item is unseen and was published in the cap window
                           ;; let's add the cap window to the publish timestamp so it will sort before the seen items
                           (r/add sort-value-base unseen-cap-ms)
                           ;; The timestamp in seconds
                           sort-value-base)]
          {;; Date of the last added comment on this thread
           :last-activity-at last-activity-at
           :comments-count (r/count interactions-base)
           :sort-value sort-value
           :unseen unseen-entry?
           :last-activity-ms last-activity-ms
           ; :reply-authors (-> interactions-base
           ;                 (r/coerce-to :array)
           ;                 (r/map (r/fn [inter] (r/get-field inter [:author :user-id])))
           ;                 (r/default []))
           :interactions (-> (r/table "interactions")
                          (r/get-all [(r/get-field row :uuid)] {:index :resource-uuid})
                          (r/pluck relation-fields)
                          (r/coerce-to :array))
           ;; FIXME: debug
           ; :debug {:unseen-with-cap unseen-with-cap?
           ;         :unseen-activity unseen-activity?}
          })))
       ;; Filter by user-visibility
       (r/filter query (r/fn [row]
        (r/and (r/gt (r/get-field row [:comments-count]) 0)
               ;; Filter out entries without unseen comments if unseen flag is on
               (r/or (r/not unseen)
                     (r/default (r/get-field row :unseen-comments) false))
               ;; Filter out threads that have comments only from the current user
               ; (-> (r/get-field row [:reply-authors])
               ;  (r/filter user-id)
               ;  (r/is-empty)
               ;  r/not)
               ;; All records after/before the start
               (r/or (r/and (= direction :before)
                            (r/gt start (r/get-field row :last-activity-ms)))
                     (r/and (= direction :after)
                            (r/le start (r/get-field row :last-activity-ms))))
               ;; Filter on the user's visibility map:
               (r/or ;; has :follow true
                     (-> row
                      (r/get-field [:user-visibility (keyword user-id) :follow])
                      (r/default false))
                     ;; has :unfollow false (key actually exists)
                     (-> row
                      (r/get-field [:user-visibility (keyword user-id) :unfollow])
                      (r/default true)
                      (r/not))))))
       ;; Sort
       (if-not count
        (r/order-by query (order-fn :sort-value))
        query)
       ;; Apply limit
       (if (pos? limit)
         (r/limit query limit)
         query)
       ;; Apply count if needed
       (if count (r/count query) query)
       ;; Run!
       (r/run query conn)
       ;; Drain cursor
       (if (= (type query) rethinkdb.net.Cursor)
         (seq query)
         query)))))

(defn last-entry-of-board
  [conn board-uuid]
  (as-> (r/table "entries") query
   (r/get-all query [board-uuid] {:index :board-uuid})
   (r/order-by query (r/desc :created-at))
   (r/default (r/nth query 0) {})
   (r/run query conn)))
