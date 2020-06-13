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
   relation-fields {:keys [count] :or {count false}}]
 (read-paginated-entries conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
  nil relation-fields nil {:count count}))

 ([conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
   relation-fields user-id {:keys [count] :or {count false}}]
  (read-paginated-entries conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
  nil relation-fields user-id {:count count}))

 ([conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
  follow-data relation-fields user-id {:keys [count] :or {count false}}]
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
        (sequential? relation-fields)
        (every? db-common/s-or-k? relation-fields)]}
  (let [index-values (if (sequential? index-value) index-value [index-value])
        order-fn (if (= order :desc) r/desc r/asc)]
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
            ;; Merge in a last-activity-at date for each post, which is the
            ;; last comment created-at, with fallback to published-at or created-at for published entries
            ;; the entry created-at in all the other cases.
            (r/merge query (r/fn [post-row]
              (cond
                (= sort-type :recent-activity)
                {:last-activity-at (-> (r/table relation-table-name)
                                       (r/get-all [[(r/get-field post-row :uuid) true]] {:index :resource-uuid-comment})
                                       (r/max :created-at)
                                       (r/get-field :created-at)
                                       (r/default (r/get-field post-row :published-at))
                                       (r/default (r/get-field post-row :created-at)))}
                (= sort-type :bookmarked-at)
                {:last-activity-at (r/default
                                    (-> (r/get-field post-row [:bookmarks])
                                        (r/filter {:user-id user-id})
                                        (r/nth 0)
                                        (r/get-field :bookmarked-at))
                                    (r/get-field post-row :published-at))}
                :else
                {:last-activity-at (r/default
                                    (r/get-field post-row :published-at)
                                    (r/get-field post-row :created-at))})))
            ;; Filter out:
            (r/filter query (r/fn [post-row]
              ;; All records after/before the start
              (if (= direction :before)
                (r/gt start (r/get-field post-row :last-activity-at))
                (r/le start (r/get-field post-row :last-activity-at)))))
            ;; Merge in all the interactions
            (if-not count
              (r/merge query (r/fn [post-row]
                {:interactions (-> (r/table relation-table-name)
                                   (r/get-all [(r/get-field post-row :uuid)] {:index :resource-uuid})
                                   (r/pluck relation-fields)
                                   (r/coerce-to :array))}))
              query)
            (if-not count (r/order-by query (order-fn :last-activity-at)) query)
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
              {:last-activity-at (-> (r/table relation-table-name)
                                     (r/get-all [[(r/get-field post-row :uuid) true]] {:index :resource-uuid-comment})
                                     (r/filter (r/fn [interaction-row]
                                       (r/ne (r/get-field interaction-row [:author :user-id]) user-id)))
                                     (r/max :created-at)
                                     (r/get-field [:created-at])
                                     (r/default (r/get-field post-row :published-at))
                                     (r/default (r/get-field post-row :created-at)))}))
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
             (r/gt start (r/get-field row :last-activity-at))))
            ;; Sort records when not counting
            (if-not count (r/order-by query (order-fn :last-activity-at)) query)
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

(defn read-paginated-entries-for-replies
  "Read all entries with at least one comment the user has access to. Filter out those not activily followed
   by the current user. Sort those with unread content at the top and sort everything by last activity descendant."
  [conn org-uuid allowed-boards user-id order start direction limit follow-data read-items {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)
         (lib-schema/unique-id? org-uuid)
         (or (sequential? allowed-boards)
             (nil? allowed-boards))
         (lib-schema/unique-id? user-id)
         (or (nil? follow-data)
             (map? follow-data))
         (or (nil? read-items)
             (coll? read-items))
         (#{:desc :asc} order)
         (not (nil? start))
         (#{:after :before} direction)
         (or (zero? limit) ;; means all
             (pos? limit))
         (boolean? count)]}
  (let [order-fn (if (= order :desc) r/desc r/asc)
        unread-cap-ms (if (zero? config/replies-unread-cap-days)
                        (* 60 60 24 365 50 1000) ;; 50 years cap
                        (* 60 60 24 config/replies-unread-cap-days 1000))
        read-items-map (r/coerce-to (zipmap (map :item-id read-items) (map :read-at read-items)) :object)]
    (db-common/with-timeout db-common/default-timeout
      (as-> (r/table "entries") query
       (r/get-all query [[:published org-uuid]] {:index :status-org-uuid})
       ;; Make an initial filter to select only posts the user has access to
       (r/filter query (r/fn [row]
        (r/and (r/or (not (sequential? allowed-boards))
                     (r/contains allowed-boards (r/get-field row :board-uuid)))
               (r/or (not (map? follow-data))
                     ;; filter on not followed boards
                     (r/not (r/contains (vec (:unfollow-board-uuids follow-data)) (r/get-field row :board-uuid)))))))
       ;; Merge in last-activity-at and entry
       (r/merge query (r/fn [row]
        (let [interactions-base (-> (r/table "interactions")
                                 (r/get-all [(r/get-field row :uuid)] {:index :resource-uuid}))
              last-activity-at (-> interactions-base
                                 (r/max :created-at)
                                 (r/get-field :created-at)
                                 (r/default (r/get-field row [:published-at])))
              sort-value-base (-> last-activity-at
                               (r/iso8601)
                               (r/to-epoch-time)
                               (r/mul 1000)
                               (r/round))
              unread-thread? (r/or (r/not (r/contains (r/keys read-items-map) (r/get-field row :uuid)))
                                   (r/gt (-> last-activity-at (r/iso8601) (r/to-epoch-time) (r/mul 1000) (r/round))
                                         (-> (r/get-field row :uuid)
                                          (as-> x (r/get-field read-items-map x))
                                          (r/iso8601)
                                          (r/to-epoch-time)
                                          (r/mul 1000)
                                          (r/round))))
              unread-with-cap? (r/and unread-thread?
                                      (r/gt (-> last-activity-at (r/iso8601) (r/to-epoch-time) (r/mul 1000) (r/round))
                                            (-> (r/now) (r/to-epoch-time) (r/mul 1000) (r/round) (r/sub unread-cap-ms))))]
          {;; Date of the last added comment on this thread
           :last-activity-at last-activity-at
           :comments-count (r/count interactions-base)
           :last-read-at (r/branch (r/contains (r/keys read-items-map) (r/get-field row :uuid))
                           (r/get-field read-items-map (r/get-field row :uuid))
                           nil)
           :sort-value (r/branch unread-with-cap?
                         ;; If the item is unread and was published in the cap window
                         ;; let's add the cap window to the publish timestamp so it will sort before the read items
                         (r/add sort-value-base unread-cap-ms)
                         ;; The timestamp in seconds
                         sort-value-base)
           :debug {:unread-with-cap unread-with-cap?
                   :unread-thread unread-thread?}})))
       ;; Filter by user-visibility
       (r/filter query (r/fn [row]
        (r/and (r/gt (r/get-field row [:comments-count]) 0)
               ;; Filter out threads that have comments only from the current user
               (r/not (r/and (r/eq (r/get-field row [:author :user-id]) user-id)
                             (-> (r/get-field row [:reply-authors])
                               (r/filter user-id)
                               (r/is-empty))))
               ;; All records after/before the start
               (r/or (r/and (= direction :before)
                            (r/gt start (r/get-field row :sort-value)))
                     (r/and (= direction :after)
                            (r/le start (r/get-field row :sort-value))))
               ;; Filter on the user-visibility map
               (r/or (r/and (-> row
                             (r/get-field [:user-visibility (keyword user-id)])
                             (r/has-fields [:follow]))
                            (r/get-field row [:user-visibility (keyword user-id) :follow]))
                     (r/and (-> row
                             (r/get-field [:user-visibility (keyword user-id)])
                             (r/has-fields [:unfollow]))
                            (r/not (r/get-field row [:user-visibility (keyword user-id) :unfollow])))))))
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