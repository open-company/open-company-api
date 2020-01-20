(ns oc.storage.db.common
  "RethinkDB fns specific to storage service."
  (:require [rethinkdb.query :as r]
            [oc.lib.db.common :as db-common]))

(defn read-paginated-entries
  "CRUD function to retrieve entries from RethinkDB with pagination."
  [conn table-name index-name index-value order start direction limit sort-type relation-table-name allowed-boards
   user-id relation-fields {:keys [count] :or {count false}}]
  {:pre [(db-common/conn? conn)
         (db-common/s-or-k? table-name)
         (db-common/s-or-k? index-name)
         (or (string? index-value) (sequential? index-value))
         (db-common/s-or-k? relation-table-name)
         (#{:desc :asc} order)
         (not (nil? start))
         (#{:after :before} direction)
         (integer? limit)
         (#{:recent-activity :recently-posted} sort-type)
         (sequential? relation-fields)
         (every? db-common/s-or-k? relation-fields)]}
  (let [index-values (if (sequential? index-value) index-value [index-value])
        order-fn (if (= order :desc) r/desc r/asc)]
    (db-common/with-timeout db-common/default-timeout
      (as-> (r/table table-name) query
            (r/get-all query index-values {:index index-name})
            ;; Merge in a last-activity-at date for each entry, which is the
            ;; last comment created-at, with fallback to published-at or created-at
            ;; for published entries, and the entry created-at in all the other cases.
            (r/merge query (r/fn [post-row]
              (if (= sort-type :recent-activity)
                {:last-activity-at (-> (r/table relation-table-name)
                                       (r/get-all [(r/get-field post-row :uuid)] {:index :resource-uuid})
                                       (r/filter (r/fn [interaction-row]
                                        ;; Filter out reactions and comments from the current user
                                        (if (seq user-id)
                                          (r/and
                                           (r/ge (r/get-field interaction-row "body") "")
                                           (r/ne (r/get-field (r/get-field interaction-row "author") "user-id") user-id))
                                          (r/ge (r/get-field interaction-row "body") ""))))
                                       (r/order-by (r/desc "created-at"))
                                       (r/coerce-to :array)
                                       (r/nth 0)
                                       (r/default (r/fn [_err]
                                        {"created-at" (r/default
                                                       (r/get-field post-row "published-at")
                                                       (r/get-field post-row "created-at"))}))
                                       (r/do (r/fn [interaction-row]
                                         (r/get-field interaction-row "created-at"))))}
                {:last-activity-at (r/default
                                    (r/get-field post-row "published-at")
                                    (r/get-field post-row "created-at"))})))
            (if (sequential? allowed-boards)
              ;; Filter out entries that don't belong
              (r/filter query (r/fn [post-row]
                (r/and ;; All entries in boards the user has no access
                       (r/contains allowed-boards (r/get-field post-row :board-uuid))
                       ;; All entries after/before the start
                       (if (= direction :before)
                         (r/gt start (r/get-field post-row :last-activity-at))
                         (r/le start (r/get-field post-row :last-activity-at))))))
              ;; Filter entries based on the date
              (r/filter query (r/fn [post-row]
                (if (= direction :before)
                  (r/gt start (r/get-field post-row :last-activity-at))
                  (r/le start (r/get-field post-row :last-activity-at))))))
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
            ;; Let's finally run this bad boy!
            (r/run query conn)
            (if (= (type query) rethinkdb.net.Cursor)
              (seq query)
              query)))))