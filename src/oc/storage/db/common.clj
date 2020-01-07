(ns oc.storage.db.common
  "CRUD function to retrieve entries from RethinkDB with pagination."
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [rethinkdb.query :as r]
            [oc.lib.time :as lib-time]
            [oc.lib.db.common :as db-common]
            [oc.storage.config :as config]
            [oc.lib.time :as oc-time]))

(defn read-paginated-entries
 [conn table-name index-name index-value order start limit sort-type relation-table-name allowed-boards user-id
  relation-fields {:keys [count] :or {count false}}]
 {:pre [(db-common/conn? conn)
        (db-common/s-or-k? table-name)
        (db-common/s-or-k? index-name)
        (or (string? index-value) (sequential? index-value))
        (db-common/s-or-k? relation-table-name)
        (#{:desc :asc} order)
        (not (nil? start))
        (integer? limit)
        (#{:recent-activity :recently-posted} sort-type)
        (sequential? relation-fields)
        (every? db-common/s-or-k? relation-fields)]}
  (let [index-values (if (sequential? index-value) index-value [index-value])
        order-fn (if (= order :desc) r/desc r/asc)]
    (db-common/with-timeout db-common/default-timeout
      (as-> (r/table table-name) query
            (r/get-all query index-values {:index index-name})
            ;; Merge in a last-activity-at date for each post, which is the
            ;; last comment created-at, with fallback to published-at or created-at for published entries
            ;; the entry created-at in all the other cases.
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
                                       (r/coerce-to :array)
                                       (r/reduce (r/fn [left right]
                                         (if (r/ge (r/get-field left "created-at") (r/get-field right "created-at"))
                                           left
                                           right)))
                                       (r/default (r/fn [_err]
                                        {"created-at" (if (r/eq (r/get-field post-row "status") "published")
                                                        (r/get-field post-row "published-at")
                                                        (r/get-field post-row "created-at"))}))
                                       (r/do (r/fn [interaction-row]
                                         (r/get-field interaction-row "created-at"))))}
                {:last-activity-at (r/get-field post-row :published-at)})))
            (if (sequential? allowed-boards)
              ;; Filter out:
              (r/filter query (r/fn [post-row]
                (r/and ;; All records in boards the user has no access
                       (r/contains allowed-boards (r/get-field post-row :board-uuid))
                       ;; All records after the start
                       (r/gt start (r/get-field post-row :last-activity-at)))))
              (r/filter query (r/fn [post-row]
                (r/gt start (r/get-field post-row :last-activity-at)))))
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
              query)))))