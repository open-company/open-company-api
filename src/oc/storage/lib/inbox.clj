(ns oc.storage.lib.inbox
  "CRUD function to retrieve posts filtered for Inbox from RethinkDB."
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [rethinkdb.query :as r]
            [oc.lib.time :as lib-time]
            [oc.lib.db.common :as db-common]
            [oc.storage.config :as config]
            [oc.lib.time :as oc-time]))

(defn read-all-inbox-for-user
 [conn table-name index-name index-value order start direction relation-table-name allowed-boards user-id
  relation-fields {:keys [count] :or {count false}}]
 {:pre [(db-common/conn? conn)
        (db-common/s-or-k? table-name)
        (db-common/s-or-k? index-name)
        (or (string? index-value) (sequential? index-value))
        (db-common/s-or-k? relation-table-name)
        (#{:desc :asc} order)
        (not (nil? start))
        (#{:before :after} direction)
        (string? user-id)
        (sequential? relation-fields)
        (every? db-common/s-or-k? relation-fields)]}
  (let [index-values (if (sequential? index-value) index-value [index-value])
        order-fn (if (= order :desc) r/desc r/asc)
        filter-fn (if (= direction :before) r/gt r/lt)
        minimum-date-timestamp (f/unparse lib-time/timestamp-format (t/minus (t/now) (t/days config/inbox-days-limit)))]
    (db-common/with-timeout db-common/default-timeout
      (as-> (r/table table-name) query
            (r/get-all query index-values {:index index-name})
            ;; Merge in a last-activity-at date for each post (last comment created-at, fallback to published-at)
            (r/merge query (r/fn [post-row]
              {:last-activity-at (-> (r/table relation-table-name)
                                     (r/get-all [(r/get-field post-row :uuid)] {:index :resource-uuid})
                                     (r/filter (r/fn [interaction-row]
                                      ;; Filter out reactions and comments from the current user
                                      (r/and
                                       (r/ge (r/get-field interaction-row "body") "")
                                       (r/ne (r/get-field (r/get-field interaction-row "author") "user-id") user-id))))
                                     (r/coerce-to :array)
                                     (r/reduce (r/fn [left right]
                                       (if (r/ge (r/get-field left "created-at") (r/get-field right "created-at"))
                                         left
                                         right)))
                                     (r/default {"created-at" (r/get-field post-row "published-at")})
                                     (r/do (r/fn [interaction-row]
                                       (r/get-field interaction-row "created-at"))))}))
            ;; Filter out:
            (r/filter query (r/fn [post-row]
              (r/and ;; All records in boards the user has no access
                     (r/contains allowed-boards (r/get-field post-row :board-uuid))
                     ;; Leave in only posts whose last activity is within a certain amount of time
                     (r/gt (r/get-field post-row :last-activity-at) minimum-date-timestamp)
                     ;; All records with follow true
                     (r/not (r/default (r/get-field (r/get-field (r/get-field post-row :user-visibility) user-id) :unfollow) false))
                     ;; All records that have a dismiss-at later or equal than the last activity
                     (r/gt (r/get-field post-row :last-activity-at)
                           (r/default (r/get-field (r/get-field (r/get-field post-row :user-visibility) user-id) :dismiss-at) "")))))
            ;; Merge in all the interactions
            (if-not count
              (r/merge query (r/fn [post-row]
                {:interactions (-> (r/table relation-table-name)
                                   (r/get-all [(r/get-field post-row :uuid)] {:index :resource-uuid})
                                   (r/pluck relation-fields)
                                   (r/coerce-to :array))}))
              query)
            ;; Apply a filter on the published-at date 
            (r/filter query (r/fn [row]
                                  (filter-fn start (r/get-field row :published-at))))

            (if-not count (r/order-by query (order-fn :published-at)) query)
            ;; Apply count if needed
            (if count (r/count query) query)
            ;; Run!
            (r/run query conn)
            (if (= (type query) rethinkdb.net.Cursor)
              (seq query)
              query)))))