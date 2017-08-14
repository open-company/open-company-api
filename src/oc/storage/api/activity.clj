(ns oc.storage.api.activity
  "Liberator API for org resources."
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (if-let*)]
            [compojure.core :as compojure :refer (OPTIONS GET)]
            [liberator.core :refer (defresource by-method)]
            [oc.lib.slugify :as slugify]
            [oc.lib.db.pool :as pool]
            [oc.lib.db.common :as db-common]
            [oc.lib.api.common :as api-common]
            [oc.storage.config :as config]
            [oc.storage.api.access :as access]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.activity :as activity-rep]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]
            [oc.storage.resources.entry :as entry-res]))

;; ----- Utility functions -----

(defn- assemble-activity
  "Assemble the requested activity (params) for the provided org."
  [conn {start :start direction :direction} org board-by-uuid]
  (let [order (if (= :after direction) :asc :desc)
        activity (cond

                  (= direction :around)
                  (let [previous-entries (entry-res/list-entries-by-org conn (:uuid org) :asc start :after)
                        next-entries (entry-res/list-entries-by-org conn (:uuid org) :desc start :before)]
                    {:direction :around
                     :previous-count (count previous-entries)
                     :next-count (count next-entries)
                     :entries (concat (reverse previous-entries) next-entries)})
                  
                  (= order :asc)
                  (let [previous-entries (entry-res/list-entries-by-org conn (:uuid org) order start direction)]
                    {:direction :previous
                     :previous-count (count previous-entries)
                     :entries (reverse previous-entries)})


                  :else
                  (let [next-entries (entry-res/list-entries-by-org conn (:uuid org) order start direction)]
                    {:direction :next
                     :next-count (count next-entries)
                     :entries next-entries}))]
    ;; Give each entry its board name
    (update activity :entries #(map (fn [entry] (merge entry {:board-slug (:slug (board-by-uuid (:board-uuid entry)))
                                                             :board-name (:name (board-by-uuid (:board-uuid entry)))}))
                                  %))))

(defn- assemble-calendar
  "
  Given a sequence of months, e.g. `[[2017 06] [2017 04] [2016 11] [2016 07] [2015 12]]`

  Return a map of the months by year, e.g. `{'2017' [[2017 06] [2017 04]]
                                             '2016' [[2016 11] [2016 07]]
                                             '2015' [[2015 12]]}`
  "
  [months]
  (let [years (distinct (map first months))
        months-by-year (map #(filter (fn [month] (= % (first month))) months) years)]
    (zipmap years months-by-year)))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for operations on the activity of a particular Org
(defresource activity [conn slug]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :get]

  ;; Media type client accepts
  :available-media-types [mt/activity-collection-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/activity-collection-media-type)

  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/access-level-for conn slug (:user ctx)))})

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [_slug? (slugify/valid-slug? slug)
                               org (or (:existing-org ctx) (org-res/get-org conn slug))]
                        {:existing-org org}
                        false))

  ;; Responses
  :handle-ok (fn [ctx] (let [user (:user ctx)
                             user-id (:user-id user)
                             org (:existing-org ctx)
                             org-id (:uuid org)
                             ctx-params (keywordize-keys (-> ctx :request :params))
                             start? (if (:start ctx-params) true false) ; flag if a start was specified
                             start-params (update ctx-params :start #(or % (db-common/current-timestamp))) ; default is now
                             direction (or (#{:after :around} (keyword (:direction ctx-params))) :before) ; default is before
                             ;; around is only valid with a specified start
                             allowed-direction (if (and (not start?) (= direction :around)) :before direction) 
                             params (merge start-params {:direction allowed-direction :start? start?})
                             boards (board-res/list-all-boards-by-org conn org-id [:created-at :updated-at :authors :viewers :access])
                             ;allowed-boards (map :uuid (filter #(access/access-level-for org % user) boards))
                             board-uuids (map :uuid boards)
                             board-slugs-and-names (map #(array-map :slug (:slug %) :name (:name %)) boards)
                             board-by-uuid (zipmap board-uuids board-slugs-and-names)
                             activity (assemble-activity conn params org board-by-uuid)]
                          (activity-rep/render-activity-list params org activity (:access-level ctx) user-id))))

;; A resource for operations on the calendar of activity for a particular Org
(defresource calendar [conn slug]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :get]

  ;; Media type client accepts
  :available-media-types [mt/activity-calendar-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/activity-calendar-media-type)

  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/access-level-for conn slug (:user ctx)))})

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [_slug? (slugify/valid-slug? slug)
                               org (or (:existing-org ctx) (org-res/get-org conn slug))]
                        {:existing-org org}
                        false))

  ;; Responses
  :handle-ok (fn [ctx] (let [user (:user ctx)
                             user-id (:user-id user)
                             org (:existing-org ctx)
                             org-id (:uuid org)
                             ;boards (board-res/list-all-boards-by-org conn org-id [:created-at :updated-at :authors :viewers :access])
                             ;allowed-boards (map :uuid (filter #(access/access-level-for org % user) boards))
                             ;board-uuids (map :uuid boards)
                             months (entry-res/entry-months-by-org conn org-id)
                             calendar-data (assemble-calendar months)]
                          (activity-rep/render-activity-calendar org calendar-data (:access-level ctx) user-id))))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; All activity operations
      (OPTIONS "/orgs/:slug/activity" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (OPTIONS "/orgs/:slug/activity/" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (GET "/orgs/:slug/activity" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (GET "/orgs/:slug/activity/" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      ;; Calendar of activity operations
      (OPTIONS "/orgs/:slug/activity/calendar" [slug] (pool/with-pool [conn db-pool] (calendar conn slug)))
      (OPTIONS "/orgs/:slug/activity/calendar/" [slug] (pool/with-pool [conn db-pool] (calendar conn slug)))
      (GET "/orgs/:slug/activity/calendar" [slug] (pool/with-pool [conn db-pool] (calendar conn slug)))
      (GET "/orgs/:slug/activity/calendar/" [slug] (pool/with-pool [conn db-pool] (calendar conn slug))))))