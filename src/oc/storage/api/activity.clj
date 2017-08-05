(ns oc.storage.api.activity
  "Liberator API for org resources."
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (if-let*)]
            [taoensso.timbre :as timbre]
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
            [oc.storage.resources.common :as common-res]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]
            [oc.storage.resources.entry :as entry-res]))

;; ----- Utility functions -----

(defn- assemble-activity
  "Assemble the requested activity (params) for the provided org."
  [conn {start :start direction :direction} org board-slugs-by-uuid]
  (let [order (if (= :after direction) :asc :desc)
        entries (cond

                  (= direction :around)
                  (concat
                    (reverse (entry-res/get-entries-by-org conn (:uuid org) :asc start :after))
                    (entry-res/get-entries-by-org conn (:uuid org) :desc start :before))
                  
                  (= order :asc) (reverse (entry-res/get-entries-by-org conn (:uuid org) order start direction))

                  :else (entry-res/get-entries-by-org conn (:uuid org) order start direction))]
    ;; Give each entry its board name
    (map #(assoc % :board-slug (board-slugs-by-uuid (:board-uuid %))) entries)))

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
                             boards (board-res/list-boards-by-org conn org-id [:created-at :updated-at :authors :viewers :access])
                             ;allowed-boards (map :uuid (filter #(access/access-level-for org % user) boards))
                             board-uuids (map :uuid boards)
                             board-slugs (map :slug boards)
                             board-slugs-by-uuid (zipmap board-uuids board-slugs)
                             activity (assemble-activity conn params org board-slugs-by-uuid)]
                          (activity-rep/render-activity-list params org activity (:access-level ctx) user-id))))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; All activity operations
      (OPTIONS "/orgs/:slug/activity" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (OPTIONS "/orgs/:slug/activity/" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (GET "/orgs/:slug/activity" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (GET "/orgs/:slug/activity/" [slug] (pool/with-pool [conn db-pool] (activity conn slug))))))