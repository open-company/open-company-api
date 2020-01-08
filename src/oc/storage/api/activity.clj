(ns oc.storage.api.activity
  "Liberator API for org resources."
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (if-let*)]
            [compojure.core :as compojure :refer (OPTIONS GET)]
            [liberator.core :refer (defresource by-method)]
            [clj-time.core :as t]
            [clj-time.format :as f]
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
            [oc.storage.resources.entry :as entry-res]
            [oc.storage.lib.timestamp :as ts]))

(defn- assemble-activity
  "Assemble the requested activity (params) for the provided org."
  [conn {start :start must-see :must-see digest-request :digest-request}
   org sort-type board-by-uuid allowed-boards user-id]
  (let [limit (if digest-request 0 config/default-activity-limit)
        entries (entry-res/paginated-entries-by-org conn (:uuid org) :desc start limit sort-type allowed-boards {:must-see must-see})
        activities {:next-count (count entries)
                    :activity entries}]
    ;; Give each activity its board name
    (update activities :activity #(map (fn [activity] (let [board (board-by-uuid (:board-uuid activity))]
                                                       (merge activity {
                                                        :board-slug (:slug board)
                                                        :board-access (:access board)
                                                        :board-name (:name board)})))
                                    %))))

(defn- assemble-follow-ups
  "Assemble the requested activity (params) for the provided org."
  [conn {start :start must-see :must-see} org sort-type board-by-uuid
   allowed-boards user-id]
  (let [entries (entry-res/list-all-entries-by-follow-ups conn (:uuid org) user-id :desc start config/default-activity-limit
                 sort-type allowed-boards {:must-see must-see})
        activities {:next-count (count entries)
                    :activity entries}]
    ;; Give each activity its board name
    (update activities :activity #(map (fn [activity] (let [board (board-by-uuid (:board-uuid activity))]
                                                       (merge activity {
                                                        :board-slug (:slug board)
                                                        :board-access (:access board)
                                                        :board-name (:name board)})))
                                    %))))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for operations on the activity of a particular Org
(defresource activity [conn slug]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :get]

  ;; Media type client accepts
  :available-media-types [mt/activity-collection-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/activity-collection-media-type)

  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/allow-members conn slug (:user ctx)))})

  ;; Check the request
  :malformed? (fn [ctx] (let [ctx-params (keywordize-keys (-> ctx :request :params))
                              start (:start ctx-params)
                              valid-start? (if start (ts/valid-timestamp? start) true)]
                          (not valid-start?)))

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [_slug? (slugify/valid-slug? slug)
                               org (or (:existing-org ctx) (org-res/get-org conn slug))]
                        {:existing-org (api-common/rep org)}
                        false))

  ;; Responses
  :handle-ok (fn [ctx] (let [user (:user ctx)
                             user-id (:user-id user)
                             org (:existing-org ctx)
                             org-id (:uuid org)
                             ctx-params (keywordize-keys (-> ctx :request :params))
                             sort (:sort ctx-params)
                             sort-type (if (= sort "activity") :recent-activity :recently-posted)
                             start? (if (:start ctx-params) true false) ; flag if a start was specified
                             params (update ctx-params :start #(or % (db-common/current-timestamp))) ; default is now
                             boards (board-res/list-boards-by-org conn org-id [:created-at :updated-at :authors :viewers :access])
                             allowed-boards (map :uuid (filter #(access/access-level-for org % user) boards))
                             board-uuids (map :uuid boards)
                             board-slugs-and-names (map #(array-map :slug (:slug %) :access (:access %) :name (:name %)) boards)
                             board-by-uuid (zipmap board-uuids board-slugs-and-names)
                             fixed-params (if (= (:auth-source user) "digest")
                                            (assoc params :digest-request true)
                                            params)
                             activity (assemble-activity conn fixed-params org sort-type board-by-uuid allowed-boards user-id)]
                          (activity-rep/render-activity-list params org "entries" sort-type activity boards user))))

;; A resource for operations on the activity of a particular Org
(defresource follow-ups [conn slug]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :get]

  ;; Media type client accepts
  :available-media-types [mt/activity-collection-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/activity-collection-media-type)

  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/allow-members conn slug (:user ctx)))})

  ;; Check the request
  :malformed? (fn [ctx] (let [ctx-params (keywordize-keys (-> ctx :request :params))
                              start (:start ctx-params)
                              valid-start? (if start (ts/valid-timestamp? start) true)]
                          (not valid-start?)))

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [_slug? (slugify/valid-slug? slug)
                               org (or (:existing-org ctx) (org-res/get-org conn slug))]
                        {:existing-org (api-common/rep org)}
                        false))

  ;; Responses
  :handle-ok (fn [ctx] (let [user (:user ctx)
                             user-id (:user-id user)
                             org (:existing-org ctx)
                             org-id (:uuid org)
                             ctx-params (keywordize-keys (-> ctx :request :params))
                             sort (:sort ctx-params)
                             sort-type (if (= sort "activity") :recent-activity :recently-posted)
                             start? (if (:start ctx-params) true false) ; flag if a start was specified
                             params (update ctx-params :start #(or % (db-common/current-timestamp))) ; default is now
                             boards (board-res/list-boards-by-org conn org-id [:created-at :updated-at :authors :viewers :access])
                             allowed-boards (map :uuid (filter #(access/access-level-for org % user) boards))
                             board-uuids (map :uuid boards)
                             board-slugs-and-names (map #(array-map :slug (:slug %) :access (:access %) :name (:name %)) boards)
                             board-by-uuid (zipmap board-uuids board-slugs-and-names)
                             activity (assemble-follow-ups conn params org sort-type board-by-uuid allowed-boards user-id)]
                          (activity-rep/render-activity-list params org "follow-ups" sort-type activity boards user))))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; All activity operations
      (OPTIONS "/orgs/:slug/entries" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (OPTIONS "/orgs/:slug/entries/" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (GET "/orgs/:slug/entries" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))
      (GET "/orgs/:slug/entries/" [slug] (pool/with-pool [conn db-pool] (activity conn slug)))

      (OPTIONS "/orgs/:slug/follow-ups" [slug] (pool/with-pool [conn db-pool] (follow-ups conn slug)))
      (OPTIONS "/orgs/:slug/follow-ups/" [slug] (pool/with-pool [conn db-pool] (follow-ups conn slug)))
      (GET "/orgs/:slug/follow-ups" [slug] (pool/with-pool [conn db-pool] (follow-ups conn slug)))
      (GET "/orgs/:slug/follow-ups/" [slug] (pool/with-pool [conn db-pool] (follow-ups conn slug))))))