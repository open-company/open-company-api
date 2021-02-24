(ns oc.storage.api.digest
  "Liberator API for digest resource."
  (:require [if-let.core :refer (if-let*)]
            [compojure.core :as compojure :refer (OPTIONS GET)]
            [liberator.core :refer (defresource by-method)]
            [oc.lib.slugify :as slugify]
            [oc.lib.db.pool :as pool]
            [oc.lib.api.common :as api-common]
            [oc.storage.config :as config]
            [oc.storage.resources.activity :as activity-res]
            [oc.storage.api.access :as access]
            [oc.storage.resources.common :as common]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.digest :as digest-rep]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]
            [oc.storage.urls.org :as org-urls]
            [oc.lib.time :as oc-time]
            [oc.lib.change.resources.read :as read]
            [clj-time.core :as clj-time]))

;; ----- Helpers -----

(defn digest-default-start []
  (oc-time/to-iso (clj-time/minus (clj-time/now) (clj-time/days 1))))

(defn assemble-digest
  "Assemble the requested (by the params) entries for the provided org to populate the digest response."
  [conn {start :start direction :direction limit :limit} org board-by-uuids allowed-boards user-id ctx]
  (let [following-data (activity-res/paginated-entries-for-digest conn (:uuid org) :desc start direction limit allowed-boards {})
        following-count (activity-res/paginated-entries-for-digest conn (:uuid org) :desc start :before 0 allowed-boards {:count true})

        user-reads (read/retrieve-by-user-org config/dynamodb-opts user-id (:uuid org))
        user-reads-map (zipmap (map :item-uuid user-reads) user-reads)]
    ;; Give each activity its board name
    (-> {:start start
         :direction direction
         :total-following-count following-count}
     (assoc :following (map (fn [entry]
                              (let [board (board-by-uuids (:board-uuid entry))]
                                (merge entry {:board-slug (:slug board)
                                              :board-access (:access board)
                                              :board-name (:name board)
                                              :last-read-at (get-in user-reads-map [(:uuid entry) :read-at])})))
                        following-data)))))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource to retrieve the digest data of a particular Org
(defresource digest [conn slug]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :get]

  ;; Media type client accepts
  :available-media-types [mt/entry-collection-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/entry-collection-media-type)

  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/allow-members conn slug (:user ctx)))})

  ;; Check the request
  :malformed? (fn [ctx] (let [ctx-params (-> ctx :request :params)
                              start (:start ctx-params)
                              ;; Start is always set for digest
                              valid-start? (and (seq start) (common/sort-value? start))
                              direction (keyword (:direction ctx-params))
                              ;; direction is always set to after for digest
                              valid-direction? (= direction :after)]
                          (not (and valid-start? valid-direction?))))

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
                             ctx-params (-> ctx :request :params)
                             params (-> ctx-params
                                     (dissoc :slug)
                                     (assoc :limit 10) ;; use a limit of 10 posts since digest can't be too long anyway
                                     (update :direction keyword)) ; always set to after)
                             boards (board-res/list-boards-by-org conn org-id [:created-at :access :authors :viewers :author :description])
                             allowed-boards (filter #(access/access-level-for org % user) boards)
                             board-uuids (map :uuid boards)
                             board-by-uuids (zipmap board-uuids boards)
                             results (assemble-digest conn params org board-by-uuids allowed-boards user-id ctx)]
                          (digest-rep/render-digest params org "digest" results boards user))))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; Digest endpoint
      (OPTIONS (org-urls/digest ":slug") [slug] (pool/with-pool [conn db-pool] (digest conn slug)))
      (OPTIONS (str (org-urls/digest ":slug") "/") [slug] (pool/with-pool [conn db-pool] (digest conn slug)))
      (GET (org-urls/digest ":slug") [slug] (pool/with-pool [conn db-pool] (digest conn slug)))
      (GET (str (org-urls/digest ":slug") "/") [slug] (pool/with-pool [conn db-pool] (digest conn slug))))))