(ns oc.storage.api.digest
  "Liberator API for digest resource."
  (:require [clojure.walk :refer (keywordize-keys)]
            [if-let.core :refer (if-let*)]
            [compojure.core :as compojure :refer (OPTIONS GET)]
            [liberator.core :refer (defresource by-method)]
            [oc.lib.slugify :as slugify]
            [oc.lib.db.pool :as pool]
            [oc.lib.api.common :as api-common]
            [oc.storage.config :as config]
            [oc.storage.api.activity :as activity-api]
            [oc.storage.api.access :as access]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.digest :as digest-rep]
            [oc.storage.representations.board :as board-rep]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]
            [oc.storage.resources.entry :as entry-res]
            [oc.storage.urls.org :as org-urls]
            [oc.lib.time :as oc-time]
            [oc.lib.change.resources.read :as read]
            [clj-time.core :as clj-time]
            [clj-time.coerce :as clj-coerce]))

;; ----- Helpers -----

(defn digest-default-start []
  (oc-time/millis (clj-time/minus (clj-time/now) (clj-time/days 1))))

(defn assemble-digest
  "Assemble the requested (by the params) entries for the provided org to populate the digest response."
  [conn {start :start direction :direction limit :limit} org board-by-uuids allowed-boards user-id ctx]
  (let [follow-data (activity-api/follow-parameters-map user-id (:slug org))

        following-follow-data (assoc follow-data :following true)
        following-data (entry-res/paginated-entries-by-org conn (:uuid org) :desc start direction limit :recently-posted allowed-boards following-follow-data nil {})
        following-count (entry-res/paginated-entries-by-org conn (:uuid org) :desc start :before 0 :recently-posted allowed-boards following-follow-data nil {:count true})

        replies-data (entry-res/list-entries-for-user-replies conn (:uuid org) allowed-boards user-id :desc start direction limit follow-data nil {})
        replies-comments (filter #(and (contains? % :body) (not= (-> % :author :user-id) user-id))
                          (flatten (map :interactions replies-data)))
        replies-authors (group-by (comp :user-id :author) replies-comments)

        newly-created-boards (->> allowed-boards
                                  (map #(when-let [b (get board-by-uuids %)] b))
                                  (remove nil?)
                                  (filter #(pos? (compare (:created-at %) (oc-time/to-iso (clj-coerce/from-long start)))))
                                  (sort-by :name))

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
                        following-data))
     (assoc :replies {:comment-count (reduce
                                      (fn [partial-count comment]
                                        (let [entry-read-at (get-in user-reads-map [(:resource-uuid comment) :read-at])]
                                          (if (pos? (compare (:created-at comment) entry-read-at))
                                            (inc partial-count)
                                            partial-count)))
                                      0
                                      replies-comments)
                      :comment-authors (map (comp :author first second) replies-authors) ;; Get the first map of each group of authors
                      :entry-count (count replies-data)})
     (assoc :new-boards (map #(board-rep/render-board-for-collection (:slug org) % ctx) newly-created-boards)))))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource to retrieve the replies of a particular Org
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
  :malformed? (fn [ctx] (let [ctx-params (-> ctx :request :params keywordize-keys)
                              start (:start ctx-params)
                              ;; Start is always set for digest
                              valid-start? (try (Long. start) (catch java.lang.NumberFormatException _ false))
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
                             ctx-params (-> ctx :request :params keywordize-keys)
                             params (-> ctx-params
                                     (dissoc :slug)
                                     (update :start #(if % (Long. %) (digest-default-start)))  ; default is now
                                     (assoc :limit 10) ;; use a limit of 10 posts since digest can't be too long anyway
                                     (update :direction keyword)) ; always set to after)
                             boards (board-res/list-boards-by-org conn org-id [:created-at :access :authors :viewers :author :description])
                             allowed-boards (map :uuid (filter #(access/access-level-for org % user) boards))
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