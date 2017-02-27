(ns oc.storage.api.boards
  "Liberator API for board resources."
  (:require [if-let.core :refer (if-let*)]
            [taoensso.timbre :as timbre]
            [compojure.core :as compojure :refer (defroutes ANY OPTIONS POST)]
            [liberator.core :refer (defresource by-method)]
            [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.slugify :as slugify]
            [oc.lib.db.pool :as pool]
            [oc.lib.api.common :as api-common]
            [oc.storage.config :as config]
            [oc.storage.api.access :as access]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.board :as board-rep]
            [oc.storage.representations.entry :as entry-rep]
            [oc.storage.resources.common :as common-res]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]
            [oc.storage.resources.entry :as entry-res]))

;; ----- Utility functions -----

(defn- assemble-board [conn org-slug slug ctx]
  (let [board (or (:updated-board ctx) (:existing-board ctx))
        topic-slugs (map name (:topics board)) ; slug for each active topic
        entries (entry-res/get-entries-by-board conn (:uuid board)) ; latest entry for each topic
        selected-entries (select-keys entries topic-slugs) ; active entries
        selected-entry-reps (zipmap topic-slugs
                              (map #(entry-rep/render-entry-for-collection org-slug slug
                                      (get selected-entries %) (:access-level ctx))
                                topic-slugs))
                                 archived-entries (clojure.set/difference (set (keys entries)) (set topic-slugs))
                                 archived (map #(identity {:slug % :title (:title (get entries %))}) archived-entries)
        authors (:authors board)
        author-reps (map #(board-rep/render-author-for-collection org-slug slug %) authors)
        viewers (:viewers board)
        viewer-reps (map #(board-rep/render-viewer-for-collection org-slug slug %) viewers)]
    (merge (-> board 
              (assoc :archived archived)
              (assoc :authors author-reps)
              (assoc :viewers viewer-reps))
      selected-entry-reps)))

;; ----- Validations -----

(defn- valid-new-board? [conn org-slug {board-map :data author :user}]
  (if-let [org (org-res/get-org conn org-slug)]
    (try
      {:new-board (board-res/->board (:uuid org) board-map author) :existing-org org}

      (catch clojure.lang.ExceptionInfo e
        [false, {:reason (.getMessage e)}])) ; Not a valid new board
    [false, {:reason :invalid-org}])) ; couldn't find the specified org

(defn- valid-board-update? [conn org-slug slug board-props]
  (if-let* [org (org-res/get-org conn org-slug)
            board (board-res/get-board conn (:uuid org) slug)]
    (let [updated-board (merge board (board-res/clean board-props))]
      (if (lib-schema/valid? common-res/Board updated-board)
        {:existing-org org :existing-board board :board-update updated-board}
        [false, {:board-update updated-board}])) ; invalid update
    true)) ; No org or board, so this will fail existence check later

;; ----- Actions -----

(defn- create-board [conn {new-board :new-board} org-slug]
  (timbre/info "Creating board for org:" org-slug)
  (if-let [board-result (board-res/create-board! conn new-board)] ; Add the board
    
    (do
      (timbre/info "Created board:" (:uuid board-result) "for org:" org-slug)
      {:new-board board-result})
    
    (do (timbre/error "Failed creating board for org:" org-slug) false)))

(defn- add-member
  "Add the specified author or viewer to the specified board."
  [conn ctx org-slug slug member-type user-id]
  (timbre/info "Adding" (str (name member-type) ":") user-id "to board:" slug "of org:" org-slug)
  (if-let* [member-fn (if (= member-type :authors)
                        board-res/add-author
                        board-res/add-viewer)
            updated-board (member-fn conn (-> ctx :existing-org :uuid) slug user-id)]
    (do
      (timbre/info "Added" (str (name member-type) ":") user-id "to board:" slug "of org:" org-slug)
      {:updated-board updated-board})
    
    (do
      (timbre/error "Failed adding" (str (name member-type) ":") user-id "to board:" slug "of org:" org-slug)
      false)))

(defn- remove-member
  "Remove the specified author or viewer from the specified board."
  [conn ctx org-slug slug member-type user-id]
  (timbre/info "Removing" (str (name member-type) ":") user-id "to board:" slug "of org:" org-slug)
  (if-let* [member-fn (if (= member-type :authors)
                        board-res/remove-author
                        board-res/remove-viewer)
            updated-board (member-fn conn (-> ctx :existing-org :uuid) slug user-id)]
    (do
      (timbre/info "Removed" (str (name member-type) ":") user-id "to board:" slug "of org:" org-slug)
      {:updated-board updated-board})
    
    (do
      (timbre/error "Failed removing" (str (name member-type) ":") user-id "to board:" slug "of org:" org-slug)
      false)))

(defn- update-board [conn ctx org-slug slug]
  (timbre/info "Updating board:" slug "of org:" org-slug)
  (if-let* [user (:user ctx)
            user-id (:user-id user)
            updated-board (:board-update ctx)
            updated-result (board-res/update-board! conn (:uuid updated-board) updated-board)]
    (do
      (timbre/info "Updated board:" slug "of org:" org-slug)
      (if (and (= "private" (:access updated-board)) ; board is being set private
               (nil? ((set (:authors updated-result)) user-id))) ; and current user is not an author
        (add-member conn ctx org-slug slug :authors user-id) ; make the current user an author
        {:updated-board updated-result}))

    (do (timbre/error "Failed updating board:" slug "of org:" org-slug) false)))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for operations on a particular board
(defresource board [conn org-slug slug]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :get :patch]

  ;; Media type client accepts
  :available-media-types [mt/board-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/board-media-type)

  ;; Media type client sends
  :known-content-type? (by-method {
    :options true
    :get true
    :patch (fn [ctx] (api-common/known-content-type? ctx mt/board-media-type))})
  
  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/access-level-for conn org-slug slug (:user ctx)))
    :patch (fn [ctx] (access/allow-authors conn org-slug slug (:user ctx)))})

  ;; Validations
  :processable? (by-method {
    :options true
    :get true
    :patch (fn [ctx] (valid-board-update? conn org-slug slug (:data ctx)))})

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [org (or (:existing-org ctx) (org-res/get-org conn org-slug))
                               board (or (:existing-board ctx) (board-res/get-board conn (:uuid org) slug))]
                        {:existing-org org :existing-board board}
                        false))

  ;; Actions
  :patch! (fn [ctx] (update-board conn ctx org-slug slug))

  ;; Responses
  :handle-ok (fn [ctx] (let [board (assemble-board conn org-slug slug ctx)]
                          (board-rep/render-board org-slug board (:access-level ctx))))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (schema/check common-res/Board (:board-update ctx)))))


;; A resource for operations on a list of boards
(defresource board-list [conn org-slug]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :post]

  ;; Media type client accepts
  :available-media-types [mt/board-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/board-media-type)
  
  ;; Media type client sends
  :known-content-type? (by-method {
                          :options true
                          :post (fn [ctx] (api-common/known-content-type? ctx mt/board-media-type))})

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  ;; Validations
  :processable? (by-method {
    :options true
    :post (fn [ctx] (valid-new-board? conn org-slug ctx))})

  ;; Actions
  :post! (fn [ctx] (create-board conn ctx org-slug))

  ;; Responses
  :handle-created (fn [ctx] (let [new-board (:new-board ctx)
                                  board-slug (:slug new-board)]
                              (api-common/location-response
                                (board-rep/url org-slug board-slug)
                                (board-rep/render-board org-slug new-board (:access-level ctx))
                                mt/board-media-type)))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (:reason ctx))))

;; A resource for the authors and viewers of a particular board
(defresource member [conn org-slug slug member-type user-id]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :post :delete]

  ;; Media type client accepts
  :available-media-types (if (= member-type :authors)
                            [mt/board-author-media-type]
                            [mt/board-viewer-media-type])
  :handle-not-acceptable (api-common/only-accept 406 (if (= member-type :authors)
                                                        mt/board-author-media-type
                                                        mt/board-viewer-media-type))
  ;; Media type client sends
  :malformed? (by-method {
    :options false
    :post (fn [ctx] (access/malformed-user-id? ctx))
    :delete false})
  :known-content-type? (by-method {
    :options true
    :post (fn [ctx] (api-common/known-content-type? ctx (if (= member-type :authors)
                                                            mt/board-author-media-type
                                                            mt/board-viewer-media-type)))
    :delete true})

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-authors conn org-slug slug (:user ctx)))
    :delete (fn [ctx] (access/allow-authors conn org-slug slug (:user ctx)))})

  ;; Existentialism
  :exists? (by-method {
    :post (fn [ctx] (if-let* [user-id (:data ctx)
                              org (and (slugify/valid-slug? org-slug) (org-res/get-org conn org-slug))
                              board (and (slugify/valid-slug? slug) (board-res/get-board conn (:uuid org) slug))]
                        {:existing-org org :existing-board board 
                         :existing? ((set (member-type board)) user-id)}
                        false))
    :delete (fn [ctx] (if-let* [org (and (slugify/valid-slug? org-slug) (org-res/get-org conn org-slug))
                                board (and (slugify/valid-slug? slug) (board-res/get-board conn (:uuid org) slug))
                                exists? ((set (member-type board)) user-id)] ; short circuits the delete w/ a 404
                        {:existing-org org :existing-board board :existing? true}
                        false))}) ; org or author doesn't exist

  ;; Actions
  :post! (fn [ctx] (when-not (:existing? ctx) (add-member conn ctx org-slug slug member-type (:data ctx))))
  :delete! (fn [ctx] (when (:existing? ctx) (remove-member conn ctx org-slug slug member-type user-id)))
  
  ;; Responses
  :respond-with-entity? false
  :handle-created (fn [ctx] (if (and (:existing-org ctx) (:existing-board ctx))
                              (api-common/blank-response)
                              (api-common/missing-response)))
  :handle-no-content (fn [ctx] (when-not (:updated-board ctx) (api-common/missing-response)))
  :handle-options (if user-id
                    (api-common/options-response [:options :delete])
                    (api-common/options-response [:options :post])))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; Board operations
      (ANY "/orgs/:org-slug/boards/:slug" [org-slug slug] (pool/with-pool [conn db-pool] (board conn org-slug slug)))
      ;; Board creation
      (OPTIONS "/orgs/:org-slug/boards/" [org-slug] (pool/with-pool [conn db-pool] (board-list conn org-slug)))
      (POST "/orgs/:org-slug/boards/" [org-slug] (pool/with-pool [conn db-pool] (board-list conn org-slug)))
      ;; Board author operations
      (ANY "/orgs/:org-slug/boards/:slug/authors" [org-slug slug]
        (pool/with-pool [conn db-pool] (member conn org-slug slug :authors nil)))
      (ANY "/orgs/:org-slug/boards/:slug/authors/" [org-slug slug]
        (pool/with-pool [conn db-pool] (member conn org-slug slug :authors nil)))
      (ANY "/orgs/:org-slug/boards/:slug/authors/:user-id" [org-slug slug user-id]
        (pool/with-pool [conn db-pool] (member conn org-slug slug :authors user-id)))
      ;; Board viewer operations
      (ANY "/orgs/:org-slug/boards/:slug/viewers" [org-slug slug]
        (pool/with-pool [conn db-pool] (member conn org-slug slug :viewers nil)))
      (ANY "/orgs/:org-slug/boards/:slug/viewers/" [org-slug slug]
        (pool/with-pool [conn db-pool] (member conn org-slug slug :viewers nil)))
      (ANY "/orgs/:org-slug/boards/:slug/viewers/:user-id" [org-slug slug user-id]
        (pool/with-pool [conn db-pool] (member conn org-slug slug :viewers user-id))))))