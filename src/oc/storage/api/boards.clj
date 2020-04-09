(ns oc.storage.api.boards
  "Liberator API for board resources."
  (:require [if-let.core :refer (if-let*)]
            [defun.core :refer (defun-)]
            [taoensso.timbre :as timbre]
            [compojure.core :as compojure :refer (defroutes ANY OPTIONS POST)]
            [liberator.core :refer (defresource by-method)]
            [clojure.walk :refer (keywordize-keys)]
            [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.slugify :as slugify]
            [oc.lib.db.pool :as pool]
            [oc.lib.api.common :as api-common]
            [oc.lib.db.common :as db-common]
            [oc.storage.config :as config]
            [oc.storage.api.access :as access]
            [oc.storage.api.entries :as entries-api]
            [oc.storage.async.notification :as notification]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.board :as board-rep]
            [oc.storage.resources.common :as common-res]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]
            [oc.storage.resources.entry :as entry-res]
            [oc.storage.lib.timestamp :as ts]
            [oc.storage.urls.board :as board-url]
            [oc.lib.change.resources.follow :as follow]))

;; ----- Utility functions -----

(defn- default-board-params []
  {:sort-type :recent-activity
   :start (db-common/current-timestamp)
   :direction :before})

(defun- assemble-board
  "Assemble the entry, author, and viewer data needed for a board response."

  ;; Draft board
  ([conn org :guard map? board :guard #(or (:draft %) (= (:slug %) (:slug board-res/default-drafts-board))) ctx]
  (let [org-slug (:slug org)
        slug (:slug board)
        all-drafts (entry-res/list-drafts-by-org-author conn (:uuid org) (-> ctx :user :user-id) {})
        entries (if (:draft board)
                  (filterv #(= (:board-uuid %) (:uuid board)) all-drafts)
                  all-drafts)
        sorted-entries (reverse (sort-by :updated-at entries))]
    (merge board {:entries sorted-entries})))

  ;; Regular paginated board
  ([conn org :guard map? board :guard map? params :guard map? ctx]
  (let [{start :start direction :direction must-see :must-see sort-type :sort-type} params
        access-level (:access-level ctx)
        user-id (-> ctx :user :user-id)
        order (if (= direction :before) :desc :asc)
        entries (entry-res/paginated-entries-by-board conn (:uuid board) order start direction
                 config/default-activity-limit sort-type {:must-see must-see :status :all})]
    ;; Give each activity its board name
    (merge board {:next-count (count entries)
                  :direction direction
                  :entries entries}))))

;; ----- Validations -----

(defn- valid-entry-with-board?
  [conn entry author]
  (if-let* [status (:status entry)
            entry-uuid (:uuid entry)
            found-entry (entry-res/get-entry conn entry-uuid)]
    (merge found-entry (assoc entry :status status))
    (let [clean-entry (dissoc entry :publisher-board)]
      (entry-res/->entry conn entry-res/temp-uuid clean-entry author))))

(defn- valid-new-board? [conn org-slug {board-map :data author :user}]
  (if-let [org (org-res/get-org conn org-slug)]
    (try
      (let [notifications (:private-notifications board-map)
            entry-data (map #(valid-entry-with-board? conn % author)
                          (:entries board-map))
            board-data (-> board-map
                        (dissoc :private-notifications :note :pre-flight :exclude)
                        (assoc :entries entry-data))]
        (if (and (:disallow-public-board (or (:content-visibility org) {}))
                 (= (:access board-data) "public"))
          [false, {:reason :disallowed-public-board}]
          {:new-board (api-common/rep (board-res/->board (:uuid org) board-data author))
           :existing-org (api-common/rep org)
           :notifications (api-common/rep notifications)}))

      (catch clojure.lang.ExceptionInfo e
        [false, {:reason (.getMessage e)}])) ; Not a valid new board
    [false, {:reason :invalid-org}])) ; couldn't find the specified org

(defn- valid-board-update? [conn org-slug slug board-props]
  (if-let* [org (org-res/get-org conn org-slug)
            board (board-res/get-board conn (:uuid org) slug)
            board-data (dissoc board-props :private-notifications :note)]
    (if (and (:disallow-public-board (or (:content-visibility org) {}))
             (not= (:access board) "public")
             (= (:access board-data) "public"))
      [false, {:reason :disallowed-public-board}]
      (let [updated-board (merge board (board-res/clean board-data))]
        (if (lib-schema/valid? common-res/Board updated-board)
          {:existing-org (api-common/rep org)
           :existing-board (api-common/rep board)
           :board-update (api-common/rep updated-board)
           :notifications (api-common/rep (:private-notifications board-props))}
          [false, {:board-update (api-common/rep updated-board)}]))) ; invalid update
    true)) ; No org or board, so this will fail existence check later

;; ----- Actions -----

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
      {:updated-board (api-common/rep updated-board)})
    
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
      {:updated-board (api-common/rep updated-board)})
    
    (do
      (timbre/error "Failed removing" (str (name member-type) ":") user-id "to board:" slug "of org:" org-slug)
      false)))

(defn- create-board [conn {org :existing-org new-board :new-board user :user :as ctx} org-slug]
  (timbre/info "Creating board for org:" org-slug)
  (let [entries (:entries new-board)
        draft-board? (and (pos? (count entries)) (every? #(-> % :status keyword (= :draft)) entries))
        new-board-data (assoc new-board :draft draft-board?)]
    (timbre/info "Creating board, is draft?" draft-board?)
     (if-let [board-result (if (:publisher-board new-board-data)
                            (entries-api/create-publisher-board conn org user)
                            (board-res/create-board! conn new-board-data))] ; Add the board

      (let [board-uuid (:uuid board-result)
            authors (-> ctx :data :authors)
            viewers (-> ctx :data :viewers)
            invitation-note (-> ctx :data :note)
            notifications (:notifications ctx)]
        (timbre/info "Created board:" board-uuid "for org:" org-slug)
        ;; Add any authors specified in the request
        (doseq [author authors] (add-member conn ctx (:slug org) (:slug board-result) :authors author))
        ;; Add any viewers specified in the request
        (doseq [viewer viewers] (add-member conn ctx (:slug org) (:slug board-result) :viewers viewer))
        ;; Add any entries specified in the request
        (doseq [entry entries]
          (let [old-board (when (and (:uuid entry) (:board-uuid entry))
                            (board-res/get-board conn (:board-uuid entry)))
                fixed-entry (-> entry
                             (assoc :board-uuid board-uuid)
                             (dissoc :publisher-board)
                             (update :user-visibility #(if (and old-board
                                                                (not= (:publisher-board old-board) (:publisher-board board-result)))
                                                         (entry-res/update-user-visibility-for-move entry
                                                          (when (:publisher-board board-result)
                                                           (follow/retrieve config/dynamodb-opts (:user-id user) (:slug org)))
                                                          (:publisher-board board-result))
                                                         %)))
                entry-action (if (entry-res/get-entry conn (:uuid entry))
                               :update
                               :add)
                new-entry (if (= entry-action :update)
                            fixed-entry
                            (entry-res/->entry conn board-uuid fixed-entry user))]
            (timbre/info "Upserting entry for new board:" board-uuid)
            (let [entry-result (entry-res/upsert-entry! conn new-entry user)]

              ;; Now publish all the entries that are not published already
              (if (and (= entry-action :update)
                       (= (keyword (:status new-entry)) :published)
                       (not (:published-at new-entry)))
                
                (do
                  (entry-res/publish-entry! conn (:uuid new-entry) org user)
                  (timbre/info "Upserted and published entry for new board:" board-uuid "as" (:uuid entry-result)))
                
                (timbre/info "Upserted entry for new board:" board-uuid "as" (:uuid entry-result)))

              ;; If we are updating an existing draft check if we need to remove the old board
              (when (not= (:board-uuid entry) entry-res/temp-uuid)
                (let [old-board (board-res/get-board conn (:board-uuid entry))
                      remaining-entries (entry-res/list-all-entries-by-board conn (:uuid old-board))]
                  (board-res/maybe-delete-draft-board conn org old-board remaining-entries user)))

              (when (= (:status entry-result) "published")
                (when (= :add entry-action)
                  (entries-api/auto-share-on-publish conn (assoc ctx :existing-board board-result) entry-result))
                (notification/send-trigger! (notification/->trigger entry-action org board-result {:new entry-result} (:user ctx) nil))))))
        (let [created-board (if (and (empty? authors) (empty? viewers))
                              ;; no additional members added, so using the create response is good
                              board-result
                              ;; retrieve the board again to get final list of members
                              (board-res/get-board conn (:uuid board-result)))]
          (notification/send-trigger! (notification/->trigger :add org {:new created-board :notifications notifications} user invitation-note))
          {:created-board (api-common/rep (assemble-board conn org created-board (default-board-params) ctx))}))
    
    (do (timbre/error "Failed creating board for org:" org-slug) false))))

(defn- update-board [conn ctx org-slug slug]
  (timbre/info "Updating board:" slug "of org:" org-slug)
  (if-let* [user (:user ctx)
            user-id (:user-id user)
            org (:existing-org ctx)
            board (:existing-board ctx)
            updated-board (:board-update ctx)
            updated-result (board-res/update-board! conn (:uuid updated-board) updated-board)]
    (let [notifications (:notifications ctx)
          current-authors (set (:authors updated-result))
          current-viewers (set (:viewers updated-result))
          new-authors (-> ctx :data :authors)
          new-viewers (-> ctx :data :viewers)
          invitation-note  (-> ctx :data :note)]
      (timbre/info "Updated board:" slug "of org:" org-slug)
      (when (= "private" (:access updated-board)) ; board is being set private
        ;; Ensure current user is author
        (when (nil? (current-authors user-id)) ; and current user is not an author
          (add-member conn ctx org-slug slug :authors user-id)) ; make the current user an author
        ;; If authors are specified, make any requested author changes as a "sync"
        (when new-authors
          (doseq [author (clojure.set/difference (set new-authors) current-authors)]
            (add-member conn ctx (:slug org) (:slug updated-result) :authors author))
          (doseq [author (clojure.set/difference current-authors (set new-authors))]
            (remove-member conn ctx (:slug org) (:slug updated-result) :authors author)))
        ;; If viewers are specified, make any requested viewer changes as a "sync"
        (when new-viewers
          (doseq [viewer (clojure.set/difference (set new-viewers) current-viewers)]
            (add-member conn ctx (:slug org) (:slug updated-result) :viewers viewer))
          (doseq [viewer (clojure.set/difference current-viewers (set new-viewers))]
            (remove-member conn ctx (:slug org) (:slug updated-result) :viewers viewer))))
      (let [final-result (board-res/get-board conn (:uuid updated-result))]
        (notification/send-trigger! (notification/->trigger :update org {:old board :new final-result :notifications notifications} user invitation-note))
        {:updated-board (api-common/rep final-result)}))

    (do (timbre/error "Failed updating board:" slug "of org:" org-slug) false)))

(defn- delete-board [conn ctx org-slug slug]
  (timbre/info "Deleting board:" slug "of org:" org-slug)
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            entries (entry-res/list-all-entries-by-board conn (:uuid board))
            _delete-result (board-res/delete-board! conn (:uuid board) entries)]
    (do 
      (timbre/info "Deleted board:" slug "of org:" org-slug)
      (notification/send-trigger! (notification/->trigger :delete org {:old board} (:user ctx)))
      true)
    (do (timbre/warn "Failed deleting board:" slug "of org:" org-slug) false)))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for operations on a particular board
(defresource board [conn org-slug slug]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :get :patch :delete]

  ;; Media type client accepts
  :available-media-types [mt/board-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/board-media-type)

  ;; Media type client sends
  :known-content-type? (by-method {
    :options true
    :get true
    :patch (fn [ctx] (api-common/known-content-type? ctx mt/board-media-type))
    :delete true})
  
  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/access-level-for conn org-slug slug (:user ctx)))
    :patch (fn [ctx] (access/allow-authors conn org-slug slug (:user ctx)))
    :delete (fn [ctx] (access/allow-authors conn org-slug slug (:user ctx)))})

  :malformed? (by-method {
    :options false
    :get (fn [ctx] (let [ctx-params (keywordize-keys (-> ctx :request :params))
                         start (:start ctx-params)
                         valid-start? (if start (ts/valid-timestamp? start) true)
                         valid-sort? (or (not (contains? ctx-params :sort))
                                         (= (:sort ctx-params) "activity"))
                         direction (keyword (:direction ctx-params))
                         ;; no direction is OK, but if specified it's from the allowed enumeration of options
                         valid-direction? (if direction (#{:before :after} direction) true)
                         ;; a specified start/direction must be together or ommitted
                         pairing-allowed? (or (and start direction)
                                              (and (not start) (not direction)))]
                     (not (and valid-start? valid-sort? valid-direction? pairing-allowed?))))
    :patch (fn [ctx] (api-common/malformed-json? ctx))
    :delete false})

  ;; Validations
  :processable? (by-method {
    :options true
    :get true
    :patch (fn [ctx] (and (slugify/valid-slug? org-slug)
                          (slugify/valid-slug? slug)
                          (valid-board-update? conn org-slug slug (:data ctx))))
    :delete true})

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [_slugs? (and (slugify/valid-slug? org-slug) (slugify/valid-slug? slug))
                               org (or (:existing-org ctx) (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)
                               board (or (:existing-board ctx)
                                         (if (and (= slug (:slug board-res/default-drafts-board))
                                                  (lib-schema/valid? lib-schema/User (:user ctx)))
                                            ;; Draft board for the user
                                            (board-res/drafts-board org-uuid (:user ctx))
                                            ;; Regular board by slug
                                            (board-res/get-board conn org-uuid slug)))
                               boards (board-res/list-boards-by-org conn org-uuid)
                               boards-map (zipmap (map :uuid boards) boards)]
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing-org-boards (api-common/rep boards-map)}
                        false))

  ;; Actions
  :patch! (fn [ctx] (update-board conn ctx org-slug slug))
  :delete! (fn [ctx] (delete-board conn ctx org-slug slug))
  
  ;; Responses
  :handle-ok (fn [ctx] (let [org (:existing-org ctx)
                             board (or (:updated-board ctx) (:existing-board ctx))
                             ctx-params (keywordize-keys (-> ctx :request :params))
                             sort (:sort ctx-params)
                             sort-type (if (= sort "activity") :recent-activity :recently-posted)
                             start-params (update ctx-params :start #(or % (db-common/current-timestamp))) ; default is now
                             direction (or (#{:after} (keyword (:direction ctx-params))) :before) ; default is before
                             drafts-board? (= (:slug board) (:slug board-res/default-drafts-board))
                             ;; For drafts board don't use parameters
                             params (when-not drafts-board?
                                      (merge start-params {:direction direction :sort-type sort-type}))
                             full-board (if drafts-board?
                                          (assemble-board conn org board ctx)
                                          (assemble-board conn org board params ctx))]
                           (board-rep/render-board org full-board ctx params)))
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
    :post (fn [ctx] (and (slugify/valid-slug? org-slug)
                         (valid-new-board? conn org-slug ctx)))})
  :conflict? (fn [ctx] (let [data (:data ctx)
                             new-slug (-> ctx :new-board :slug) ; proposed new slug
                             ;; some new slugs can be excluded from being checked during a pre-flight check only
                             ;; right now, these prevent us from denying the UI a name update back to the current name
                             exclude-slugs (when (:pre-flight data) (set (map slugify/slugify (:exclude data))))
                             excluded? (when exclude-slugs (exclude-slugs new-slug))] ; excluded slugs don't need checked
                        (not (or excluded?
                                 (board-res/slug-available? conn (org-res/uuid-for conn org-slug) new-slug)))))

  ;; Actions
  :post! (fn [ctx] (if (-> ctx :data :pre-flight)
                        true ; we were just checking if this would work
                        (create-board conn ctx org-slug)))

  ;; Responses
  :handle-created (fn [ctx] (let [pre-flight? (-> ctx :data :pre-flight)
                                  org (:existing-org ctx)
                                  new-board (:created-board ctx)
                                  board-slug (:slug new-board)]
                              (if pre-flight?
                                (api-common/blank-response)
                                (api-common/location-response
                                  (board-url/url org-slug board-slug)
                                  (board-rep/render-board org new-board ctx (default-board-params))
                                  mt/board-media-type))))
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
    :post (fn [ctx] (if-let* [_slugs? (and (slugify/valid-slug? org-slug) (slugify/valid-slug? slug)) 
                              user-id (:data ctx)
                              org (and (slugify/valid-slug? org-slug) (org-res/get-org conn org-slug))
                              board (and (slugify/valid-slug? slug) (board-res/get-board conn (:uuid org) slug))]
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing? ((set (member-type board)) user-id)}
                        false))
    :delete (fn [ctx] (if-let* [org (and (slugify/valid-slug? org-slug) (org-res/get-org conn org-slug))
                                board (and (slugify/valid-slug? slug) (board-res/get-board conn (:uuid org) slug))
                                exists? ((set (member-type board)) user-id)] ; short circuits the delete w/ a 404
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board) :existing? true}
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
      (ANY "/orgs/:org-slug/boards/:slug/" [org-slug slug] (pool/with-pool [conn db-pool] (board conn org-slug slug)))
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