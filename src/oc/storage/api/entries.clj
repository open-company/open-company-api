(ns oc.storage.api.entries
  "Liberator API for entry resources."
  (:require [clojure.string :as s]
            [defun.core :refer (defun-)]
            [if-let.core :refer (if-let* when-let*)]
            [taoensso.timbre :as timbre]
            [compojure.core :as compojure :refer (ANY OPTIONS DELETE)]
            [liberator.core :refer (defresource by-method)]
            [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.db.pool :as pool]
            [oc.lib.db.common :as db-common]
            [oc.lib.api.common :as api-common]
            [oc.lib.slugify :as slugify]
            [oc.storage.config :as config]
            [oc.storage.api.access :as access]
            [oc.storage.async.notification :as notification]
            [oc.storage.async.email :as email]
            [oc.storage.async.bot :as bot]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.entry :as entry-rep]
            [oc.storage.resources.common :as common-res]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]
            [oc.storage.resources.entry :as entry-res]
            [oc.storage.resources.reaction :as reaction-res]))

;; ----- Utility functions -----

(defun- trigger-share-requests
  "Parallel recursive function to send share requests to AWS SQS."

  ;; Initial
  ([org board entry user share-requests :guard seq?]
  (doall (pmap (partial trigger-share-requests org board entry user) share-requests)))

  ;; Email share
  ([org board entry user share-request :guard #(= "email" (:medium %)) ]
  (timbre/info "Triggering share: email for" (:uuid entry) "of" (:slug org))
  (email/send-trigger! (email/->trigger org board entry share-request user)))

  ;; Slack share
  ([org board entry user share-request :guard #(= "slack" (:medium %))]
  (timbre/info "Triggering share: slack for" (:uuid entry) "of" (:slug org))
  (bot/send-share-entry-trigger! (bot/->share-entry-trigger org board entry share-request user))))

;; ----- Validations -----

(defn- valid-new-entry? [conn org-slug board-slug ctx]
  (if-let [board (board-res/get-board conn (org-res/uuid-for conn org-slug) board-slug)]
    (try
      ;; Create the new entry from the URL and data provided
      (let [entry-map (:data ctx)
            author (:user ctx)
            new-entry (entry-res/->entry conn (:uuid board) entry-map author)
            ts (db-common/current-timestamp)]
        {:new-entry (api-common/rep new-entry) :existing-board (api-common/rep board)})

      (catch clojure.lang.ExceptionInfo e
        [false, {:reason (.getMessage e)}])) ; Not a valid new entry
    [false, {:reason "Invalid board."}])) ; couldn't find the specified board

(defn- valid-entry-update? [conn entry-uuid entry-props user entry-publish?]
  (if-let [existing-entry (entry-res/get-entry conn entry-uuid)]
    ;; Merge the existing entry with the new updates
    (let [new-board-slug (:board-slug entry-props) ; check if they are moving the entry
          new-board (when new-board-slug ; look up the board it's being moved to
                          (board-res/get-board conn (:org-uuid existing-entry) new-board-slug))
          old-board (when new-board
                      (board-res/get-board conn (:board-uuid existing-entry)))
          new-board-uuid (:uuid new-board)
          without-status-props (dissoc entry-props :status)
          with-status-props (if entry-publish?
                              (assoc without-status-props :status :published)
                              (assoc without-status-props :status (:status existing-entry)))
          props (if new-board-uuid
                  (assoc with-status-props :board-uuid new-board-uuid)
                  (dissoc with-status-props :board-uuid))
          merged-entry (merge existing-entry (entry-res/ignore-props props))
          updated-entry (update merged-entry :attachments #(entry-res/timestamp-attachments %))
          ts (db-common/current-timestamp)]
      (if (lib-schema/valid? common-res/Entry updated-entry)
        {:existing-entry (api-common/rep existing-entry)
         :existing-board (api-common/rep new-board)
         :moving-board (api-common/rep old-board)
         :updated-entry (api-common/rep updated-entry)}
        (do
         (println (lib-schema/valid? common-res/Entry updated-entry))
         [false, {:updated-entry (api-common/rep updated-entry)}]))) ; invalid update
    
    true)) ; no existing entry, so this will fail existence check later

(defn- valid-entry-revert? [entry-props]
  (lib-schema/valid? schema/Int (:revision-id entry-props)))

(defn- valid-share-requests? [conn entry-uuid share-props]
  (if-let* [existing-entry (entry-res/get-entry conn entry-uuid)
            ts (db-common/current-timestamp)
            _seq? (seq? share-props)
            share-requests (map #(assoc % :shared-at ts) share-props)]
    (if (every? #(lib-schema/valid? common-res/ShareRequest %) share-requests)
        {:existing-entry (api-common/rep existing-entry) :share-requests (api-common/rep share-requests)}
        [false, {:share-requests (api-common/rep share-requests)}]) ; invalid share request
    
    true)) ; no existing entry, so this will fail existence check later

(defn- entry-list-for-board
  "Retrieve an entry list for the board, or false if the org or board doesn't exist."
  [conn org-slug board-slug-or-uuid ctx]
  (if-let* [_slugs? (and (slugify/valid-slug? org-slug)
                         (slugify/valid-slug? board-slug-or-uuid))
            org (or (:existing-org ctx)
                    (org-res/get-org conn org-slug))
            org-uuid (:uuid org)
            board (board-res/get-board conn org-uuid board-slug-or-uuid)
            board-uuid (:uuid board)
            entries (entry-res/list-entries-by-board conn board-uuid {})]
    {:existing-org (api-common/rep org)
     :existing-board (api-common/rep board)
     :existing-entries (api-common/rep entries)}
    false))

(defn- valid-entry-inbox-update? [conn ctx org-slug entry-uuid user action-type]
  (timbre/info "Valid new update for" entry-uuid "from user" (:user-id user) "action" action-type)
  (if-let* [existing-entry (entry-res/get-entry conn entry-uuid)
            existing-org (or (:existing-org ctx) (org-res/get-org conn org-slug))
            existing-board (or (:existing-board ctx) (board-res/get-board conn (:board-uuid existing-entry)))]
    ;; Merge the existing entry with the new updates
    (let [dismiss-at (when (= action-type :dismiss) (-> ctx :request :body slurp))
          user-visibility (or (some (fn [[k v]] (when (= k (-> user :user-id keyword)) v)) (:user-visibility existing-entry))
                              {})
          updated-user-visibility (cond
                                    (= action-type :dismiss)
                                    (assoc user-visibility :dismiss-at dismiss-at)
                                    (= action-type :unread)
                                    (merge user-visibility {:dismiss-at nil :unfollow false})
                                    (= action-type :follow)
                                    (assoc user-visibility :unfollow false)
                                    (= action-type :unfollow)
                                    (assoc user-visibility :unfollow true))
          updated-entry (assoc-in existing-entry [:user-visibility (keyword (:user-id user))] updated-user-visibility)]
      (timbre/info "User visibility" user-visibility "updated:" updated-user-visibility)
      (if (and (or (not= action-type :dismiss)
                   (and (= action-type :dismiss)
                        (lib-schema/valid? lib-schema/ISO8601 dismiss-at)))
               (lib-schema/valid? common-res/Entry updated-entry))
        {:existing-org (api-common/rep existing-org)
         :existing-board (api-common/rep existing-board)
         :existing-entry (api-common/rep existing-entry)
         :updated-entry (api-common/rep updated-entry)
         :dismiss-at dismiss-at}
        [false, {:updated-entry (api-common/rep updated-entry)}])) ; invalid update
    
    true)) ; no existing entry, so this will fail existence check later

(defn- valid-dismiss-all-update? [conn ctx org-slug user]
  (timbre/info "Valid dismiss-all update for" org-slug "from user" (:user-id user))
  (if-let* [existing-org (or (:existing-org ctx) (org-res/get-org conn org-slug))
            dismiss-at (-> ctx :request :body slurp)]
    (let [boards (board-res/list-boards-by-org conn (:uuid existing-org) [:created-at :updated-at :authors :viewers :access])
          allowed-boards (map :uuid (filter #(access/access-level-for existing-org % user) boards))
          existing-entries (entry-res/list-all-entries-for-inbox conn (:uuid existing-org) (:user-id user) :desc
                            (db-common/current-timestamp) 0 allowed-boards)
          updated-entries (mapv
                           #(-> %
                             (assoc-in [:user-visibility (keyword (:user-id user)) :dismiss-at] dismiss-at)
                             (dissoc :last-activity-at :interactions))
                           existing-entries)]
      (if (and (lib-schema/valid? lib-schema/ISO8601 dismiss-at)
               (every? #(lib-schema/valid? common-res/Entry %) updated-entries))
        (do
          (timbre/info "Update user-visibility for entries:" (map :uuid updated-entries))
          {:existing-org (api-common/rep existing-org)
           :existing-entries (api-common/rep existing-entries)
           :updated-entries (api-common/rep updated-entries)
           :dismiss-at dismiss-at})
        (do
          (timbre/warn "Failed dismiss-all with dismiss-at:" dismiss-at)
          (doseq [e updated-entries]
            (when-not (lib-schema/valid? common-res/Entry e)
              (timbre/info "Failed for" (:uuid e))))
          false)))
    true)) ; no existing entry, so this will fail existence check later

;; ----- Actions -----

(defn- share-entry [conn ctx entry-for]
  (timbre/info "Sharing entry:" entry-for)
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            user (:user ctx)
            share-requests (:share-requests ctx)
            shared {:shared (take 50 (reverse (sort-by :shared-at (concat (or (:shared entry) []) share-requests))))}
            update-result (entry-res/update-entry-no-version! conn (:uuid entry) shared user)
            entry-with-comments (assoc entry :existing-comments (entry-res/list-comments-for-entry conn (:uuid entry)))]
    (do
      (when (and (seq? share-requests) (any? share-requests))
        (trigger-share-requests org board (assoc entry-with-comments :auto-share (:auto-share ctx)) user share-requests))
      (timbre/info "Shared entry:" entry-for)
      {:updated-entry (api-common/rep update-result)})
    (do
      (timbre/error "Failed sharing entry:" entry-for) false)))

(defn auto-share-on-publish
  [conn ctx entry-result]
  (if-let* [slack-channel (:slack-mirror (:existing-board ctx))
            _can-slack-share (bot/has-slack-bot-for? (:slack-org-id slack-channel) (:user ctx))]
    (let [share-request {:medium "slack"
                         :note ""
                         :shared-at (db-common/current-timestamp)
                         :channel slack-channel}
          share-ctx (-> ctx
                        (assoc :share-requests (list share-request))
                        (assoc :existing-entry (api-common/rep entry-result))
                        (assoc :auto-share true))]
      (share-entry conn share-ctx (:uuid entry-result)))))

(defn undraft-board [conn user org board]
  (when (:draft board)
    (let [updated-board (assoc board :draft false)]
      (timbre/info "Unsetting draft for board:" (:slug board))
      (board-res/update-board! conn (:uuid board) updated-board))))

(defn- create-entry [conn ctx entry-for]
  (timbre/info "Creating entry for:" entry-for)
  (if-let* [user (:user ctx)
            org (:existing-org ctx)
            board (:existing-board ctx)
            new-entry (:new-entry ctx)
            entry-result (entry-res/create-entry! conn new-entry)] ; Add the entry    
    (do
      (timbre/info "Created entry for:" entry-for "as" (:uuid entry-result))
      (when (= (keyword (:status entry-result)) :published)
        (undraft-board conn user org board)
        (entry-res/delete-versions conn entry-result)
        (auto-share-on-publish conn ctx entry-result))
      (notification/send-trigger! (notification/->trigger :add org board {:new entry-result} user nil))
      {:created-entry (api-common/rep entry-result)})

    (do (timbre/error "Failed creating entry:" entry-for) false)))

(defn- update-entry [conn ctx entry-for]
  (timbre/info "Updating entry for:" entry-for)
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            user (:user ctx)
            entry (:existing-entry ctx)
            updated-entry (:updated-entry ctx)
            updated-result (entry-res/update-entry! conn (:uuid entry) updated-entry user)]
    (let [old-board (:moving-board ctx)]
      ;; If we are moving the entry from a draft board, check if we need to remove the board itself.
      (when old-board
        (let [remaining-entries (entry-res/list-all-entries-by-board conn (:uuid old-board))]
          (board-res/maybe-delete-draft-board conn org old-board remaining-entries user)))
      (timbre/info "Updated entry for:" entry-for)
      (notification/send-trigger! (notification/->trigger :update org board {:old entry :new updated-result} user nil))
      {:updated-entry (api-common/rep (assoc updated-result :board-name (:name board)))})

    (do (timbre/error "Failed updating entry:" entry-for) false)))

(defn- update-user-visibility [conn ctx entry-for action-type]
  (timbre/info "Updating entry for:" entry-for)
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            user (:user ctx)
            entry (:existing-entry ctx)
            updated-entry (:updated-entry ctx)
            final-entry (entry-res/update-entry-no-user! conn (:uuid updated-entry) updated-entry)]
    (let [sender-ws-client-id (api-common/get-change-client-id ctx)
          notify-map (cond-> {}
                        (seq sender-ws-client-id) (assoc :client-id sender-ws-client-id)
                        (= action-type :dismiss)  (assoc :dismiss-at (:dismiss-at ctx))
                        (= action-type :unread)   (assoc :dismiss-at nil)
                        (= action-type :follow)   (assoc :follow true)
                        (= action-type :unfollow) (assoc :unfollow true))]
      (timbre/info "Updated entry new for:" entry-for "action:" action-type)
      (notification/send-trigger! (notification/->trigger action-type org board {:old entry :new updated-entry :inbox-action notify-map} user nil))
      {:updated-entry (api-common/rep final-entry)})

    (do (timbre/error "Failed updating entry:" entry-for) false)))

(defn- update-user-visibility-dismiss-all [conn ctx entry-for]
  (timbre/info "Dismiss all entries for:" entry-for ". Dismissing" (count (:updated-entries ctx)) "entries for user" (-> ctx :user :user-id))
  (if-let* [org (:existing-org ctx)
            user (:user ctx)
            existing-entries (:existing-entries ctx)
            updated-entries (:updated-entries ctx)
            final-entries (map
                            #(entry-res/update-entry-no-user! conn (:uuid %) %)
                            updated-entries)]
    (if (every? #(lib-schema/valid? common-res/Entry %) final-entries)
      (let [notify-map {:client-id (api-common/get-change-client-id ctx)
                        :dismiss-at (:dismiss-at ctx)}]
        (timbre/info "Dismissed all entries for:" entry-for)
        (doseq [entry final-entries]
          (notification/send-trigger! (notification/->trigger :dismiss org nil {:new entry :inbox-action notify-map} user nil)))
        {:updated-entries (api-common/rep final-entries)})
      false)

    (do (timbre/error "Failed updating entry:" entry-for) false)))

(defn- publish-entry [conn ctx entry-for]
  (timbre/info "Publishing entry for:" entry-for)
  (if-let* [user (:user ctx)
            org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            updated-entry (:updated-entry ctx)
            final-entry (entry-res/publish-entry! conn (:uuid updated-entry) updated-entry user)]
    (let [old-board (:moving-board ctx)]
      (undraft-board conn user org board)
      ;; If we are moving the entry from a draft board, check if we need to remove the board itself.
      (when old-board
        (let [remaining-entries (entry-res/list-all-entries-by-board conn (:uuid old-board))]
          (board-res/maybe-delete-draft-board conn org old-board remaining-entries user)))
      (entry-res/delete-versions conn final-entry)
      (auto-share-on-publish conn ctx final-entry)
      (timbre/info "Published entry:" entry-for)
      (notification/send-trigger! (notification/->trigger :add org board {:new final-entry} user nil))
      {:updated-entry (api-common/rep final-entry)})
    (do (timbre/error "Failed publishing entry:" entry-for) false)))

(defn- delete-entry [conn ctx entry-for]
  (timbre/info "Deleting entry for:" entry-for)
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            _delete-result (entry-res/delete-entry! conn (:uuid entry))]
    (do
      ;; If deleting a draft on a draft board
      (when (= (keyword (:status entry)) :draft)
        (let [remaining-entries (entry-res/list-all-entries-by-board conn (:uuid board))]
          (board-res/maybe-delete-draft-board conn org board remaining-entries (:user ctx))))
      (when (not= (keyword (:status entry)) :published)
        (entry-res/delete-versions conn (assoc entry :delete-entry true)))
      (timbre/info "Deleted entry for:" entry-for)
      (notification/send-trigger! (notification/->trigger :delete org board {:old entry} (:user ctx) nil))
      true)
    (do (timbre/error "Failed deleting entry for:" entry-for) false)))

(defn- revert-entry-version [conn ctx entry-for]
  (timbre/info "Reverting entry for:" entry-for)
  (if-let* [user (:user ctx)
            org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            entry-version (:existing-version ctx)
            revert-result (entry-res/revert-entry! conn entry entry-version user)]
  (do
    (timbre/info "Reverted entry for:" (:uuid entry))
    {:updated-entry (api-common/rep revert-result)})
  (do (timbre/error "Failed reverting entry:" entry-for) false)))

(defn- delete-sample-entries! [conn ctx]
  (timbre/info "Remove all sample entries for org:" (:uuid (:existing-org ctx)))
  (if-let* [org (:existing-org ctx)
            samples (entry-res/get-sample-entries conn (:uuid org))]
    (do
      (doseq [sample samples]
        (entry-res/delete-entry! conn (:uuid sample)))
      {:deleted-samples (count samples)})
    false))

(defn- add-bookmark [conn ctx entry-for]
  (timbre/info "Creating bookmark for entry:" entry-for "for user" (-> ctx :user :user-id))
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            user (:user ctx)]
    (if-let [updated-entry (entry-res/add-bookmark! conn (:uuid entry) user)]
      (do
        (timbre/info "Bookmark created for entry:" entry-for "and user" (-> ctx :user :user-id))
        {:updated-entry (api-common/rep updated-entry)})
      (do
        (timbre/info "Bookmark not added, it already exists for entry" entry-for "and user" (-> ctx :user :user-id))
        {:updated-entry (api-common/rep entry)}))
    (do
      (timbre/error "Failed adding bookmark for entry:" entry-for "and user" (-> ctx :user :user-id))
      false)))

(defn- remove-bookmark [conn ctx entry-for]
  (timbre/info "Removing bookmark for entry:" entry-for "for user" (-> ctx :user :user-id))
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            user (:user ctx)]
    (if-let [updated-entry (entry-res/remove-bookmark! conn (:uuid entry) user)]
      (do
        (timbre/info "Bookmark removed for entry:" entry-for "and user" (-> ctx :user :user-id))
        {:updated-entry (api-common/rep updated-entry)})
      (do
        (timbre/info "Bookmark not removed, no bookmark to remove for:" entry-for "and user" (-> ctx :user :user-id))
        {:updated-entry (api-common/rep entry)}))
    (do
      (timbre/error "Failed removing bookmark for entry:" entry-for "and user" (-> ctx :user :user-id))
      false)))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for operations on a particular entry
(defresource entry [conn org-slug board-slug-or-uuid entry-uuid]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :get :patch :delete]

  ;; Media type client accepts
  :available-media-types [mt/entry-media-type]
  :handle-not-acceptable (api-common/only-accept 406 mt/entry-media-type)
  
  ;; Media type client sends
  :known-content-type? (by-method {
    :options true
    :get true
    :patch (fn [ctx] (api-common/known-content-type? ctx mt/entry-media-type))
    :delete true})

  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/access-level-for conn org-slug board-slug-or-uuid (:user ctx)))
    :patch (fn [ctx] (access/allow-authors conn org-slug board-slug-or-uuid (:user ctx)))
    :delete (fn [ctx] (access/allow-authors conn org-slug board-slug-or-uuid (:user ctx)))})

  ;; Validations
  :processable? (by-method {
    :options true
    :get true
    :patch (fn [ctx] (and (slugify/valid-slug? org-slug)
                          (slugify/valid-slug? board-slug-or-uuid)
                          (valid-entry-update? conn entry-uuid (:data ctx) (:user ctx) false)))
    :delete true})

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [_entry-id (lib-schema/unique-id? entry-uuid)
                               org (or (:existing-org ctx)
                                       (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)
                               board (or (:existing-board ctx)
                                         (board-res/get-board conn org-uuid board-slug-or-uuid))
                               entry (or (:existing-entry ctx)
                                         (entry-res/get-entry conn org-uuid (:uuid board) entry-uuid))
                               comments (or (:existing-comments ctx)
                                            (entry-res/list-comments-for-entry conn (:uuid entry)))
                               reactions (or (:existing-reactions ctx)
                                            (entry-res/list-reactions-for-entry conn (:uuid entry)))]
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing-entry (api-common/rep entry) :existing-comments (api-common/rep comments)
                         :existing-reactions (api-common/rep reactions)}
                        false))

  ;; Actions
  :patch! (fn [ctx] (update-entry conn ctx (s/join " " [org-slug board-slug-or-uuid entry-uuid])))
  :delete! (fn [ctx] (delete-entry conn ctx (s/join " " [org-slug board-slug-or-uuid entry-uuid])))

  ;; Responses
  :handle-ok (by-method {
    :get (fn [ctx] (entry-rep/render-entry 
                      (:existing-org ctx)
                      (:existing-board ctx)
                      (:existing-entry ctx)
                      (:existing-comments ctx)
                      (reaction-res/aggregate-reactions (:existing-reactions ctx))
                      (:access-level ctx)
                      (-> ctx :user :user-id)))
    :patch (fn [ctx] (entry-rep/render-entry
                        (:existing-org ctx)
                        (:existing-board ctx)
                        (:updated-entry ctx)
                        (:existing-comments ctx)
                        (reaction-res/aggregate-reactions (:existing-reactions ctx))
                        (:access-level ctx)
                        (-> ctx :user :user-id)))})
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (schema/check common-res/Entry (:updated-entry ctx)))))

;; A resource for operations on all entries of a particular board
(defresource entry-list [conn org-slug board-slug-or-uuid]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :get :post :delete]

  ;; Media type client accepts
  :available-media-types (by-method {
                            :get [mt/entry-collection-media-type]
                            :post [mt/entry-media-type]
                            :delete [mt/entry-collection-media-type]})
  :handle-not-acceptable (by-method {
                            :get (api-common/only-accept 406 mt/entry-collection-media-type)
                            :post (api-common/only-accept 406 mt/entry-media-type)
                            :delete (api-common/only-accept 406 mt/entry-collection-media-type)})

  ;; Media type client sends
  :known-content-type? (by-method {
                          :options true
                          :get true
                          :post (fn [ctx] (api-common/known-content-type? ctx mt/entry-media-type))
                          :delete true})

  ;; Authorization
  :allowed? (by-method {
    :options true
    :get (fn [ctx] (access/access-level-for conn org-slug board-slug-or-uuid (:user ctx)))
    :post (fn [ctx] (access/allow-authors conn org-slug board-slug-or-uuid (:user ctx)))
    :delete (fn [ctx] (access/allow-authors conn org-slug board-slug-or-uuid (:user ctx)))})

  ;; Validations
  :processable? (by-method {
    :options true
    :get true
    :post (fn [ctx] (and (slugify/valid-slug? org-slug)
                         (slugify/valid-slug? board-slug-or-uuid)
                         (valid-new-entry? conn org-slug board-slug-or-uuid ctx)))
    :delete true})

  ;; Existentialism
  :exists? (by-method {
    :options true
    :post (partial entry-list-for-board conn org-slug board-slug-or-uuid)
    :get (fn [ctx] (entry-list-for-board conn org-slug board-slug-or-uuid))
    :delete (fn [ctx]
              (if-let* [_slugs? (slugify/valid-slug? org-slug)
                        org (or (:existing-org ctx)
                                (org-res/get-org conn org-slug))]
                {:existing-org (api-common/rep org)}
                false))})

  ;; Actions
  :post! (fn [ctx] (create-entry conn ctx (s/join " " [org-slug (:slug (:existing-board ctx))])))
  :delete! (fn [ctx] (delete-sample-entries! conn ctx))

  ;; Responses
  :handle-ok (by-method {
    :get (fn [ctx] (entry-rep/render-entry-list (:existing-org ctx) (:existing-board ctx) (:existing-entries ctx) ctx))
    :post (fn [ctx] (entry-rep/render-entry-list (:existing-org ctx) (:existing-board ctx) (:existing-entries ctx)
                     ctx))})
  :handle-created (fn [ctx] (let [new-entry (:created-entry ctx)
                                  existing-board (:existing-board ctx)]
                              (api-common/location-response
                                (entry-rep/url org-slug (:slug existing-board) (:uuid new-entry))
                                (entry-rep/render-entry (:existing-org ctx) (:existing-board ctx) new-entry [] []
                                  :author (-> ctx :user :user-id))
                                mt/entry-media-type)))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (:reason ctx))))

;; A resource for reverting to a specific revision number.
(defresource revert-version [conn org-slug board-slug-or-uuid entry-uuid]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :post]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-authors conn org-slug board-slug-or-uuid (:user ctx)))})

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/entry-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/entry-media-type)})

  ;; Media type client sends
  :known-content-type? (by-method {
                          :options true
                          :post (fn [ctx] (api-common/known-content-type? ctx mt/revert-request-media-type))})

  ;; Possibly no data to handle
  :malformed? (by-method {
    :options false
    :post (fn [ctx] (api-common/malformed-json? ctx true))}) ; allow nil
  :processable? (by-method {
    :options true
    :post (fn [ctx] (let [entry-props (:data ctx)]
                      (valid-entry-revert? entry-props)))})
  :new? false
  :respond-with-entity? true

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx]
             (if-let* [_slugs? (and (slugify/valid-slug? org-slug)
                                    (slugify/valid-slug? board-slug-or-uuid))
                       org (org-res/get-org conn org-slug)
                       org-uuid (:uuid org)
                       entry (entry-res/get-entry conn entry-uuid)
                       existing-version (if (= -1 (:revision-id (:data ctx)))
                                          (assoc entry :revision-id -1)
                                          (entry-res/get-version
                                           conn
                                           entry-uuid
                                           (:revision-id (:data ctx))))
                       board (board-res/get-board conn (:board-uuid entry))
                      _matches? (and (= org-uuid (:org-uuid entry))
                                     (= org-uuid (:org-uuid board)))]
                      {:existing-org (api-common/rep org)
                       :existing-board (api-common/rep board)
                       :existing-entry (api-common/rep entry)
                       :existing-version (api-common/rep existing-version)}
                      false))

  ;; Actions
  :post! (fn [ctx] (revert-entry-version conn ctx (s/join " " [org-slug (:slug (:existing-board ctx)) entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (:updated-entry ctx)
                                               [] ; no comments since it's always a draft
                                               [] ; no reactions since it's always a draft
                                               (:access-level ctx)
                                               (-> ctx :user :user-id))))

;; A resource for operations to publish a particular entry
(defresource publish [conn org-slug board-slug entry-uuid]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :post]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-authors conn org-slug board-slug (:user ctx)))})

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/entry-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/entry-media-type)})

  ;; Possibly no data to handle
  :malformed? (by-method {
    :options false
    :post (fn [ctx] (api-common/malformed-json? ctx true))}) ; allow nil
  :processable? (by-method {
    :options true
    :post (fn [ctx] (let [entry-props (:data ctx)]
                      (or (nil? entry-props) ; no updates during publish is fine
                          (valid-entry-update? conn entry-uuid entry-props (:user ctx) true))))})
  :new? false
  :respond-with-entity? true

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx] (if-let* [_slugs? (and (slugify/valid-slug? org-slug)
                                            (slugify/valid-slug? board-slug))
                               org (or (:existing-org ctx)
                                       (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)
                               entry (or (:existing-entry ctx)
                                         (entry-res/get-entry conn entry-uuid))
                               board (or (:existing-board ctx)
                                         (board-res/get-board conn (:board-uuid entry)))
                               _matches? (and (= org-uuid (:org-uuid entry))
                                              (= org-uuid (:org-uuid board))
                                              (= :draft (keyword (:status entry))))] ; sanity check
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing-entry (api-common/rep entry)}
                        false))
  
  ;; Actions
  :post! (fn [ctx] (publish-entry conn ctx (s/join " " [org-slug board-slug entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (:updated-entry ctx)
                                               [] ; no comments
                                               [] ; no reactions
                                               (:access-level ctx)
                                               (-> ctx :user :user-id))))

;; A resource for operations to share a particular entry
(defresource share [conn org-slug board-slug entry-uuid]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken

  :allowed-methods [:options :post]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-authors conn org-slug board-slug (:user ctx)))})

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/entry-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/entry-media-type)})

  ;; Media type client sends
  :known-content-type? (by-method {
                          :options true
                          :post (fn [ctx] (api-common/known-content-type? ctx mt/share-request-media-type))})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? (by-method {
    :options true
    :post (fn [ctx] (valid-share-requests? conn entry-uuid (:data ctx)))})

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx] (if-let* [_slugs? (and (slugify/valid-slug? org-slug)
                                            (slugify/valid-slug? board-slug))
                               org (or (:existing-org ctx)
                                       (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)
                               entry (or (:existing-entry ctx)
                                         (entry-res/get-entry conn entry-uuid))
                               board (board-res/get-board conn (:board-uuid entry))
                               _matches? (and (= org-uuid (:org-uuid entry))
                                              (= org-uuid (:org-uuid board))
                                              (= :published (keyword (:status entry)))) ; sanity check
                               comments (or (:existing-comments ctx)
                                            (entry-res/list-comments-for-entry conn (:uuid entry)))
                               reactions (or (:existing-reactions ctx)
                                             (entry-res/list-reactions-for-entry conn (:uuid entry)))] 
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing-entry (api-common/rep entry) :existing-comments (api-common/rep comments)
                         :existing-reactions (api-common/rep reactions)}
                        false))
  
  ;; Actions
  :post! (fn [ctx] (share-entry conn ctx (s/join " " [org-slug board-slug entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (:updated-entry ctx)
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (:access-level ctx)
                                               (-> ctx :user :user-id)))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (map #(schema/check common-res/ShareRequest %) (:share-requests ctx)))))


;; A resource for access to a particular entry by its secure UUID
(defresource entry-access [conn org-slug secure-uuid]
  (api-common/anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :get]

  ;; Authorization
  :allowed? (fn [ctx]
              (let [org (or (:existing-org ctx)
                            (org-res/get-org conn org-slug))]                
                (if (:id-token ctx) ; access by secure link
                  (= secure-uuid (:secure-uuid (:user ctx))) ; ensure secured UUID from secure link is for this entry
                  (if (-> org :content-visibility :disallow-public-share)
                    false ; org doesn't allow secure links                  
                    true)))) ; not logged in are allowed by using the secure link

  ;; Media type client accepts
  :available-media-types (by-method {
                            :get [mt/entry-media-type]})
  :handle-not-acceptable (by-method {
                            :get (api-common/only-accept 406 mt/entry-media-type)})

  ;; Existentialism
  :exists? (fn [ctx] (if-let* [_slug? (slugify/valid-slug? org-slug)
                               org (or (:existing-org ctx)
                                       (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)
                               entry (or (:existing-entry ctx)
                                         (entry-res/get-entry-by-secure-uuid conn org-uuid secure-uuid))
                               board (board-res/get-board conn (:board-uuid entry))
                               _matches? (= org-uuid (:org-uuid board)) ; sanity check
                               access-level (or (:access-level (access/access-level-for org board (:user ctx))) :public)
                               comments (if (or (= :author access-level) (= :viewer access-level))
                                          (or (:existing-comments ctx)
                                              (entry-res/list-comments-for-entry conn (:uuid entry)))
                                          [])
                               reactions (if (or (= :author access-level) (= :viewer access-level))
                                          (or (:existing-reactions ctx)
                                              (entry-res/list-reactions-for-entry conn (:uuid entry)))
                                          [])]
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing-entry (api-common/rep entry) :existing-comments (api-common/rep comments)
                         :existing-reactions (api-common/rep reactions)
                         :access-level access-level}
                        false))
  
  ;; Responses
  :handle-ok (fn [ctx] (let [access-level (:access-level ctx)]
                          (entry-rep/render-entry (:existing-org ctx)
                                                  (:existing-board ctx)
                                                  (:existing-entry ctx)
                                                  (:existing-comments ctx)
                                                  (if (or (= :author access-level) (= :viewer access-level))
                                                    (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                                    [])
                                                  access-level
                                                  (-> ctx :user :user-id)
                                                  :secure))))

(defresource bookmark [conn org-slug board-slug entry-uuid]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken
  :allowed-methods [:options :post :delete]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/entry-media-type]
                            :delete [mt/entry-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/entry-media-type)
                            :delete (api-common/only-accept 406 mt/entry-media-type)})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? true

  ;; Possibly no data to handle
  :malformed? false ; allow nil

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx] (if-let* [_slugs? (and (slugify/valid-slug? org-slug)
                                            (slugify/valid-slug? board-slug))
                               org (or (:existing-org ctx)
                                       (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)
                               entry (or (:existing-entry ctx)
                                         (entry-res/get-entry conn entry-uuid))
                               board (board-res/get-board conn (:board-uuid entry))
                               _matches? (and (= org-uuid (:org-uuid entry))
                                              (= org-uuid (:org-uuid board))
                                              (= :published (keyword (:status entry)))) ; sanity check
                               comments (or (:existing-comments ctx)
                                            (entry-res/list-comments-for-entry conn (:uuid entry)))
                               reactions (or (:existing-reactions ctx)
                                             (entry-res/list-reactions-for-entry conn (:uuid entry)))
                               access-level (or (:access-level (access/access-level-for org board (:user ctx))) :public)]
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing-entry (api-common/rep entry)
                         :existing-comments (api-common/rep comments)
                         :existing-reactions (api-common/rep reactions)
                         :access-level access-level}
                        false))

  ;; Actions
  :post! (fn [ctx] (add-bookmark conn ctx (s/join " " [org-slug board-slug entry-uuid])))
  :delete! (fn [ctx] (remove-bookmark conn ctx (s/join " " [org-slug board-slug entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (or (:updated-entry ctx) (:existing-entry ctx))
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (:access-level ctx)
                                               (-> ctx :user :user-id)))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (schema/check common-res/Entry (:updated-entry ctx)))))

(defresource inbox-dismiss-all [conn org-slug]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken
  :allowed-methods [:options :post]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  :known-content-type? (by-method {
    :options true
    :post (fn [ctx] (api-common/known-content-type? ctx "text/plain"))})

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/entry-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/entry-media-type)})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? (by-method {
    :options true
    :post (fn [ctx] (valid-dismiss-all-update? conn ctx org-slug (:user ctx)))})

  ;; Possibly no data to handle
  :malformed? false ; allow nil

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx] (if-let* [_slugs? (slugify/valid-slug? org-slug)
                               org (or (:existing-org ctx)
                                       (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)]
                        {:existing-org (api-common/rep org)}
                        false))

  ;; Actions
  :post! (fn [ctx]
           (update-user-visibility-dismiss-all conn ctx org-slug))

  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (:updated-entries ctx))))

(defresource inbox [conn org-slug board-slug entry-uuid action-type]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken
  :allowed-methods [:options :post]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  :known-content-type? (by-method {
    :options true
    :post (fn [ctx] (api-common/known-content-type? ctx "text/plain"))})

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/entry-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/entry-media-type)})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? (by-method {
    :options true
    :post (fn [ctx] (valid-entry-inbox-update? conn ctx org-slug entry-uuid (:user ctx) action-type))})

  ;; Possibly no data to handle
  :malformed? false ; allow nil

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx] (if-let* [_slugs? (and (slugify/valid-slug? org-slug)
                                            (slugify/valid-slug? board-slug))
                               org (or (:existing-org ctx)
                                       (org-res/get-org conn org-slug))
                               org-uuid (:uuid org)
                               entry (or (:existing-entry ctx)
                                         (entry-res/get-entry conn entry-uuid))
                               board (board-res/get-board conn (:board-uuid entry))
                               comments (or (:existing-comments ctx)
                                            (entry-res/list-comments-for-entry conn (:uuid entry)))
                               reactions (or (:existing-reactions ctx)
                                             (entry-res/list-reactions-for-entry conn (:uuid entry)))]
                        {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
                         :existing-entry (api-common/rep entry)
                         :existing-comments (api-common/rep comments)
                         :existing-reactions (api-common/rep reactions)}
                        false))

  ;; Actions
  :post! (fn [ctx]
           (update-user-visibility conn ctx (s/join " " [org-slug board-slug entry-uuid]) action-type))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (:updated-entry ctx)
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (:access-level ctx)
                                               (-> ctx :user :user-id)))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (:updated-entry ctx))))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; Delete sample posts
      (OPTIONS "/orgs/:org-slug/entries/samples"
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      (OPTIONS "/orgs/:org-slug/entries/samples/"
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      (DELETE "/orgs/:org-slug/entries/samples"
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      (DELETE "/orgs/:org-slug/entries/samples/"
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      ;; Secure UUID access
      (ANY "/orgs/:org-slug/entries/:secure-uuid"
        [org-slug secure-uuid]
        (pool/with-pool [conn db-pool] 
          (entry-access conn org-slug secure-uuid)))
      (ANY "/orgs/:org-slug/entries/:secure-uuid/"
        [org-slug secure-uuid]
        (pool/with-pool [conn db-pool] 
          (entry-access conn org-slug secure-uuid)))
      ;; Entry list operations
      (ANY "/orgs/:org-slug/boards/:board-slug/entries"
        [org-slug board-slug]
        (pool/with-pool [conn db-pool] 
          (entry-list conn org-slug board-slug)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/"
        [org-slug board-slug]
        (pool/with-pool [conn db-pool] 
          (entry-list conn org-slug board-slug)))
      ;; Entry operations
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool] 
          (entry conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (entry conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/publish"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (publish conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/publish/"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (publish conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/revert"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (revert-version conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/revert/"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (revert-version conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/share"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (share conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/share/"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (share conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/bookmark"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (bookmark conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/bookmark/"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (bookmark conn org-slug board-slug entry-uuid)))
      (ANY "/orgs/:org-slug/inbox/dismiss-all"
        [org-slug]
        (pool/with-pool [conn db-pool]
          (inbox-dismiss-all conn org-slug)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/inbox/dismiss"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :dismiss)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/inbox/unread"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :unread)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/inbox/follow"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :follow)))
      (ANY "/orgs/:org-slug/boards/:board-slug/entries/:entry-uuid/inbox/unfollow"
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :unfollow))))))
