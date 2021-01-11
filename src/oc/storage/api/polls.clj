(ns oc.storage.api.polls
  "Liberator API for poll resources."
  (:require [if-let.core :refer (if-let* when-let*)]
            [taoensso.timbre :as timbre]
            [compojure.core :as compojure :refer (OPTIONS POST DELETE)]
            [liberator.core :refer (defresource by-method)]
            [oc.lib.schema :as lib-schema]
            [oc.lib.db.pool :as pool]
            [oc.lib.db.common :as db-common]
            [oc.lib.api.common :as api-common]
            [oc.storage.config :as config]
            [oc.storage.api.access :as access]
            [oc.storage.async.notification :as notification]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.entry :as entry-rep]
            [oc.storage.resources.org :as org-res]
            [oc.storage.urls.entry :as entry-urls]
            [oc.storage.resources.board :as board-res]
            [oc.storage.resources.entry :as entry-res]
            [oc.storage.resources.reaction :as reaction-res]))

;; Malformed?

(defn malformed-poll-reply?
  "Read in the body param from the request and make sure it's a non-blank string
  that corresponds to a user-id. Otherwise just indicate it's malformed."
  [ctx]
  (try
    (if-let* [reply-body (slurp (get-in ctx [:request :body]))]
      [false {:reply-body reply-body}]
      true)
    (catch Exception e
      (do (timbre/warn "Request body is empty " e)
        true))))

;; Existentialism checks

(defn- poll-exists? [conn ctx org-slug board-slug-or-uuid entry-uuid poll-uuid]
  (if-let* [_entry-id (lib-schema/unique-id? entry-uuid)
            org (or (:existing-org ctx)
                    (org-res/get-org conn org-slug))
            org-uuid (:uuid org)
            board (or (:existing-board ctx)
                      (board-res/get-board conn org-uuid board-slug-or-uuid))
            entry (or (:existing-entry ctx)
                      (entry-res/get-entry conn org-uuid (:uuid board) entry-uuid))
            poll (or (:existing-poll ctx)
                     (entry-res/get-poll conn entry-uuid entry poll-uuid))]
    {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
     :existing-entry (api-common/rep entry) :existing-poll poll}
    false))

(defn- poll-reply-exists? [conn ctx org-slug board-slug-or-uuid entry-uuid poll-uuid reply-id]
  (if-let* [{:keys [existing-entry existing-poll] :as existing-poll-ctx} (poll-exists? conn ctx org-slug board-slug-or-uuid entry-uuid poll-uuid)
            poll-reply (or (:existing-poll-reply ctx)
                           (entry-res/get-poll-reply conn entry-uuid existing-entry poll-uuid existing-poll reply-id))]
    (assoc existing-poll-ctx :existing-poll-reply poll-reply)
    false))

;; Replies and votes handling

(defn- create-poll-reply [conn ctx org board entry poll user reply-body]
  (timbre/info "Adding reply to poll:" (:poll-uuid poll) "on entry:" (:uuid entry))
  (let [reply-id (db-common/unique-id)
        final-entry (assoc-in entry [:polls (:poll-uuid poll) :replies reply-id]
                     {:body reply-body
                      :author (lib-schema/author-for-user user)
                      :reply-id reply-id
                      :votes []})
        updated-entry (entry-res/update-entry-no-user! conn (:uuid entry) final-entry)]
    (notification/send-trigger! (notification/->trigger :update org board {:new updated-entry :old entry} user nil (api-common/get-change-client-id ctx)))
    {:updated-entry updated-entry}))

(defn- delete-poll-reply [conn ctx org board entry poll poll-reply user]
  (timbre/info "Deleting reply for poll" (:poll-uuid poll) "on entry:" (:uuid entry) "reply:" (:reply-id poll-reply))
  (let [final-entry (update-in entry [:polls (:poll-uuid poll) :replies] dissoc (:reply-id poll-reply))
        updated-entry (entry-res/update-entry-no-user! conn (:uuid entry) final-entry)]
    (notification/send-trigger! (notification/->trigger :update org board {:new updated-entry :old entry} user nil (api-common/get-change-client-id ctx)))
    {:updated-entry updated-entry}))

(defn- update-poll-vote [conn ctx org board entry poll reply-id user add?]
  (timbre/info "Update vote for poll" (:poll-uuid poll) "on entry:" (:uuid entry) "reply:" reply-id "user:" (:user-id user) "add?" add?)
  (let [updated-entry (entry-res/poll-reply-vote! conn (:uuid entry) (:poll-uuid poll) reply-id (:user-id user) add?)]
    (notification/send-trigger! (notification/->trigger :update org board {:new updated-entry :old entry} user nil (api-common/get-change-client-id ctx)))
    {:updated-entry updated-entry}))

;; A resource for adding replies to a poll
(defresource poll-replies [conn org-slug board-slug-or-uuid entry-uuid poll-uuid]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :post]

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/poll-reply-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/poll-media-type)})

  ;; Media type client sends
  :known-content-type? (by-method {
    :options true
    :post (fn [ctx] (api-common/known-content-type? ctx "text/plain"))})

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? true

  ;; Possibly no data to handle
  :malformed? (by-method {
    :options false
    :post malformed-poll-reply?
  })

  ;; Existentialism
  :exists? (by-method {
    :options true
    :post #(poll-exists? conn % org-slug board-slug-or-uuid entry-uuid poll-uuid)})

  ;; Actions
  :post! (fn [ctx] (create-poll-reply conn ctx (:existing-org ctx) (:existing-board ctx) (:existing-entry ctx)
                    (:existing-poll ctx) (:user ctx) (:reply-body ctx)))

  ;; Responses
  :handle-ok (by-method {
    :post (fn [ctx] (entry-rep/render-entry
                     (:existing-org ctx)
                     (:existing-board ctx)
                     (:updated-entry ctx)
                     (:existing-comments ctx)
                     (reaction-res/aggregate-reactions (:existing-reactions ctx))
                     (select-keys ctx [:access-level :role])
                     (-> ctx :user :user-id)))})
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (:reason ctx))))

;; A resource for deleting a reply from a poll
(defresource poll-reply [conn org-slug board-slug-or-uuid entry-uuid poll-uuid reply-id]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :delete]

  ;; Media type client accepts
  :available-media-types (by-method {
                            :delete [mt/poll-reply-media-type]})
  :handle-not-acceptable (by-method {
                            :delete (api-common/only-accept 406 mt/poll-media-type)})

  ;; Media type client sends
  :known-content-type? true

  ;; Authorization
  :allowed? (by-method {
    :options true
    :delete (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? true

  ;; Possibly no data to handle
  :malformed? false

  ;; Existentialism
  :exists? (by-method {
    :options true
    :delete #(poll-reply-exists? conn % org-slug board-slug-or-uuid entry-uuid poll-uuid reply-id)})

  ;; Actions
  :delete! (fn [ctx] (delete-poll-reply conn ctx (:existing-org ctx) (:existing-board ctx) (:existing-entry ctx)
                      (:existing-poll ctx) (:existing-poll-reply ctx) (:user ctx)))

  ;; Responses
  :handle-ok (by-method {
    :delete (fn [ctx] (entry-rep/render-entry
                       (:existing-org ctx)
                       (:existing-board ctx)
                       (:updated-entry ctx)
                       (:existing-comments ctx)
                       (reaction-res/aggregate-reactions (:existing-reactions ctx))
                       (select-keys ctx [:access-level :role])
                       (-> ctx :user :user-id)))})
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (:reason ctx))))

;; A resource for casting and removing votes from a poll
(defresource poll-vote [conn org-slug board-slug-or-uuid entry-uuid poll-uuid reply-id]
  (api-common/open-company-anonymous-resource config/passphrase) ; verify validity of optional JWToken

  :allowed-methods [:options :post :delete]

  ;; Media type client accepts
  :available-media-types (by-method {
                            :post [mt/poll-media-type]
                            :delete [mt/poll-media-type]})
  :handle-not-acceptable (by-method {
                            :post (api-common/only-accept 406 mt/entry-media-type)
                            :delete (api-common/only-accept 406 mt/entry-collection-media-type)})

  ;; Media type client sends
  :known-content-type? true

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))
    :delete (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? true

  ;; Possibly no data to handle
  :malformed? false ; allow nil

  ;; Existentialism
  :exists? (by-method {
    :options true
    :post #(poll-reply-exists? conn % org-slug board-slug-or-uuid entry-uuid poll-uuid reply-id)
    :delete #(poll-reply-exists? conn % org-slug board-slug-or-uuid entry-uuid poll-uuid reply-id)})

  ;; Actions
  :post! (fn [ctx] (update-poll-vote conn ctx (:existing-org ctx) (:existing-board ctx) (:existing-entry ctx)
                    (:existing-poll ctx) reply-id (:user ctx) true))
  :delete! (fn [ctx] (update-poll-vote conn ctx (:existing-org ctx) (:existing-board ctx) (:existing-entry ctx)
                    (:existing-poll ctx) reply-id (:user ctx) false))

  ;; Responses
  :handle-ok (by-method {
    :post (fn [ctx] (entry-rep/render-entry
                     (:existing-org ctx)
                     (:existing-board ctx)
                     (:updated-entry ctx)
                     (:existing-comments ctx)
                     (reaction-res/aggregate-reactions (:existing-reactions ctx))
                     (select-keys ctx [:access-level :role])
                     (-> ctx :user :user-id)))
    :delete (fn [ctx] (entry-rep/render-entry
                       (:existing-org ctx)
                       (:existing-board ctx)
                       (:updated-entry ctx)
                       (:existing-comments ctx)
                       (reaction-res/aggregate-reactions (:existing-reactions ctx))
                       (select-keys ctx [:access-level :role])
                       (-> ctx :user :user-id)))})
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-response (:reason ctx))))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
     ;; Replies
     (OPTIONS (entry-urls/poll-replies ":org-slug" ":board-slug" ":entry-uuid" ":poll-uuid")
       [org-slug board-slug entry-uuid poll-uuid]
       (pool/with-pool [conn db-pool]
         (poll-replies conn org-slug board-slug entry-uuid poll-uuid)))
     (POST (entry-urls/poll-replies ":org-slug" ":board-slug" ":entry-uuid" ":poll-uuid")
       [org-slug board-slug entry-uuid poll-uuid]
       (pool/with-pool [conn db-pool]
         (poll-replies conn org-slug board-slug entry-uuid poll-uuid)))
      ;; Reply
     (OPTIONS (entry-urls/poll-reply ":org-slug" ":board-slug" ":entry-uuid" ":poll-uuid" ":reply-id")
       [org-slug board-slug entry-uuid poll-uuid reply-id]
       (pool/with-pool [conn db-pool]
         (poll-reply conn org-slug board-slug entry-uuid poll-uuid reply-id)))
     (DELETE (entry-urls/poll-reply ":org-slug" ":board-slug" ":entry-uuid" ":poll-uuid" ":reply-id")
       [org-slug board-slug entry-uuid poll-uuid reply-id]
       (pool/with-pool [conn db-pool]
         (poll-reply conn org-slug board-slug entry-uuid poll-uuid reply-id)))
     ;; Vote
     (OPTIONS (entry-urls/poll-reply-vote ":org-slug" ":board-slug" ":entry-uuid" ":poll-uuid" ":reply-id")
       [org-slug board-slug entry-uuid poll-uuid reply-id]
       (pool/with-pool [conn db-pool]
         (poll-vote conn org-slug board-slug entry-uuid poll-uuid reply-id)))
     (POST (entry-urls/poll-reply-vote ":org-slug" ":board-slug" ":entry-uuid" ":poll-uuid" ":reply-id")
       [org-slug board-slug entry-uuid poll-uuid reply-id]
       (pool/with-pool [conn db-pool]
         (poll-vote conn org-slug board-slug entry-uuid poll-uuid reply-id)))
     (DELETE (entry-urls/poll-reply-vote ":org-slug" ":board-slug" ":entry-uuid" ":poll-uuid" ":reply-id")
       [org-slug board-slug entry-uuid poll-uuid reply-id]
       (pool/with-pool [conn db-pool]
         (poll-vote conn org-slug board-slug entry-uuid poll-uuid reply-id))))))