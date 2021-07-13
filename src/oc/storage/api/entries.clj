(ns oc.storage.api.entries
  "Liberator API for entry resources."
  (:require [clojure.string :as s]
            [defun.core :refer (defun-)]
            [if-let.core :refer (if-let*)]
            [taoensso.timbre :as timbre]
            [clojure.set :as clj-set]
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
            [oc.storage.resources.label :as label-res]
            [oc.storage.resources.reaction :as reaction-res]
            [oc.storage.urls.org :as org-urls]
            [oc.storage.urls.entry :as entry-urls]))

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

(defn draft-entry-exists? [conn ctx org-slug board-slug entry-uuid user]
  (if-let* [_slugs? (and (slugify/valid-slug? org-slug)
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
                           (= :draft (keyword (:status entry)))) ; sanity check
            access-level (or (access/access-level-for org board user) {:access-level :public})]
    (merge access-level
           {:existing-org (api-common/rep org)
            :existing-board (api-common/rep board)
            :existing-entry (api-common/rep entry)})
    false))

(defn published-entry-exists? [conn ctx org-slug board-slug-or-uuid entry-uuid user]
  (if-let* [_entry-id (lib-schema/unique-id? entry-uuid)
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
                          (entry-res/list-reactions-for-entry conn (:uuid entry)))
            _matches? (and (= org-uuid (:org-uuid entry))
                           (= org-uuid (:org-uuid board))
                           (not= :draft (keyword (:status entry)))) ; sanity check
            access-level (or (access/access-level-for org board user) {:access-level :public})]
    (merge access-level
           {:existing-org (api-common/rep org) :existing-board (api-common/rep board)
            :existing-entry (api-common/rep entry) :existing-comments (api-common/rep comments)
            :existing-reactions (api-common/rep reactions)})
    false))

(defn entry-exists? [conn ctx org-slug board-slug-or-uuid entry-uuid user]
  (if (->> entry-uuid
           (entry-res/get-entry conn)
           :status
           keyword
           (= :draft))
    (draft-entry-exists? conn ctx org-slug board-slug-or-uuid entry-uuid user)
    (published-entry-exists? conn ctx org-slug board-slug-or-uuid entry-uuid user)))

(defn- secure-entry-exists? [conn ctx org-slug secure-uuid user]
  (if-let* [org (or (:existing-org ctx)
                    (org-res/get-org conn org-slug))
            org-uuid (:uuid org)
            entry (or (:existing-entry ctx)
                      (entry-res/get-entry-by-secure-uuid conn org-uuid secure-uuid))
            board (board-res/get-board conn (:board-uuid entry))
            comments (or (:existing-comments ctx)
                         (entry-res/list-comments-for-entry conn (:uuid entry)))
            reactions (or (:existing-reactions ctx)
                          (entry-res/list-reactions-for-entry conn (:uuid entry)))
            _matches? (= org-uuid (:org-uuid board)) ; sanity check
            access-level (or (access/access-level-for org board user) {:access-level :public})]
    (merge access-level
           {:existing-org (api-common/rep org)
            :existing-board (api-common/rep board)
            :existing-entry (api-common/rep entry)
            :existing-comments (api-common/rep comments)
            :existing-reactions (api-common/rep reactions)})
    false))

(defn label-exists? [conn ctx org-slug board-slug entry-uuid label-uuid user]
  (if-let* [{existing-entry :existing-entry
             existing-org :existing-org
             :as next-ctx} (entry-exists? conn ctx org-slug board-slug entry-uuid user)
            existing-label (label-res/get-label conn label-uuid)]
    (merge next-ctx {:existing-label (api-common/rep existing-label)})
    false))

(defn entry-label-exists? [conn ctx org-slug board-slug entry-uuid label-slug-or-uuid user]
  (if-let* [{existing-entry :existing-entry
             existing-org :existing-org
             :as next-ctx} (entry-exists? conn ctx org-slug board-slug entry-uuid user)
            existing-entry-label (some #(when ((set [(:uuid %) (:slug %)]) label-slug-or-uuid) %) (:labels existing-entry))]
    (merge next-ctx {:existing-entry-label (api-common/rep existing-entry-label)
                     :existing-label (api-common/rep (label-res/get-label conn label-slug-or-uuid))})
    false))

(defn create-publisher-board [conn org author]
  (let [created-board (board-res/create-publisher-board! conn (:uuid org) author)]
    (notification/send-trigger! (notification/->trigger :add org {:new created-board} author nil))
    created-board))

(defn- valid-entry-labels? [entry-labels]
  (<= (count entry-labels) config/max-entry-labels))

(defn- valid-entry-label-add? [conn entry-uuid label-slug-or-uuid]
  (let [entry-data (entry-res/get-entry conn entry-uuid)
        entry-labels (:labels entry-data)
        label-already-in-entry? ((set (mapcat #(vec [(:uuid %) (:slug %)]) entry-labels)) label-slug-or-uuid)
        entry-labels-count (count entry-labels)
        can-add-label? (<= (inc entry-labels-count) config/max-entry-labels)]
    (cond (not entry-data) ;; Entry not found, will fail existance later
          true
          label-already-in-entry? ;; No need to error
          true
          can-add-label? ;; Good!
          true
          :else
          [false {:reason (format "Entry %s has already %d the maximum allowed labels (max: %d)" entry-uuid (count (:labels entry-data)) config/max-entry-labels)}])))

(defn- entry-labels-error [entry-labels]
  (format "Too many labels (%d): %s. Max allowed is %d"
          (count entry-labels)
          (s/join " | " (map :name entry-labels))
          config/max-entry-labels))

(defn- valid-new-entry? [conn org-slug board-slug ctx]
  (let [org (org-res/get-org conn org-slug)
        board (board-res/get-board conn (:uuid org) board-slug)
        entry-map (:data ctx)
        author (:user ctx)
        existing-board (cond
                        board
                        board

                        (and (not board)
                             (:publisher-board entry-map)
                             (= (:user-id author) (:board-slug entry-map)))
                        (create-publisher-board conn org author))]
    (cond (not board)
          [false, {:reason "Invalid board."}] ; couldn't find the specified board
          (not (valid-entry-labels? (:labels entry-map)))
          [false, {:reason (entry-labels-error (:labels entry-map))}]
          :else
          (try
            ;; Create the new entry from the URL and data provided
            (let [clean-entry-map (dissoc entry-map :publisher-board)
                  new-entry (entry-res/->entry conn (:uuid existing-board) clean-entry-map author)]
              {:new-entry (api-common/rep new-entry)
              :existing-board (api-common/rep existing-board)
              :existing-org (api-common/rep org)})
            (catch clojure.lang.ExceptionInfo e
              [false, {:reason (.getMessage e)}]))))) ; Not a valid entry

(defn- clean-poll-reply
  "Copy the votes from the existing poll's reply if any, into the new reply."
  [existing-poll reply-id reply-data]
  (let [existing-reply (get existing-poll reply-id)
        reply-votes (if existing-reply (:votes existing-reply) (:votes reply-data))]
    (assoc reply-data :votes reply-votes)))

(defn- clean-poll-for-patch
  "Update a poll without overriding the votes but preserve the added/removed/updated replies."
  [existing-entry poll-data]
  (if-let [existing-poll (get-in existing-entry [:polls (:poll-uuid poll-data)])]
    ;; Make sure we keep the votes that are currently saved,
    ;; client can't update votes with an entry patch
    ;; but can add/delete/update replies.
    (let [updated-replies (map (fn [[reply-id reply-data]]
                                 (clean-poll-reply existing-poll reply-id reply-data))
                           (:replies poll-data))]
      (assoc poll-data :replies (zipmap (map (comp keyword :reply-id) updated-replies)
                                         updated-replies)))
    poll-data))

(defn- clean-polls-for-patch
  "Given the existing entry and the patched entry, clean the polls contained in the
   the new data."
  [entry existing-entry]
  (update entry :polls (fn [polls]
    (let [updated-polls (map (partial clean-poll-for-patch existing-entry) (vals polls))
          poll-uuids (map (comp keyword :poll-uuid) updated-polls)]
      (zipmap poll-uuids updated-polls)))))

(defn- valid-entry-update? [conn entry-uuid entry-props user entry-publish?]
  (if-let [existing-entry (entry-res/get-entry conn entry-uuid)]
    ;; Merge the existing entry with the new updates
    (let [org (org-res/get-org conn (:org-uuid existing-entry))
          new-board-slug (:board-slug entry-props) ; check if they are moving the entry
          old-board (board-res/get-board conn (:board-uuid existing-entry))
          moving-board? (not= (:slug old-board) new-board-slug)
          new-board* (board-res/get-board conn (:uuid org) new-board-slug)
          new-board (cond
                      ;; Entry not moved from old board
                      (and new-board-slug
                           (= new-board-slug (:slug old-board)))
                      old-board
                      ;; Entry moved to another existing board
                      (and new-board-slug
                           (not= new-board-slug (:slug old-board))
                           (map? new-board*))
                      new-board*
                      ;; Entry moved to a new board: if the board doesn't exists
                      ;; it means it's a publisher-board since the endpoint
                      ;; needed to create a new board with entries is another
                      (and new-board-slug
                           (not= new-board-slug (:slug old-board))
                           (not new-board*))
                      (create-publisher-board conn org user))
          clean-entry-props (cond-> entry-props
                              moving-board? (assoc :board-uuid (:uuid new-board))
                              (not moving-board?) (dissoc :board-uuid)
                              true (-> (dissoc :publisher-board)
                                       (update :status #(if entry-publish? :published %))))
          updated-entry (-> existing-entry
                         (merge (entry-res/ignore-props clean-entry-props))
                         (update :attachments #(entry-res/timestamp-attachments %))
                         (clean-polls-for-patch existing-entry))
          ctx-base (if moving-board?
                     {:moving-board (api-common/rep old-board)}
                     {})]
      (cond (not (lib-schema/valid? common-res/Entry updated-entry))
            [false, {:updated-entry (api-common/rep updated-entry)}] ; invalid update
            (not (valid-entry-labels? (:labels entry-props)))
            [false, {:reason (entry-labels-error (:labels entry-props))}]
            :else
            (merge ctx-base
                   {:existing-entry (api-common/rep existing-entry)
                    :existing-board (api-common/rep new-board)
                    :existing-org (api-common/rep org)
                    :updated-entry (api-common/rep updated-entry)})))

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
            entries (entry-res/list-entries-by-board conn board {})]
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
          update-visibility-fn #(cond-> (or % {})
                                 ;; dismiss
                                 (= action-type :dismiss) (assoc :dismiss-at dismiss-at)
                                 ;; unread
                                 (= action-type :unread) (dissoc :follow)
                                 (= action-type :unread) (assoc :unfollow false)
                                 ;; follow
                                 (= action-type :follow) (dissoc :follow)
                                 (= action-type :follow) (assoc :unfollow false)
                                 ;; unfollow
                                 (= action-type :unfollow) (dissoc :follow)
                                 (= action-type :unfollow) (assoc :unfollow true))
          update-visibility-key [:user-visibility (keyword (:user-id user))]
          updated-entry (update-in existing-entry update-visibility-key update-visibility-fn)]
      (timbre/info "User visibility updated:" (get-in updated-entry update-visibility-key))
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

(defn malformed-label-uuids?
  "Read in the body param from the request and make sure it's a non-blank string
  that corresponds to a list of UUIDs of the labels to add/remove."
  [ctx]
  (try
    (if-let* [labels-change-map (slurp (get-in ctx [:request :body]))
              valid? (and (map? labels-change-map)
                          (or (and (seq (:add labels-change-map))
                                   (every? lib-schema/unique-id? (:add labels-change-map)))
                              (and (seq (:remove labels-change-map))
                                   (every? lib-schema/unique-id? (:remove labels-change-map)))))]
             [false {:label-changes labels-change-map}]
             true)
    (catch Exception e
      (timbre/warn "Request body not processable as valid entry label changes: " e)
      true)))

;; ----- Actions -----

(defn- update-user-labels [conn user old-entry new-entry]
  (timbre/debugf "Update user labels for entry %s with status %s" (:uuid new-entry) (:status new-entry))
  (when (= (keyword (:status new-entry)) :published)
    (let [existing-labels (set (map :uuid (:labels old-entry)))
          updated-labels (set (map :uuid (:labels new-entry)))
          remove-labels? (= (keyword (:status old-entry)) :published)
          added-labels (clj-set/difference updated-labels existing-labels)
          removed-labels (when remove-labels?
                           (clj-set/difference existing-labels updated-labels))]
      (timbre/debugf "Updating labels for user %s on entry %s" (:user-id user) (:uuid new-entry))
      (timbre/tracef "Existing labels: %s, updated labels: %s remove? %s, add? %s, removed-labels %s" existing-labels updated-labels remove-labels? added-labels removed-labels)
      (when (seq added-labels)
        (timbre/infof "Adding used labels %s to user %s" added-labels (:user-id user))
        (label-res/labels-used-by! conn added-labels (:org-uuid new-entry) user))
      (when (seq removed-labels)
        (timbre/infof "Removing used labels %s to user %s" removed-labels (:user-id user))
        (label-res/labels-unused-by! conn removed-labels (:org-uuid new-entry) user)))))

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
  (timbre/infof "Auto sharing entry %s" (:uuid entry-result))
  (let [slack-channels* (:slack-mirror (:existing-board ctx))
        slack-channels (if (map? slack-channels*) [slack-channels*] slack-channels*)]
    (doseq [slack-channel slack-channels]
      (timbre/debugf "Sharing to %s/%s" (:slack-org-id slack-channel) (:channel-id slack-channel))
      (if (bot/has-slack-bot-for? (:slack-org-id slack-channel) (:user ctx))
        (let [share-request {:medium "slack"
                             :note ""
                             :shared-at (db-common/current-timestamp)
                             :channel slack-channel}
              share-ctx (-> ctx
                            (assoc :share-requests (list share-request))
                            (assoc :existing-entry (api-common/rep entry-result))
                            (assoc :auto-share true))]
          (share-entry conn share-ctx (:uuid entry-result)))
        (timbre/infof "")))))

(defn undraft-board [conn _user _org board]
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
      (update-user-labels conn user nil entry-result)
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
      (update-user-labels conn user entry updated-entry)
      ;; If we are moving the entry from a draft board, check if we need to remove the board itself.
      (when old-board
        (let [remaining-entries (entry-res/list-all-entries-by-board conn old-board)]
          (board-res/maybe-delete-draft-board conn org old-board remaining-entries user)))
      (timbre/info "Updated entry for:" entry-for)
      (notification/send-trigger! (notification/->trigger :update org board {:old entry :new updated-result} user nil (api-common/get-change-client-id ctx)))
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

(defn- publish-entry [conn ctx entry-for]
  (timbre/info "Publishing entry for:" entry-for)
  (if-let* [user (:user ctx)
            org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            updated-entry (:updated-entry ctx)
            final-entry (entry-res/publish-entry! conn (:uuid updated-entry) updated-entry org user)]
    (let [old-board (:moving-board ctx)]
      (update-user-labels conn user entry updated-entry)
      (undraft-board conn user org board)
      ;; If we are moving the entry from a draft board, check if we need to remove the board itself.
      (when old-board
        (let [remaining-entries (entry-res/list-all-entries-by-board conn old-board)]
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
        (let [remaining-entries (entry-res/list-all-entries-by-board conn board)]
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

;; Label add/remove

(defn- add-label [conn ctx label-slug-or-uuid entry-for]
  (timbre/infof "Adding label %s by user %s to entry %s" label-slug-or-uuid (-> ctx :user :user-id) entry-for)
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            label (:existing-label ctx)
            user (:user ctx)]
    (if-let [updated-entry (entry-res/add-label! conn (:uuid entry) (:uuid label) user)]
      (do
        (timbre/debugf "Label %s added by user %s on entry %s" label-slug-or-uuid (-> ctx :user :user-id) (:uuid entry))
        {:updated-entry (api-common/rep updated-entry)})
      (do
        (timbre/infof "Label not added, it probably means the entry %s already has %s" (:uuid entry) label-slug-or-uuid)
        {:existing-entry (api-common/rep entry)}))
    (do
      (timbre/errorf "Failed adding label %s to entry %s by user %s" label-slug-or-uuid (-> ctx :existing-entry :uuid) (-> ctx :user :user-id))
      false)))

(defn- remove-label [conn ctx label-slug-or-uuid entry-for]
  (timbre/infof "Removing label %s by user %s to entry %s" label-slug-or-uuid (-> ctx :user :user-id) entry-for)
  (if-let* [org (:existing-org ctx)
            board (:existing-board ctx)
            entry (:existing-entry ctx)
            label (:existing-entry-label ctx)
            user (:user ctx)]
    (if-let [updated-entry (entry-res/remove-label! conn (:uuid entry) (:uuid label) user)]
      (do
        (timbre/debugf "Label %s removed by user %s on entry %s" label-slug-or-uuid (-> ctx :user :user-id) (:uuid entry))
        {:updated-entry (api-common/rep updated-entry)})
      (do
        (timbre/infof "Label not removed, it probably means the entry %s already has %s" (:uuid entry) label-slug-or-uuid)
        {:existing-entry (api-common/rep entry)}))
    (do
      (timbre/errorf "Failed removing label %s to entry %s by user %s" label-slug-or-uuid (-> ctx :existing-entry :uuid) (-> ctx :user :user-id))
      false)))

(defn- toggle-label-uuids [conn ctx entry-for]
  (let [existing-entry (:existing-entry ctx)
        user (:user ctx)
        {add-label-uuids :add remove-label-uuids :remove} (:label-changes ctx)]
    (when (seq add-label-uuids)
      (timbre/infof "Adding labels %s to entry %s by user %s" add-label-uuids entry-for (:user-id user))
      (entry-res/add-labels! conn (:uuid existing-entry) add-label-uuids user))
    (when (seq remove-label-uuids)
      (timbre/infof "Removing labels %s to entry %s by user %s" remove-label-uuids entry-for (:user-id user))
      (entry-res/remove-labels! conn (:uuid existing-entry) remove-label-uuids user))
    ;; Return the updated entry
    {:updated-entry (entry-res/get-entry conn (:uuid existing-entry))}))

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
  :exists? (fn [ctx] (entry-exists? conn ctx org-slug board-slug-or-uuid entry-uuid (:user ctx)))

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
                      (select-keys ctx [:access-level :role])
                      (-> ctx :user :user-id)))
    :patch (fn [ctx] (entry-rep/render-entry
                        (:existing-org ctx)
                        (:existing-board ctx)
                        (:updated-entry ctx)
                        (:existing-comments ctx)
                        (reaction-res/aggregate-reactions (:existing-reactions ctx))
                        (select-keys ctx [:access-level :role])
                        (-> ctx :user :user-id)))})
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-handler (merge ctx {:reason (str (schema/check common-res/Entry (:updated-entry ctx)))}))))

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
    :get (partial entry-list-for-board conn org-slug board-slug-or-uuid)
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
                                (entry-urls/entry org-slug (:slug existing-board) (:uuid new-entry))
                                (entry-rep/render-entry (:existing-org ctx) (:existing-board ctx) new-entry [] [] {:access-level :author} (-> ctx :user :user-id))
                                mt/entry-media-type))))

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
                                               (select-keys ctx [:access-level :role])
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
  :exists? (fn [ctx] (draft-entry-exists? conn ctx org-slug board-slug entry-uuid (:user ctx)))

  ;; Actions
  :post! (fn [ctx] (publish-entry conn ctx (s/join " " [org-slug board-slug entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (:updated-entry ctx)
                                               [] ; no comments
                                               [] ; no reactions
                                               (select-keys ctx [:access-level :role])
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
  :exists? (fn [ctx] (published-entry-exists? conn ctx org-slug board-slug entry-uuid (:user ctx)))

  ;; Actions
  :post! (fn [ctx] (share-entry conn ctx (s/join " " [org-slug board-slug entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (:updated-entry ctx)
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (select-keys ctx [:access-level :role])
                                               (-> ctx :user :user-id)))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-handler (merge ctx {:reason (s/join "\n" (map #(str (schema/check common-res/ShareRequest %)) (:share-requests ctx)))}))))


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
  :exists? (fn [ctx] (secure-entry-exists? conn ctx org-slug secure-uuid (:user ctx)))

  ;; Responses
  :handle-ok (fn [ctx] (let [access-level (:access-level ctx)]
                          (entry-rep/render-entry (:existing-org ctx)
                                                  (:existing-board ctx)
                                                  (:existing-entry ctx)
                                                  (:existing-comments ctx)
                                                  (if (or (= :author access-level) (= :viewer access-level))
                                                    (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                                    [])
                                                  (select-keys ctx [:access-level :role])
                                                  (-> ctx :user :user-id)
                                                  :secure))))

(defresource bookmark [conn org-slug board-slug entry-uuid]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken
  :allowed-methods [:options :post :delete]

  ;; Authorization
  :allowed? (by-method {:options true
                        :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  ;; Media type client accepts
  :available-media-types (by-method {:post [mt/entry-media-type]
                                     :delete [mt/entry-media-type]})
  :handle-not-acceptable (by-method {:post (api-common/only-accept 406 mt/entry-media-type)
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
  :exists? (fn [ctx] (published-entry-exists? conn ctx org-slug board-slug entry-uuid (:user ctx)))

  ;; Actions
  :post! (fn [ctx] (add-bookmark conn ctx (s/join " " [org-slug board-slug entry-uuid])))
  :delete! (fn [ctx] (remove-bookmark conn ctx (s/join " " [org-slug board-slug entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (or (:updated-entry ctx) (:existing-entry ctx))
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (select-keys ctx [:access-level :role])
                                               (-> ctx :user :user-id)))
  :handle-unprocessable-entity (fn [ctx]
                                 (api-common/unprocessable-entity-handler (merge ctx {:reason (str (schema/check common-res/Entry (:updated-entry ctx)))}))))

(defresource inbox [conn org-slug board-slug entry-uuid action-type]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken
  :allowed-methods [:options :post]

  ;; Authorization
  :allowed? (by-method {:options true
                        :post (fn [ctx] (access/allow-members conn org-slug (:user ctx)))})

  :known-content-type? (by-method {:options true
                                   :post (fn [ctx] (api-common/known-content-type? ctx "text/plain"))})

  ;; Media type client accepts
  :available-media-types (by-method {:post [mt/entry-media-type]})
  :handle-not-acceptable (by-method {:post (api-common/only-accept 406 mt/entry-media-type)})

  ;; Data handling
  :new? false
  :respond-with-entity? true

  ;; Validations
  :processable? (by-method {:options true
                            :post (fn [ctx] (valid-entry-inbox-update? conn ctx org-slug entry-uuid (:user ctx) action-type))})

  ;; Possibly no data to handle
  :malformed? false ; allow nil

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx] (published-entry-exists? conn ctx org-slug board-slug entry-uuid (:user ctx)))

  ;; Actions
  :post! (fn [ctx]
           (update-user-visibility conn ctx (s/join " " [org-slug board-slug entry-uuid]) action-type))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (:updated-entry ctx)
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (select-keys ctx [:access-level :role])
                                               (-> ctx :user :user-id)))
  :handle-unprocessable-entity (fn [ctx]
                                 (api-common/unprocessable-entity-handler (merge ctx {:reason (:updated-entry ctx)}))))

(defresource toggle-label [conn org-slug board-slug entry-uuid label-slug-or-uuid]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken
  :allowed-methods [:options :post :delete]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-authors conn org-slug board-slug (:user ctx)))
    :delete (fn [ctx] (access/allow-authors conn org-slug board-slug (:user ctx)))})

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
  :processable? (by-method {
                 :options true
                 :post (fn [ctx] (valid-entry-label-add? conn entry-uuid label-slug-or-uuid))
                 :delete true})

  ;; Possibly no data to handle
  :malformed? false ; allow nil

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (by-method {:post (fn [ctx]
                               (label-exists? conn ctx org-slug board-slug entry-uuid label-slug-or-uuid(:user ctx)))
                       :delete (fn [ctx]
                                 (entry-label-exists? conn ctx org-slug board-slug entry-uuid label-slug-or-uuid (:user ctx)))})

  ;; Actions
  :post! (fn [ctx] (add-label conn ctx label-slug-or-uuid (s/join " " [org-slug board-slug entry-uuid label-slug-or-uuid])))
  :delete! (fn [ctx] (remove-label conn ctx label-slug-or-uuid (s/join " " [org-slug board-slug entry-uuid label-slug-or-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (or (:updated-entry ctx) (:existing-entry ctx))
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (select-keys ctx [:access-level :role])
                                               (-> ctx :user :user-id))))

(defresource toggle-labels [conn org-slug board-slug entry-uuid]
  (api-common/open-company-authenticated-resource config/passphrase) ; verify validity and presence of required JWToken
  :allowed-methods [:options :post]

  ;; Authorization
  :allowed? (by-method {
    :options true
    :post (fn [ctx] (access/allow-authors conn org-slug board-slug (:user ctx)))})

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
  :malformed? (by-method {:options false
                          :post (fn [ctx] (malformed-label-uuids? ctx))})
  :known-content-type? (by-method {:options true
                                   :post (fn [ctx] (api-common/known-content-type? ctx mt/entry-label-changes-media-type))})

  ;; Existentialism
  :can-post-to-missing? false
  :exists? (fn [ctx] (entry-exists? conn ctx org-slug board-slug entry-uuid (:user ctx)))

  ;; Actions
  :post! (fn [ctx] (toggle-label-uuids conn ctx (s/join " " [org-slug board-slug entry-uuid])))

  ;; Responses
  :handle-ok (fn [ctx] (entry-rep/render-entry (:existing-org ctx)
                                               (:existing-board ctx)
                                               (or (:updated-entry ctx) (:existing-entry ctx))
                                               (:existing-comments ctx)
                                               (reaction-res/aggregate-reactions (:existing-reactions ctx))
                                               (select-keys ctx [:access-level :role])
                                               (-> ctx :user :user-id)))
  :handle-unprocessable-entity (fn [ctx]
    (api-common/unprocessable-entity-handler (merge ctx {:reason (str (schema/check common-res/Entry (:updated-entry ctx)))}))))

;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; Delete sample posts
      (OPTIONS (org-urls/sample-entries ":org-slug")
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      (OPTIONS (str (org-urls/sample-entries ":org-slug") "/")
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      (DELETE (org-urls/sample-entries ":org-slug")
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      (DELETE (str (org-urls/sample-entries ":org-slug") "/")
        [org-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug nil)))
      ;; Secure UUID access
      (ANY (entry-urls/secure-entry ":org-slug" ":secure-uuid")
        [org-slug secure-uuid]
        (pool/with-pool [conn db-pool]
          (entry-access conn org-slug secure-uuid)))
      (ANY (str (entry-urls/secure-entry ":org-slug" ":secure-uuid") "/")
        [org-slug secure-uuid]
        (pool/with-pool [conn db-pool]
          (entry-access conn org-slug secure-uuid)))
      ;; Entry list operations
      (ANY (entry-urls/entries ":org-slug" ":board-slug")
        [org-slug board-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug board-slug)))
      (ANY (str (entry-urls/entries ":org-slug" ":board-slug") "/")
        [org-slug board-slug]
        (pool/with-pool [conn db-pool]
          (entry-list conn org-slug board-slug)))
      ;; Entry operations
      (ANY (entry-urls/entry ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (entry conn org-slug board-slug entry-uuid)))
      (ANY (entry-urls/publish ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (publish conn org-slug board-slug entry-uuid)))
      (ANY (str (entry-urls/publish ":org-slug" ":board-slug" ":entry-uuid") "/")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (publish conn org-slug board-slug entry-uuid)))
      (ANY (entry-urls/revert ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (revert-version conn org-slug board-slug entry-uuid)))
      (ANY (str (entry-urls/revert ":org-slug" ":board-slug" ":entry-uuid") "/")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (revert-version conn org-slug board-slug entry-uuid)))
      (ANY (entry-urls/share ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (share conn org-slug board-slug entry-uuid)))
      (ANY (str (entry-urls/share ":org-slug" ":board-slug" ":entry-uuid") "/")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (share conn org-slug board-slug entry-uuid)))
      (ANY (entry-urls/bookmark ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (bookmark conn org-slug board-slug entry-uuid)))
      (ANY (str (entry-urls/bookmark ":org-slug" ":board-slug" ":entry-uuid") "/")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (bookmark conn org-slug board-slug entry-uuid)))
      (ANY (entry-urls/inbox-follow ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :follow)))
      (ANY (str (entry-urls/inbox-follow ":org-slug" ":board-slug" ":entry-uuid") "/")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :follow)))
      (ANY (entry-urls/inbox-unfollow ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :unfollow)))
      (ANY (str (entry-urls/inbox-unfollow ":org-slug" ":board-slug" ":entry-uuid") "/")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (inbox conn org-slug board-slug entry-uuid :unfollow)))
      ;; Multiple labels
      (ANY (entry-urls/labels ":org-slug" ":board-slug" ":entry-uuid")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (toggle-labels conn org-slug board-slug entry-uuid)))
      (ANY (str (entry-urls/labels ":org-slug" ":board-slug" ":entry-uuid") "/")
        [org-slug board-slug entry-uuid]
        (pool/with-pool [conn db-pool]
          (toggle-labels conn org-slug board-slug entry-uuid)))
      ;; Single label
      (ANY (entry-urls/label ":org-slug" ":board-slug" ":entry-uuid" ":label-slug-or-uuid")
        [org-slug board-slug entry-uuid label-slug-or-uuid]
        (pool/with-pool [conn db-pool]
          (toggle-label conn org-slug board-slug entry-uuid label-slug-or-uuid)))
      (ANY (str (entry-urls/label ":org-slug" ":board-slug" ":entry-uuid" ":label-slug-or-uuid") "/")
        [org-slug board-slug entry-uuid label-slug-or-uuid]
        (pool/with-pool [conn db-pool]
          (toggle-label conn org-slug board-slug entry-uuid label-slug-or-uuid))))))
