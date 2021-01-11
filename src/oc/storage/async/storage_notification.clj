(ns oc.storage.async.storage-notification
  "
  Consume storage notifications about new post requests.

  This SQS queue is feed by other services like Notify (in case of new mention on a post or comment).
  "
  (:require
   [clojure.core.async :as async :refer (<!! >!!)]
   [defun.core :refer (defun-)]
   [if-let.core :refer (if-let*)]
   [cheshire.core :as json]
   [oc.lib.sqs :as sqs]
   [oc.lib.db.pool :as pool]
   [oc.lib.sentry.core :as sentry]
   [taoensso.timbre :as timbre]
   [oc.storage.resources.entry :as entry-res]
   [oc.storage.resources.org :as org-res]
   [oc.storage.resources.board :as board-res]
   [oc.storage.async.notification :as notification]))

;; ----- core.async -----

(defonce storage-notification-chan (async/chan 10000)) ; buffered channel

(defonce storage-notification-go (atom nil))

;; ----- SQS handling -----

(defun- action-event

  "
  Callback from Interaction to follow/unfollow posts for mentioned or commenting users...
  
  {:type 'inbox-action'
   :sub-type 'follow'
   :item-id '4321-4321-4321'
   :users [{:user-id '1234-1234-1234'
            :name '...'
            :avatar-url ''},
           ...]}

  {:type 'inbox-action'
   :sub-type 'unfollow'
   :item-id '4321-4321-4321'
   :users [{:user-id '1234-1234-1234'
            :name '...'
            :avatar-url ''},
           ...]}

  {:type 'inbox-action'
   :sub-type 'dismiss'
   :item-id '4321-4321-4321'
   :users [{:user-id '1234-1234-1234'
            :name '...'
            :avatar-url ''},
           ...]
   :dismiss-at '2019-11-29T14:26:12Z'}

  {:type 'inbox-action'
   :sub-type 'comment-add'
   :item-id '4321-4321-4321'
   :users [{:user-id '1234-1234-1234'}]}
  "

  ([db-pool body :guard #(and (= (:type %) "inbox-action")
                              (or (= (:sub-type %) "follow")
                                  (= (:sub-type %) "unfollow")))]
  (timbre/debug "Got inbox-action message of type 'follow' or 'unfollow' from Interaction:" body)
  (pool/with-pool [conn db-pool]
    (if-let* [entry-uuid (:item-id body)
              users (:users body)
              entry-data (entry-res/get-entry conn entry-uuid)
              org (org-res/get-org conn (:org-uuid entry-data))
              board (board-res/get-board conn (:board-uuid entry-data))
              user-visibility (get entry-data :user-visibility {})
              sub-type (keyword (:sub-type body))
              new-entry-data (reduce (fn [uv user]
                                      (update-in uv [:user-visibility (keyword (:user-id user))]
                                       #(cond-> (or % {})
                                         ;; follow
                                         (= sub-type :follow) (dissoc :unfollow)
                                         (= sub-type :follow) (assoc :follow true)
                                         ;; unfollow
                                         (= sub-type :unfollow) (dissoc :follow)
                                         (= sub-type :unfollow) (assoc :unfollow true))))
                                     entry-data users)
              entry-result (entry-res/update-entry-no-user! conn entry-uuid new-entry-data)]
      (do
        (when (= (:status entry-data) "published")
          (doseq [user users]
            (notification/send-trigger! (notification/->trigger :follow org board
             {:old entry-data
              :new entry-result
              :inbox-action {:follow true}} user nil))))
        (timbre/info "Handled inbox-action " (:sub-type body) " for entry:" entry-result))
      (timbre/error "Failed handling" (:sub-type body) "message for item:" (:item-id body) "and users:" (mapv :user-id (:users body))))))

  ([db-pool body :guard #(and (= (:type %) "inbox-action")
                              (= (:sub-type %) "dismiss"))]
  (timbre/debug "Got inbox-action message of type 'dismiss' from Interaction:" body)
  (pool/with-pool [conn db-pool]
    (if-let* [entry-uuid (:item-id body)
              users (:users body)
              dismiss-at (:dismiss-at body)
              entry-data (entry-res/get-entry conn entry-uuid)
              org (org-res/get-org conn (:org-uuid entry-data))
              board (board-res/get-board conn (:board-uuid entry-data))
              user-visibility (get entry-data :user-visibility {})
              new-entry-data (reduce (fn [uv user]
                                      (update-in uv [:user-visibility (keyword (:user-id user))]
                                       #(merge % {:dismiss-at dismiss-at})))
                                     entry-data users)
              entry-result (entry-res/update-entry-no-user! conn entry-uuid new-entry-data)]
      (do
        (when (= (:status entry-data) "published")
          (doseq [user users]
            (notification/send-trigger! (notification/->trigger :follow org board
             {:old entry-data
              :new entry-result
              :inbox-action {:follow true}} user nil))))
        (timbre/info "Handled inbox-action dismiss for entry:" entry-result))
      (timbre/error "Failed handling follow message for item:" (:item-id body) "and users:" (mapv :user-id (:users body))))))

  ([db-pool body :guard #(and (= (:type %) "inbox-action")
                              (= (:sub-type %) "comment-add"))]
  (timbre/debug "Got inbox-action message of type 'comment-add' from Interaction:" body)
  (pool/with-pool [conn db-pool]
    (if-let* [entry-uuid (:item-id body)
              entry-data (entry-res/get-entry conn entry-uuid)
              comment-author (first (:users body))
              org (org-res/get-org conn (:org-uuid entry-data))
              board (board-res/get-board conn (:board-uuid entry-data))
              user-visibility (get entry-data :user-visibility {})
              all-users (if (= (:access board) "private")
                          (clojure.set/union (set (:authors board)) (set (:viewers board)))
                          (clojure.set/union (set (:authors org)) (set (:viewers org))))
              unfollowing-users (clojure.set/union
                                 (set (remove nil? (map
                                  (fn [[k v]]
                                    (when-not (:unfollow v)
                                      (name k)))
                                  (:user-visibility entry-data))))
                                 (hash-set (:user-id comment-author)))
              following-users (vec (clojure.set/intersection all-users unfollowing-users))]
      ; Send a message to all following users to force refresh inbox
      (do
        (when (= (:status entry-data) "published")
          ;; Send to all following users except the comment publisher
          ;; since inbox won't show it until another user adds a comment
          (timbre/info "Triggering inbox-action/comment-add notification for" following-users)
          (notification/send-trigger! (notification/->trigger :comment-add org board
           {:new entry-data
            :inbox-action {:comment-add true}}
           following-users
           nil)))
        (timbre/info "Handled inbox-action comment-add for entry:" (:uuid entry-data)))
      (timbre/error "Failed handling comment-add message for item:" (:item-id body)))))

  ([_ body] (timbre/debug "Skipped message:" body)))

(defn- read-message-body
  "Try to parse as json, otherwise use read-string."
  [msg]
  (try
    (json/parse-string msg true)
    (catch Exception e
      (read-string msg))))

(defn sqs-handler
  "Handle an incoming SQS message to the storage service."
  [msg done-channel]
  (let [msg-body (read-message-body (:body msg))
        error (if (:test-error msg-body) (/ 1 0) false)] ; a message testing Sentry error reporting
    (timbre/debugf "Received message from SQS (storage notification): %s\n" msg-body)
    (>!! storage-notification-chan msg-body))
  (sqs/ack done-channel msg))

;; ----- Event loop -----

(defn- storage-notification-loop
  "Start a core.async consumer of the storage notification channel."
  [db-pool]
  (reset! storage-notification-go true)
  (async/go
    (while @storage-notification-go
      (timbre/info "Waiting for message on storage notification channel...")
      (let [msg (<!! storage-notification-chan)]
        (timbre/trace "Processing message on storage notification channel...")
        (if (:stop msg)
          (do (reset! storage-notification-go false) (timbre/info "Storage notification stopped."))
          (async/thread
            (try
              (when (= (:type msg) "inbox-action")
                (timbre/trace "Storage notification handling:" msg)
                (action-event db-pool msg))
              (timbre/trace "Processing complete.")
              (catch Exception e
                (timbre/warn e)
                (sentry/capture e)))))))))

;; ----- Component start/stop -----

(defn start
 "Stop the core.async storage notification channel consumer."
 [sys]
 (let [db-pool (-> sys :db-pool :pool)]
   (timbre/info "Starting storage notification...")
   (storage-notification-loop db-pool)))

(defn stop
 "Stop the core.async storage notification channel consumer."
  []
  (when @storage-notification-go
    (timbre/info "Stopping storage notification...")
    (>!! storage-notification-chan {:stop true})))