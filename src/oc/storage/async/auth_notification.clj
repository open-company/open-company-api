(ns oc.storage.async.auth-notification
  "
  Consume auth notifications about newly created users. This SQS queue
  is subscribed to the auth SNS topic.
  "
  (:require
   [clojure.core.async :as async :refer (<!! >!!)]
   [cheshire.core :as json]
   [oc.lib.sqs :as sqs]
   [oc.lib.sentry.core :as sentry]
   [oc.lib.db.pool :as pool]
   [oc.storage.resources.org :as org-res]
   [taoensso.timbre :as timbre]))

;; ----- core.async -----

(defonce auth-notification-chan (async/chan 10000)) ; buffered channel

(defonce auth-notification-go (atom nil))

;; ----- SQS handling -----

(defn new-user-event
  [db-pool body]
  (let [user (:new (:content body))
        teams (:teams user)]
    ;; This event is only for newly created users. Invites will create
    ;; a user.
    (pool/with-pool [conn db-pool]
      ;; if there are no orgs, then it is the first user
      (doseq [team teams]
        (doseq [org (org-res/list-orgs-by-team conn team)]
          ;; A new user will not be created for invites (viewers)
          ;; make a new user a contributor
          (org-res/add-author conn (:slug org) (:user-id user)))))))

(defn- read-message-body
  "Try to parse as json, otherwise use read-string."
  [msg]
  (try
    (json/parse-string msg true)
    (catch Exception e
      (read-string msg))))

(defn sqs-handler
  "Handle an incoming SQS message from the auth service."
  [msg done-channel]
  (let [msg-body (read-message-body (:body msg))
        error (if (:test-error msg-body) (/ 1 0) false)] ; a message testing Sentry error reporting
    (timbre/debugf "Received message from SQS (auth notification): %s\n" msg-body)
    (>!! auth-notification-chan msg-body))
  (sqs/ack done-channel msg))

;; ----- Event loop -----

(defn- auth-notification-loop
  "Start a core.async consumer of the auth notification channel."
  [db-pool]
  (reset! auth-notification-go true)
  (async/go
    (while @auth-notification-go
      (timbre/info "Waiting for message on auth notification channel...")
      (let [msg (<!! auth-notification-chan)]
        (timbre/trace "Processing message on auth notification channel...")
        (if (:stop msg)
          (do (reset! auth-notification-go false) (timbre/info "Auth notification stopped."))
          (async/thread
            (try
              (when (:Message msg) ;; data change SNS message
                (let [msg-parsed (json/parse-string (:Message msg) true)]
                  (new-user-event db-pool msg-parsed)))
              (timbre/trace "Processing complete.")
              (catch Exception e
                (timbre/warn e)
                (sentry/capture e)))))))))

;; ----- Component start/stop -----

(defn start
 "Stop the core.async auth notification channel consumer."
 [sys]
 (let [db-pool (-> sys :db-pool :pool)]
   (timbre/info "Starting auth notification...")
   (auth-notification-loop db-pool)))

(defn stop
 "Stop the core.async auth notification channel consumer."
  []
  (when @auth-notification-go
    (timbre/info "Stopping auth notification...")
    (>!! auth-notification-chan {:stop true})))