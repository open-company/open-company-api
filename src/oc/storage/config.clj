(ns oc.storage.config
  "Namespace for the configuration parameters."
  (:require [environ.core :refer (env)]))

(defn- bool
  "Handle the fact that we may have true/false strings, when we want booleans."
  [val]
  (boolean (Boolean/valueOf val)))

;; ----- System -----

(defonce processors (.availableProcessors (Runtime/getRuntime)))
(defonce core-async-limit (+ 42 (* 2 processors)))

(defonce prod? (= "production" (env :env)))
(defonce intro? (not prod?))

;; ----- Sentry -----

(defonce dsn (or (env :open-company-sentry-storage) false))
(defonce sentry-release (or (env :release) ""))
(defonce sentry-env (or (env :environment) "local"))
(defonce sentry-config {:dsn dsn
                        :release sentry-release
                        :environment sentry-env})

;; ----- Logging (see https://github.com/ptaoussanis/timbre) -----

(defonce log-level (or (env :log-level) :info))

;; ----- RethinkDB -----

(defonce migrations-dir "./src/oc/storage/db/migrations")
(defonce migration-template "./src/oc/storage/assets/migration.template.edn")

(defonce db-host (or (env :db-host) "localhost"))
(defonce db-port (or (env :db-port) 28015))
(defonce db-name (or (env :db-name) "open_company_storage"))
(defonce db-pool-size (or (env :db-pool-size) (- core-async-limit 21))) ; conservative with the core.async limit

(defonce db-map {:host db-host :port db-port :db db-name})
(defonce db-options (flatten (vec db-map))) ; k/v sequence as clj-rethinkdb wants it

;; ----- HTTP server -----

(defonce hot-reload (bool (or (env :hot-reload) false)))
(defonce storage-server-port (Integer/parseInt (or (env :port) "3001")))

;; ----- URLs -----

(defonce host (or (env :local-dev-host) "localhost"))

(defonce auth-server-url (or (env :auth-server-url) (str "http://" host ":3003")))
(defonce interaction-server-url (or (env :interaction-server-url) (str "http://" host ":3002")))
(defonce interaction-server-ws-url (or (env :interaction-server-ws-url) (str "ws://" host ":3002")))
(defonce change-server-url (or (env :change-server-url) (str "http://" host ":3006")))
(defonce change-server-ws-url (or (env :change-server-ws-url) (str "ws://" host ":3006")))
(defonce notify-server-ws-url (or (env :notify-server-ws-url) (str "ws://" host ":3010")))
(defonce reminder-server-url (or (env :reminder-server-url) (str "http://" host ":3011")))

;; ----- Liberator -----

;; see header response, or http://localhost:3001/x-liberator/requests/ for trace results
(defonce liberator-trace (bool (or (env :liberator-trace) false)))
(defonce pretty? (not prod?)) ; JSON response as pretty?

;; ----- AWS SNS / SQS -----

(defonce aws-access-key-id (env :aws-access-key-id))
(defonce aws-secret-access-key (env :aws-secret-access-key))

(defonce aws-sqs-bot-queue (env :aws-sqs-bot-queue)) ; out-bound to send Slack messages
(defonce aws-sqs-email-queue (env :aws-sqs-email-queue)) ; out-bound to send Email
(defonce aws-sqs-auth-queue (env :aws-sqs-auth-queue)) ; in-bound from Auth
(defonce aws-sqs-storage-queue (env :aws-sqs-storage-queue)) ; in-bound posts from bot

;; Only used for bulk updates of the search service, normal real-time updates use SNS
(defonce aws-sqs-search-index-queue (env :aws-sqs-search-index-queue)) ; out-bound to Search

(defonce aws-sns-storage-topic-arn (env :aws-sns-storage-topic-arn)) ; out-bound to listeners
(defonce aws-kinesis-stream-name (or (env :aws-kinesis-stream-name) false))
(defonce aws-kinesis-reindex-stream-name (or (env :aws-kinesis-reindex-stream-name) false))

;; ----- JWT -----

(defonce passphrase (env :open-company-auth-passphrase))

;; ----- OpenCompany -----

(defonce new-org-board-names #{"General"})

(defonce max-reaction-count 3)

(defonce inline-comment-count 10)

(defonce default-activity-limit 10)

(defonce reminders-enabled? (bool (env :reminders-enabled)))

(defonce publisher-board-enabled? (bool (env :publisher-board-enabled)))

;; DynamoDB

(defonce dynamodb-end-point (or (env :dynamodb-end-point) "http://localhost:8000"))

(defonce dynamodb-table-prefix (or (env :dynamodb-table-prefix) "local"))

(defonce dynamodb-opts {
    :access-key (env :aws-access-key-id)
    :secret-key (env :aws-secret-access-key)
    :endpoint dynamodb-end-point
    :table-prefix dynamodb-table-prefix
  })

(defonce seen-home-container-id (or (env :seen-home-container-id) "1111-1111-1111"))
(defonce seen-replies-container-id (or (env :seen-replies-container-id) "1111-1111-2222"))

(defonce unread-days-limit (or (env :unread-days-limit) 30))

(defonce unseen-cap-days (or (env :unseen-cap-days) 0))
