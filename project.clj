(defproject open-company-storage "0.3.0-SNAPSHOT"
  :description "OpenCompany Storage Service"
  :url "https://github.com/open-company/open-company-storage"
  :license {
    :name "GNU Affero General Public License Version 3"
    :url "https://www.gnu.org/licenses/agpl-3.0.en.html"
  }

  :min-lein-version "2.7.1"

  ;; JVM memory
  :jvm-opts ^:replace ["-Xms512m" "-Xmx3072m" "-server"]

  ;; All profile dependencies
  :dependencies [
    ;; Lisp on the JVM http://clojure.org/documentation
    [org.clojure/clojure "1.10.0-RC2"]
    ;; Command-line parsing https://github.com/clojure/tools.cli
    [org.clojure/tools.cli "0.4.1"]
    ;; Web application library https://github.com/ring-clojure/ring
    [ring/ring-devel "1.7.1"]
    ;; Web application library https://github.com/ring-clojure/ring
    ;; NB: clj-time pulled in by oc.lib
    ;; NB: joda-time pulled in by oc.lib via clj-time
    ;; NB: commons-codec pulled in by oc.lib
    [ring/ring-core "1.7.1" :exclusions [clj-time joda-time commons-codec]]
    ;; CORS library https://github.com/jumblerg/ring.middleware.cors
    [jumblerg/ring.middleware.cors "1.0.1"]
    ;; Ring logging https://github.com/nberger/ring-logger-timbre
    ;; NB: com.taoensso/encore pulled in by oc.lib
    ;; NB: com.taoensso/timbre pulled in by oc.lib
    [ring-logger-timbre "0.7.6" :exclusions [com.taoensso/encore com.taoensso/timbre]] 
    ;; Web routing https://github.com/weavejester/compojure
    [compojure "1.6.1"]
    ;; Utility functions https://github.com/weavejester/medley
    [medley "1.0.0"]
    ;; Pretty-print clj and EDN https://github.com/kkinnear/zprint
    [zprint "0.4.13"]
    ;; Library for OC projects https://github.com/open-company/open-company-lib
    [open-company/lib "0.16.30alpha1"]

    ;; In addition to common functions, brings in the following common dependencies used by this project:
    ;; httpkit - HTTP client/server http://www.http-kit.org/
    ;; defun - Erlang-esque pattern matching for Clojure functions https://github.com/killme2008/defun
    ;; if-let - More than one binding for if/when macros https://github.com/LockedOn/if-let
    ;; Component - Component Lifecycle https://github.com/stuartsierra/component
    ;; Liberator - WebMachine (REST API server) port to Clojure https://github.com/clojure-liberator/liberator
    ;; RethinkDB - RethinkDB client for Clojure https://github.com/apa512/clj-rethinkdb
    ;; Schema - Data validation https://github.com/Prismatic/schema
    ;; Timbre - Pure Clojure/Script logging library https://github.com/ptaoussanis/timbre
    ;; Amazonica - A comprehensive Clojure client for the AWS API https://github.com/mcohen01/amazonica
    ;; Raven - Interface to Sentry error reporting https://github.com/sethtrain/raven-clj
    ;; Cheshire - JSON encoding / decoding https://github.com/dakrone/cheshire
    ;; clj-jwt - A Clojure library for JSON Web Token(JWT) https://github.com/liquidz/clj-jwt
    ;; clj-time - Date and time lib https://github.com/clj-time/clj-time
    ;; Environ - Get environment settings from different sources https://github.com/weavejester/environ
  ]

  ;; All profile plugins
  :plugins [
    ;; Common ring tasks https://github.com/weavejester/lein-ring
    [lein-ring "0.12.4"]
    ;; Get environment settings from different sources https://github.com/weavejester/environ
    [lein-environ "1.1.0"]
  ]

  :profiles {

    ;; QA environment and dependencies
    :qa {
      :env {
        :db-name "open_company_storage_qa"
        :liberator-trace "false"
        :hot-reload "false"
        :open-company-auth-passphrase "this_is_a_qa_secret" ; JWT secret
      }
      :dependencies [
        ;; Example-based testing https://github.com/marick/Midje
        ;; NB: org.clojure/tools.macro is pulled in manually
        ;; NB: clj-time is pulled in by oc.lib
        ;; NB: joda-time is pulled in by oc.lib via clj-time
        ;; NB: commons-codec pulled in by oc.lib
        [midje "1.9.4" :exclusions [joda-time org.clojure/tools.macro clj-time commons-codec]] 
        ;; Test Ring requests https://github.com/weavejester/ring-mock
        [ring-mock "0.1.5"]
      ]
      :plugins [
        ;; Example-based testing https://github.com/marick/lein-midje
        [lein-midje "3.2.1"]
        ;; Linter https://github.com/jonase/eastwood
        [jonase/eastwood "0.3.3"]
        ;; Static code search for non-idiomatic code https://github.com/jonase/kibit
        [lein-kibit "0.1.6" :exclusions [org.clojure/clojure]]
      ]
    }

    ;; Dev environment and dependencies
    :dev [:qa {
      :env ^:replace {
        :db-name "open_company_storage_dev"
        :liberator-trace "true" ; liberator debug data in HTTP response headers
        :hot-reload "true" ; reload code when changed on the file system
        :open-company-auth-passphrase "this_is_a_dev_secret" ; JWT secret
        :aws-access-key-id "CHANGE-ME"
        :aws-secret-access-key "CHANGE-ME"
        :aws-sqs-bot-queue "CHANGE-ME" ; SQS queue to pass on requests to the Slack Bot
        :aws-sqs-email-queue "CHANGE-ME" ; SQS queue to pass on requests to the Email service
        :aws-sqs-auth-queue "CHANGE-ME" ; SQS queue to read notifications from the Auth service
        :aws-sns-storage-topic-arn "" ; SNS topic to publish notifications (optional)
        :open-company-ziggeo-api-token "CHANGE-ME" ; Video processing API token
        :open-company-ziggeo-api-key "CHANGE-ME" ; Video processing API key
        :log-level "debug"
      }
      :plugins [
        ;; Check for code smells https://github.com/dakrone/lein-bikeshed
        ;; NB: org.clojure/tools.cli is pulled in by lein-kibit
        [lein-bikeshed "0.5.1" :exclusions [org.clojure/tools.cli]] 
        ;; Runs bikeshed, kibit and eastwood https://github.com/itang/lein-checkall
        [lein-checkall "0.1.1"]
        ;; pretty-print the lein project map https://github.com/technomancy/leiningen/tree/master/lein-pprint
        [lein-pprint "1.2.0"]
        ;; Check for outdated dependencies https://github.com/xsc/lein-ancient
        [lein-ancient "0.6.15"]
        ;; Catch spelling mistakes in docs and docstrings https://github.com/cldwalker/lein-spell
        [lein-spell "0.1.0"]
        ;; Dead code finder https://github.com/venantius/yagni
        [venantius/yagni "0.1.6" :exclusions [org.clojure/clojure]]
        ;; Pretty-print clj and EDN https://github.com/kkinnear/lein-zprint
        [lein-zprint "0.3.13" :exclusions [org.clojure/clojure]]
      ]  
    }]
    :repl-config [:dev {
      :dependencies [
        ;; Network REPL https://github.com/clojure/tools.nrepl
        [org.clojure/tools.nrepl "0.2.13"]
        ;; Pretty printing in the REPL (aprint ...) https://github.com/razum2um/aprint
        [aprint "0.1.3"]
      ]
      ;; REPL injections
      :injections [
        (require '[aprint.core :refer (aprint ap)]
                 '[clojure.stacktrace :refer (print-stack-trace)]
                 '[clj-time.core :as t]
                 '[clj-time.format :as f]
                 '[clojure.string :as s]
                 '[rethinkdb.query :as r]
                 '[cheshire.core :as json]
                 '[ring.mock.request :refer (request body content-type header)]
                 '[schema.core :as schema]
                 '[oc.lib.schema :as lib-schema]
                 '[oc.lib.jwt :as jwt]
                 '[oc.lib.db.common :as db-common]
                 '[oc.lib.slugify :as slug]
                 '[oc.storage.app :refer (app)]
                 '[oc.storage.config :as config]
                 '[oc.storage.resources.common :as common]
                 '[oc.storage.resources.org :as org]
                 '[oc.storage.resources.board :as board]
                 '[oc.storage.resources.entry :as entry]
                 '[oc.storage.representations.org :as org-rep]
                 '[oc.storage.resources.reaction :as reaction]
                 '[oc.storage.representations.board :as board-rep]
                 '[oc.storage.representations.entry :as entry-rep]
                 '[oc.storage.resources.maintenance :as maint]
                 )
      ]
    }]

    ;; Production environment
    :prod {
      :env {
        :db-name "open_company_storage"
        :env "production"
        :liberator-trace "false"
        :hot-reload "false"
      }
    }

    :uberjar {:aot :all}

  }

  :repl-options {
    :welcome (println (str "\n" (slurp (clojure.java.io/resource "ascii_art.txt")) "\n"
                      "OpenCompany Storage REPL\n"
                      "\nReady to do your bidding... I suggest (go) or (go <port>) or (go-db) as your first command.\n"))
    :init-ns dev
  }

  :aliases {
    "build" ["do" "clean," "deps," "compile"] ; clean and build code
    "create-migration" ["run" "-m" "oc.storage.db.migrations" "create"] ; create a data migration
    "migrate-db" ["run" "-m" "oc.storage.db.migrations" "migrate"] ; run pending data migrations
    "start" ["do" "migrate-db," "run"] ; start a development server
    "start!" ["with-profile" "prod" "do" "start"] ; start a server in production
    "full-index" ["run" "-m" "oc.storage.util.search"] ; push a full index or re-index to search service SQS queue
    "autotest" ["with-profile" "qa" "do" "migrate-db," "midje" ":autotest"] ; watch for code changes and run affected tests
    "test!" ["with-profile" "qa" "do" "build," "migrate-db," "midje"] ; build, init the DB and run all tests
    "repl" ["with-profile" "+repl-config" "repl"]
    "spell!" ["spell" "-n"] ; check spelling in docs and docstrings
    "bikeshed!" ["bikeshed" "-v" "-m" "120"] ; code check with max line length warning of 120 characters
    "ancient" ["ancient" ":all" ":allow-qualified"] ; check for out of date dependencies
  }

  ;; ----- Code check configuration -----

  :eastwood {
    ;; Disable some linters that are enabled by default
    ;; contant-test - just seems mostly ill-advised, logical constants are useful in something like a `->cond` 
    ;; wrong-arity - unfortunate, but it's failing on 3/arity of sqs/send-message
    ;; implicit-dependencies - uhh, just seems dumb
    :exclude-linters [:constant-test :wrong-arity :implicit-dependencies]
    ;; Enable some linters that are disabled by default
    :add-linters [:unused-namespaces :unused-private-vars] ; :unused-locals]

    :config-files ["third-party-macros.clj"]
    
    ;; Exclude testing namespaces
    :tests-paths ["test"]
    :exclude-namespaces [:test-paths]
  }

  :zprint {:old? false}
  
  ;; ----- API -----

  :ring {
    :handler oc.storage.app/app
    :reload-paths ["src"] ; work around issue https://github.com/weavejester/lein-ring/issues/68
  }

  :main oc.storage.app
)