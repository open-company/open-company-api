(defproject open-company-api "0.0.2-SNAPSHOT"
  :description "OpenCompany Platform API"
  :url "https://opencompany.com/"
  :license {
    :name "Mozilla Public License v2.0"
    :url "http://www.mozilla.org/MPL/2.0/"
  }

  :min-lein-version "2.5.1" ; highest version supported by Travis-CI as of 1/28/2016

  ;; JVM memory
  :jvm-opts ^:replace ["-Xms512m" "-Xmx3072m" "-server"]

  ;; All profile dependencies
  :dependencies [
    [org.clojure/clojure "1.9.0-alpha14"] ; Lisp on the JVM http://clojure.org/documentation
    [org.clojure/core.match "0.3.0-alpha4"] ; Erlang-esque pattern matching https://github.com/clojure/core.match
    [org.clojure/core.async "0.2.395"] ; Async programming and communication https://github.com/clojure/core.async
    [defun "0.3.0-RC1"] ; Erlang-esque pattern matching for Clojure functions https://github.com/killme2008/defun
    [lockedon/if-let "0.1.0"] ; More than one binding for if/when macros https://github.com/LockedOn/if-let
    [ring/ring-devel "1.6.0-beta7"] ; Web application library https://github.com/ring-clojure/ring
    [ring/ring-core "1.6.0-beta7"] ; Web application library https://github.com/ring-clojure/ring
    [jumblerg/ring.middleware.cors "1.0.1"] ; CORS library https://github.com/jumblerg/ring.middleware.cors
    [http-kit "2.3.0-alpha1"] ; Web server http://http-kit.org/
    [compojure "1.6.0-beta3"] ; Web routing https://github.com/weavejester/compojure
    [liberator "0.14.1"] ; WebMachine (REST API server) port to Clojure https://github.com/clojure-liberator/liberator
    [com.apa512/rethinkdb "0.15.26"] ; RethinkDB client for Clojure https://github.com/apa512/clj-rethinkdb
    [prismatic/schema "1.1.3"] ; Data validation https://github.com/Prismatic/schema
    [environ "1.1.0"] ; Environment settings from different sources https://github.com/weavejester/environ
    [cheshire "5.6.3"] ; JSON encoding / decoding https://github.com/dakrone/cheshire
    [com.taoensso/timbre "4.8.0"] ; Logging https://github.com/ptaoussanis/timbre
    [raven-clj "1.5.0"] ; Interface to Sentry error reporting https://github.com/sethtrain/raven-clj
    [clj-http "3.4.1"] ; HTTP client https://github.com/dakrone/clj-http
    [clj-time "0.13.0"] ; Date and time lib https://github.com/clj-time/clj-time
    [org.clojure/tools.cli "0.3.5"] ; Command-line parsing https://github.com/clojure/tools.cli
    [clj-jwt "0.1.1"] ; Library for JSON Web Token (JWT) https://github.com/liquidz/clj-jwt
    [medley "0.8.4"] ; Utility functions https://github.com/weavejester/medley
    [com.stuartsierra/component "0.3.2"] ; Component Lifecycle
    [amazonica "0.3.83"] ; A comprehensive Clojure client for the entire Amazon AWS api https://github.com/mcohen01/amazonica
    [zprint "0.2.12"] ; Pretty-print clj and EDN https://github.com/kkinnear/zprint
    [open-company/lib "0.1.1-dc1d942"] ; Library for OC projects https://github.com/open-company/open-company-lib
  ]

  ;; All profile plugins
  :plugins [
    [lein-ring "0.10.0"] ; Common ring tasks https://github.com/weavejester/lein-ring
    [lein-environ "1.1.0"] ; Get environment settings from different sources https://github.com/weavejester/environ
  ]

  :profiles {

    ;; QA environment and dependencies
    :qa {
      :env {
        :db-name "open_company_qa"
        :liberator-trace "false"
        :hot-reload "false"
        :open-company-auth-passphrase "this_is_a_qa_secret" ; JWT secret
      }
      :dependencies [
        [midje "1.9.0-alpha6"] ; Example-based testing https://github.com/marick/Midje
        [ring-mock "0.1.5"] ; Test Ring requests https://github.com/weavejester/ring-mock
        [philoskim/debux "0.2.1"] ; `dbg` macro around -> or let https://github.com/philoskim/debux
      ]
      :plugins [
        [lein-midje "3.2.1"] ; Example-based testing https://github.com/marick/lein-midje
        [jonase/eastwood "0.2.3"] ; Linter https://github.com/jonase/eastwood
        [lein-kibit "0.1.3"] ; Static code search for non-idiomatic code https://github.com/jonase/kibit
      ]
    }

    ;; Dev environment and dependencies
    :dev [:qa {
      :env ^:replace {
        :db-name "open_company_dev"
        :liberator-trace "true" ; liberator debug data in HTTP response headers
        :hot-reload "true" ; reload code when changed on the file system
        :open-company-auth-passphrase "this_is_a_dev_secret" ; JWT secret
        :aws-access-key-id "CHANGE-ME"
        :aws-secret-access-key "CHANGE-ME"
        :aws-sqs-bot-queue "https://sqs.REGION.amazonaws.com/CHANGE/ME" 
        :aws-sqs-email-queue "https://sqs.REGION.amazonaws.com/CHANGE/ME" 
      }
      :plugins [
        [lein-bikeshed "0.4.1"] ; Check for code smells https://github.com/dakrone/lein-bikeshed
        [lein-checkall "0.1.1"] ; Runs bikeshed, kibit and eastwood https://github.com/itang/lein-checkall
        [lein-pprint "1.1.2"] ; pretty-print the lein project map https://github.com/technomancy/leiningen/tree/master/lein-pprint
        [lein-ancient "0.6.10"] ; Check for outdated dependencies https://github.com/xsc/lein-ancient
        [lein-spell "0.1.0"] ; Catch spelling mistakes in docs and docstrings https://github.com/cldwalker/lein-spell
        [lein-deps-tree "0.1.2"] ; Print a tree of project dependencies https://github.com/the-kenny/lein-deps-tree
        [venantius/yagni "0.1.4"] ; Dead code finder https://github.com/venantius/yagni
        [lein-zprint "0.1.12"] ; Pretty-print clj and EDN https://github.com/kkinnear/lein-zprint
      ]  
    }]
    :repl-config [:dev {
      :dependencies [
        [org.clojure/tools.nrepl "0.2.12"] ; Network REPL https://github.com/clojure/tools.nrepl
        [aprint "0.1.3"] ; Pretty printing in the REPL (aprint ...) https://github.com/razum2um/aprint
      ]
      ;; REPL injections
      :injections [
        (require '[aprint.core :refer (aprint ap)]
                 '[clojure.stacktrace :refer (print-stack-trace)]
                 '[clj-time.core :as t]
                 '[clj-time.format :as f]
                 '[clojure.string :as s]
                 '[rethinkdb.query :as r]
                 '[schema.core :as schema]
                 '[cheshire.core :as json]
                 '[ring.mock.request :refer (request body content-type header)]
                 ; '[open-company.lib.rest-api-mock :refer (api-request)]
                 '[oc.lib.rethinkdb.common :as db-common]
                 '[oc.lib.slugify :as slug]
                 '[oc.api.app :refer (app)]
                 '[oc.api.config :as c]
                 '[oc.api.resources.common :as common]
                 '[oc.api.resources.org :as org]
                 '[oc.api.resources.dashboard :as dash]
                 '[oc.api.resources.entry :as entry]
                 '[oc.api.resources.update :as update]
                 ; '[open-company.representations.company :as company-rep]
                 ; '[open-company.representations.section :as section-rep]
                 ; '[open-company.representations.stakeholder-update :as su-rep]
                 )
      ]
    }]

    ;; Production environment
    :prod {
      :env {
        :db-name "open_company"
        :env "production"
        :liberator-trace "false"
        :hot-reload "false"
      }
    }
  }

  :repl-options {
    :welcome (println (str "\n" (slurp (clojure.java.io/resource "ascii_art.txt")) "\n"
                      "OpenCompany API REPL\n"
                      "Database: " oc.api.config/db-name "\n"
                      "\nReady to do your bidding... I suggest (go) or (go <port>) as your first command.\n"))
    :init-ns dev
  }

  :aliases {
    "build" ["do" "clean," "deps," "compile"] ; clean and build code
    "create-migration" ["run" "-m" "oc.api.db.migrations" "create"] ; create a data migration
    "migrate-db" ["run" "-m" "oc.api.db.migrations" "migrate"] ; run pending data migrations
    "start" ["do" "migrate-db," "run"] ; start a development server
    "start!" ["with-profile" "prod" "do" "start"] ; start a server in production
    "autotest" ["with-profile" "qa" "do" "migrate-db," "midje" ":autotest"] ; watch for code changes and run affected tests
    "test!" ["with-profile" "qa" "do" "clean," "build," "migrate-db," "midje"] ; build, init the DB and run all tests
    "repl" ["with-profile" "+repl-config" "repl"]
    "spell!" ["spell" "-n"] ; check spelling in docs and docstrings
    "bikeshed!" ["bikeshed" "-v" "-m" "120"] ; code check with max line length warning of 120 characters
    "ancient" ["ancient" ":all" ":allow-qualified"] ; check for out of date dependencies
  }

  ;; ----- Code check configuration -----

  :eastwood {
    ;; Disable some linters that are enabled by default
    :exclude-linters [:constant-test :wrong-arity]
    ;; Enable some linters that are disabled by default
    :add-linters [:unused-namespaces :unused-private-vars] ; :unused-locals]

    ;; Exclude testing namespaces
    :tests-paths ["test"]
    :exclude-namespaces [:test-paths]
  }

  :zprint {:old? false}
  
  ;; ----- API -----

  :ring {
    :handler oc.api.app/app
    :reload-paths ["src"] ; work around issue https://github.com/weavejester/lein-ring/issues/68
  }

  :main oc.api.app
)