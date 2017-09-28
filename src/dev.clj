(ns dev
  (:require [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :as ctnrepl]
            [oc.lib.db.pool :as pool]
            [oc.storage.config :as c]
            [oc.storage.app :as app]
            [oc.storage.components :as components]))

(defonce system nil)
(defonce conn nil)

(defn init
  ([] (init c/storage-server-port))
  ([port]
  (alter-var-root #'system (constantly (components/storage-system {:handler-fn app/app
                                                                   :port port})))))

(defn bind-conn! []
  (alter-var-root #'conn (constantly (pool/claim (get-in system [:db-pool :pool])))))

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system (fn [s] (when s (component/stop s)))))

(defn go
  
  ([] (go c/storage-server-port))
  
  ([port]
  (init port)
  (start)
  (bind-conn!)
  (app/echo-config port)
  (println (str "Now serving storage from the REPL.\n"
                "A DB connection is available with: conn\n"
                "When you're ready to stop the system, just type: (stop)\n"))
  port))

(defn reset []
  (stop)
  (ctnrepl/refresh :after 'user/go))