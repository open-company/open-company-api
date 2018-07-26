(ns oc.storage.api.videos
  "Liberator API for ziggeo video resources."
  (:require [taoensso.timbre :as timbre]
            [compojure.core :as compojure :refer (POST)]
            [liberator.core :refer (defresource by-method)]
            [oc.lib.db.pool :as pool]
            [oc.lib.api.common :as api-common]
            [oc.storage.util.ziggeo :as ziggeo]
            [oc.storage.resources.entry :as entry-res]
            [oc.storage.config :as config]))

(defn- handle-video-webhook [conn ctx]
  (let [body (:data ctx)
        video (get-in body [:data :video])
        token (:token video)
        entry (if token
                (entry-res/get-entry-by-video conn token)
                false)]
    (timbre/debug entry)
    (when entry
      (let [video-processed (= (:state video) 5)
            video-transcript (:transcription video)]
        (entry-res/update-video-data conn video entry)))))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for operations on a ziggeo video
(defresource video [conn]
  (api-common/open-company-anonymous-resource config/passphrase)

  :allowed-methods [:post]
  ;; Media type client accepts
  :available-media-types ["application/json"]
  :handle-not-acceptable (api-common/only-accept 406 "application/json")

  ;; Actions
  :post! (fn [ctx] (handle-video-webhook conn ctx)))
;; ----- Routes -----

(defn routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      ;; Secure UUID access
      (POST "/ziggeo/videos"
        []
        (pool/with-pool [conn db-pool] 
          (video conn))))))

