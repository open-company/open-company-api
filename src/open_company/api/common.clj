(ns open-company.api.common
  (:require [taoensso.timbre :refer (debug info warn error fatal spy)]
            [clojure.string :as s]
            [cheshire.core :as json]
            [liberator.representation :refer (ring-response)]
            [liberator.core :refer (by-method)]
            [open-company.lib.jwt :as jwt]))

(def UTF8 "utf-8")

(def malformed true)
(def good-json false)

;; ----- Responses -----

(defn missing-response
  ([]
    (ring-response {
      :status 404
      :headers {"Content-Type" (format "text/plain;charset=%s" UTF8)}}))
  ([reason]
    (ring-response {
      :status 404
      :body reason
      :headers {"Content-Type" (format "text/plain;charset=%s" UTF8)}})))

(defn missing-authentication-response []
  (ring-response {
    :status 401
    :body "Not authorized. Provide a JWToken in Authorization header."
    :headers {"Content-Type" (format "text/plain;charset=%s" UTF8)}}))

(defn unprocessable-entity-response [reason]
  (ring-response
    {:status 422
      :body reason
      :headers {"Content-Type" (format "text/plain;charset=%s" UTF8)}}))

(defn only-accept [status media-type]
  (ring-response
    {:status status
     :body (format "Acceptable media type: %s\nAcceptable charset: %s" media-type UTF8)
     :headers {"Content-Type" (format "text/plain;charset=%s" UTF8)}}))

(defn location-response [path-parts body media-type]
  (ring-response
    {:body body
     :headers {"Location" (format "/%s" (s/join "/" path-parts))
               "Content-Type" (format "%s;charset=%s" media-type UTF8)}}))

;; ----- Validations -----

(defn malformed-json?
  "Read in the body param from the request as a string, parse it into JSON, make sure all the
  keys are keywords, and then return it, mapped to :data as the 2nd value in a vector,
  with the first value indicating it's not malformed. Otherwise just indicate it's malformed."
  [ctx]
  (try
    (if-let [data (-> (get-in ctx [:request :body]) slurp (json/parse-string true))]
      ; handle case of a string which is valid JSON, but still malformed for us
      (do (when-not (map? data) (throw (Exception.)))
        [good-json {:data data}])
      malformed)
    (catch Exception e
      (debug "Request body not processable as JSON: " e)
      malformed)))

(defn known-content-type?
  [ctx content-type]
  (if-let [request-type (get-in ctx [:request :headers "content-type"])]
    (= (first (s/split content-type #";")) (first (s/split request-type #";")))
    true))

(defn check-input [check]
  (if (= check true) true [false {:reason check}]))

(defn authorized?
  "
  Check for the presence and validity of a JWToken in the Authorization header.
  
  Return false if the header isn't present or valid, otherwise return a map to
  add the JWToken to the Liberator context.
  "
  [headers]
  (if-let [authorization (or (headers "Authorization") (headers "authorization"))]
    (let [jwtoken (last (s/split authorization #" "))]
      (if (jwt/check-token jwtoken) {:jwtoken jwtoken} false))
    false))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

(def authenticated-resource {
  :authorized? (fn [ctx] (authorized? (get-in ctx [:request :headers])))
  :handle-unauthorized (fn [_] (missing-authentication-response))})

(def open-company-resource (merge authenticated-resource {
  :available-charsets [UTF8]
  :handle-not-found (fn [_] (missing-response))
  :allowed-methods [:get :put :delete :patch]
  :respond-with-entity? (by-method {:put true :patch true :delete false})
  :malformed? (by-method {
    :get false
    :delete false
    :put (fn [ctx] (malformed-json? ctx))
    :patch (fn [ctx] (malformed-json? ctx))})
  :can-put-to-missing? (fn [_] true)
  :conflict? (fn [_] false)}))