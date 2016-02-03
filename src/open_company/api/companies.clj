(ns open-company.api.companies
  (:require [defun :refer (defun)]
            [compojure.core :refer (defroutes ANY OPTIONS GET POST)]
            [liberator.core :refer (defresource by-method)]
            [clojure.set :as cset]
            [schema.core :as s]
            [open-company.config :as config]
            [open-company.api.common :as common]
            [open-company.resources.common :as common-res]
            [open-company.resources.company :as company]
            [open-company.resources.section :as section]
            [open-company.representations.company :as company-rep]
            [cheshire.core :as json]))

;; Round-trip it through Cheshire to ensure the embedded HTML gets encodedod or the client has issues parsing it
(defonce sections (json/generate-string config/sections {:pretty true}))

(defun add-slug
  "Add the slug to the company properties if it's missing."
  ([_ company :guard :slug] company)
  ([slug company] (assoc company :slug slug)))

;; ----- Responses -----

(defn- company-location-response [company]
  (common/location-response ["companies" (:symbol company)]
    (company-rep/render-company company) company-rep/media-type))

(defn- unprocessable-reason [reason]
  (case reason
    :bad-company (common/missing-response)
    :invalid-name (common/unprocessable-entity-response "Company name is required.")
    :invalid-slug (common/unprocessable-entity-response "Invalid slug.")
    (common/unprocessable-entity-response "Not processable.")))

(defn- options-for-company [slug ctx]
  (if-let [company (company/get-company slug)]
    (if (common/authorized-to-company? (assoc ctx :company company))
      (common/options-response [:options :get :put :patch :delete])
      (common/options-response [:options :get]))
    (common/missing-response)))

;; ----- Actions -----

(defn- get-company [slug]
  (if-let [company (company/get-company slug)]
    {:company company}))

(defn- put-company [slug company user]
  (let [full-company (assoc company :slug slug)]
    {:updated-company (company/put-company slug full-company user)}))

(defn- patch-company [slug company-updates user]
  (let [original-company (company/get-company slug)
        section-names (clojure.set/intersection (set (keys company-updates)) common-res/section-names)
        updated-sections (->> section-names
          (map #(section/put-section slug % (company-updates %) user)) ; put each section that's in the patch
          (map #(dissoc % :id :section-name))) ; not needed for sections in company
        patch-updates (merge company-updates (zipmap section-names updated-sections))] ; updated sections & anythig else
    ;; update the company
    {:updated-company (company/put-company slug (merge original-company patch-updates) user)}))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for a specific company.
(defresource company
  [slug]
  common/open-company-anonymous-resource ; verify validity of JWToken if it's provided, but it's not required

  :available-media-types [company-rep/media-type]
  :exists? (fn [_] (get-company slug))
  :known-content-type? (fn [ctx] (common/known-content-type? ctx company-rep/media-type))

  :allowed? (by-method {
    :options (fn [ctx] (common/allow-anonymous ctx))
    :get (fn [ctx] (common/allow-anonymous ctx))
    :put (fn [ctx] (common/allow-org-members slug ctx))
    :patch (fn [ctx] (common/allow-org-members slug ctx))
    :delete (fn [ctx] (common/allow-org-members slug ctx))})

  :processable? (by-method {
    :options true
    :get true
    :put (fn [ctx] (common/check-input (company/valid-company slug (add-slug slug (:data ctx)))))
    :patch (fn [ctx] true)}) ;; TODO validate for subset of company properties

  ;; Handlers
  :handle-ok (by-method {
    :get (fn [ctx] (company-rep/render-company (:company ctx) (common/authorized-to-company? ctx)))
    :put (fn [ctx] (company-rep/render-company (:updated-company ctx)))
    :patch (fn [ctx] (company-rep/render-company (:updated-company ctx)))})
  :handle-not-acceptable (fn [_] (common/only-accept 406 company-rep/media-type))
  :handle-unsupported-media-type (fn [_] (common/only-accept 415 company-rep/media-type))
  :handle-unprocessable-entity (fn [ctx] (unprocessable-reason (:reason ctx)))
  :handle-options (fn [ctx] (options-for-company slug ctx))

  ;; Delete a company
  :delete! (fn [_] (company/delete-company slug))

  ;; Create or update a company
  ;; TODO remove possibility to create company
  :new? (by-method {:put (not (company/get-company slug))})
  :put! (fn [ctx] (put-company slug (add-slug slug (:data ctx)) (:user ctx)))
  :patch! (fn [ctx] (patch-company slug (add-slug slug (:data ctx)) (:user ctx)))
  :handle-created (fn [ctx] (company-location-response (:updated-company ctx))))

(defn valid-post-req
  "If the request contains valid JSON POST data to create
   companies retun the parsed JSON, otherwise nil."
  [ctx]
  (let [[invalid-json? parsed] (common/malformed-json? ctx)]
   (when (and (not invalid-json?)
              ;; Ensure all required keys are present and valid
              (nil? (->> (keys common-res/CompanyMinimum)
                         (select-keys (:data parsed))
                         (s/check common-res/CompanyMinimum)))
              ;; Ensure all extra fields match with our sections
              (->> (into (keys common-res/CompanyMinimum)
                         (keys common-res/CompanyOptional))
                   (cset/difference (-> parsed :data keys set))
                   (cset/superset? common-res/section-names)))
      parsed)))

;; A resource for a list of all the companies the user has access to.
(defresource company-list
  []
  common/open-company-anonymous-resource ; verify validity of JWToken if it's provided, but it's not required

  :available-charsets [common/UTF8]
  :available-media-types (by-method {:get [company-rep/collection-media-type]
                                     :post ["application/json"]})
  :allowed-methods [:options :post :get]
  :allowed? (by-method {
    :options (fn [ctx] (common/allow-anonymous ctx))
    :get (fn [ctx] (common/allow-anonymous ctx))
    :post (fn [ctx] (common/allow-authenticated ctx))})

  :handle-not-acceptable (common/only-accept 406 company-rep/collection-media-type)

  :malformed? (by-method {
    :options false
    :get false
    :post (fn [ctx] (if-let [data (valid-post-req ctx)] [false data] true))})

  ;; Get a list of companies
  :exists? (fn [_] {:companies (company/list-companies)})

  :conflict? (by-method {
    :get false
    :options false
    :post (fn [ctx] (-> ctx :data :slug company/slug-available? not))})

  :handle-ok (by-method {
    :get  (fn [ctx] (company-rep/render-company-list (:companies ctx)))
    :post (fn [ctx] (println (company/->company (:data ctx))))})
  :handle-options (fn [ctx] (if (common/authenticated? ctx)
                              (common/options-response [:options :get :post])
                              (common/options-response [:options :get]))))

;; A resource for the available sections for a specific company.
(defresource section-list
  [slug]
  common/authenticated-resource ; verify validity and presence of required JWToken

  :available-charsets [common/UTF8]
  :available-media-types [company-rep/section-list-media-type]
  :allowed-methods [:options :get]
  :allowed? (fn [ctx] (common/allow-org-members slug ctx))

  :handle-not-acceptable (common/only-accept 406 company-rep/section-list-media-type)
  :handle-options (if (company/get-company slug) (common/options-response [:options :get]) (common/missing-response))

  ;; Get a list of sections
  :exists? (fn [_] (get-company slug))
  :handle-ok (fn [_] sections))

;; ----- Routes -----

(defroutes company-routes
  (OPTIONS "/companies/:slug/section/new" [slug] (section-list slug))
  (OPTIONS "/companies/:slug/section/new/" [slug] (section-list slug))
  (GET "/companies/:slug/section/new" [slug] (section-list slug))
  (GET "/companies/:slug/section/new/" [slug] (section-list slug))
  (ANY "/companies/:slug" [slug] (company slug))
  (ANY "/companies/:slug/" [slug] (company slug))
  (OPTIONS "/companies/" [] (company-list))
  (OPTIONS "/companies" [] (company-list))
  (GET "/companies/" [] (company-list))
  (GET "/companies" [] (company-list))
  (POST "/companies/" [] (company-list))
  (POST "/companies" [] (company-list)))