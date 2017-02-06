(ns open-company.api.companies
  (:require [compojure.core :as compojure :refer (defroutes ANY OPTIONS GET POST)]
            [liberator.core :refer (defresource by-method)]
            [schema.core :as schema]
            [open-company.config :as config]
            [oc.lib.db.pool :as pool]
            [oc.lib.slugify :as slug]
            [open-company.lib.bot :as bot]
            [open-company.api.common :as common]
            [open-company.resources.common :as common-res]
            [open-company.resources.company :as company]
            [open-company.resources.section :as section]
            [open-company.representations.company :as company-rep]
            [open-company.representations.section :as section-rep]
            [cheshire.core :as json]))

;; ----- Utility functions -----

(defn sections-for [company-slug]
  (let [sections config/sections
        templates (:templates sections)
        with-links (map #(section-rep/section-template-for-rendering % company-slug) templates)]
    (json/generate-string (assoc sections :templates with-links) {:pretty true})))

(defn- add-slug
  "Add the slug to the company properties if it's missing."
  [slug company]
  (update company :slug (fnil identity slug)))

(defn- find-slug [conn company-props]
  (or (:slug company-props) (slug/find-available-slug (:name company-props) (company/taken-slugs conn))))

;; ----- Responses -----

(defn- company-location-response [conn company]
  (common/location-response ["companies" (:symbol company)]
    (company-rep/render-company conn company) company-rep/media-type))

(defn- unprocessable-reason [reason]
  (case reason
    :invalid-slug-format (common/unprocessable-entity-response "Invalid slug format.")
    :slug-taken (common/unprocessable-entity-response "Slug already taken.")
    :name (common/unprocessable-entity-response "Company name is required.")
    :slug (common/unprocessable-entity-response "Invalid or missing slug.")
    (common/unprocessable-entity-response (str "Not processable: " (pr-str reason)))))

(defn- options-for-company [conn slug ctx]
  (if-let [company (company/get-company conn slug)]
    (if (common/authorized-to-company? (assoc ctx :company company))
      (common/options-response [:options :get :patch :delete])
      (common/options-response [:options :get]))
    (common/missing-response)))

;; ----- Actions -----

(defn- get-company [conn slug ctx]
  (if-let [company (or (:company ctx) (company/get-company conn slug))]
    {:company company}))

(defn- patch-company [conn slug company-updates user]
  (let [original-company (company/get-company conn slug)
        section-names (filter common-res/section-name? (keys company-updates))
        ; store any new or updated sections that were provided in the company as sections
        updated-sections (->> section-names
          (map #(section/put-section conn slug % (company-updates %) user)) ; put each section that's included in the patch
          (map #(dissoc % :id :section-name))) ; not needed for sections in company
        ; merge the original company with the updated sections & any other properties they provided 
        with-section-updates (->> (zipmap section-names updated-sections)
                          (merge company-updates)
                          (merge original-company))
        ; get any sections that we used to have, that have been added back in (sections back from the dead)
        with-prior-sections (company/add-prior-sections conn with-section-updates)
        ; add in the placeholder sections for any brand new added sections
        with-placeholders (company/add-placeholder-sections with-prior-sections)]
    ;; update the company
    {:updated-company (company/put-company conn slug with-placeholders user)}))

(defn- promoted-or-authorized? [user company]
  (or 
    (and (:public company) (:promoted company)) ; public and promoted
    (common/authorized-to-company? {:company company :user user}))) ; specifically auth'd for this user

(defn- accessible-company-list
  "Return a list of all companies this user has access to."
  [conn user]
  (let [logo-companies (company/list-companies conn [:org-id :public :promoted :logo]) ; every company w/ a logo
        all-companies (company/list-companies conn [:org-id :public :promoted]) ; every company w/ or w/o a logo
        logo-index (zipmap (map :slug logo-companies) (map :logo logo-companies))
        companies (map #(assoc % :logo (logo-index (:slug %))) all-companies)] ; every company w/ a logo if they have it
    (sort-by :name (filter #(promoted-or-authorized? user %) companies)))) ; public or authorized to this user

;; ----- Validations -----

(defn processable-patch-req? [conn slug {:keys [user data]}]
  (if-let [existing-company (company/get-company conn slug)] ; can only PATCH a company that already exists
    (let [patch (assoc data :slug slug) ; they probably didn't bother to include the slug in the PATCH since it's in the URL
          with-complete-sections (company/complete-sections patch user (company/real-sections patch)) ; PATCH data w/ completed sections
          updated-company (merge existing-company with-complete-sections) ; apply the PATCH to the existing company
          invalid? (schema/check common-res/Company updated-company)] ; check that it's still valid
      (cond
        invalid? [false {:reason invalid?}] ; invalid
        :else [true {:company existing-company}])) ; it's valid, keep the existing company around for efficiency
    [true {}])) ; it will fail later on :exists?

(defn processable-post-req? [conn {:keys [user data]}]
  (let [company     (company/->company data user (find-slug conn data))
        invalid?    (schema/check common-res/Company company)
        slug-taken? (not (company/slug-available? conn (:slug company)))]
    (cond
      invalid? [false {:reason invalid?}]
      slug-taken? [false {:reason :slug-taken}]
      :else [true {}])))

;; ----- Resources - see: http://clojure-liberator.github.io/liberator/assets/img/decision-graph.svg

;; A resource for a specific company.
(defresource company
  [conn slug]
  common/open-company-anonymous-resource ; verify validity of JWToken if it's provided, but it's not required

  :allowed-methods [:options :get :patch :delete]

  :available-media-types [company-rep/media-type]
  :handle-not-acceptable (fn [_] (common/only-accept 406 company-rep/media-type))
  :known-content-type? (fn [ctx] (common/known-content-type? ctx company-rep/media-type))
  :handle-unsupported-media-type (fn [_] (common/only-accept 415 company-rep/media-type))

  :exists? (fn [ctx] (get-company conn slug ctx))

  :allowed? (by-method {
    :options (fn [ctx] (or (common/allow-public conn slug ctx) (common/allow-org-members conn slug ctx)))
    :get (fn [ctx] (or (common/allow-public conn slug ctx) (common/allow-org-members conn slug ctx)))
    :patch (fn [ctx] (common/allow-org-members conn slug ctx))
    :delete (fn [ctx] (common/allow-org-members conn slug ctx))})

  :processable? (by-method {
    :options true
    :get true
    :patch (fn [ctx] (processable-patch-req? conn slug ctx))})
  :handle-unprocessable-entity (fn [ctx] (unprocessable-reason (:reason ctx)))
 oc.auth.representations.team
  :handle-options (fn [ctx] (options-for-company conn slug ctx))
  :handle-ok (by-method {
    :get (fn [ctx] (company-rep/render-company conn (:company ctx) (common/authorized-to-company? ctx)))
    :patch (fn [ctx] (company-rep/render-company conn (:updated-company ctx)))})

  :patch! (fn [ctx] (patch-company conn slug (add-slug slug (:data ctx)) (:user ctx)))
  :delete! (fn [_] (company/delete-company! conn slug)))

;; A resource for a list of all the companies the user has access to.
(defresource company-list [conn]
  common/open-company-anonymous-resource ; verify validity of JWToken if it's provided, but it's not required

  :allowed-methods [:options :get :post]

  :available-media-types (by-method {:get [company-rep/collection-media-type]
                                     :post [company-rep/media-type]})
  :handle-not-acceptable (common/only-accept 406 company-rep/collection-media-type)

  ;; Get a list of companies
  :exists? (fn [{:keys [user]}] {:companies (accessible-company-list conn user)})

  :allowed? (by-method {
    :options (fn [ctx] (common/allow-anonymous ctx))
    :get (fn [ctx] (common/allow-anonymous ctx))
    :post (fn [ctx] (common/allow-authenticated ctx))})

  :processable? (by-method {
    :get true
    :options true
    :post (fn [ctx] (processable-post-req? conn ctx))})
  :handle-unprocessable-entity (fn [ctx] (unprocessable-reason (:reason ctx)))

  :post! (fn [{:keys [user data] :as ctx}]
           (let [company (company/create-company! conn (company/->company data user (find-slug conn data)))]
             (when (:bot user) ; Some JWTokens might not have a bot token
               (->> (assoc (common/clone ctx) :company company)
                    (bot/ctx->trigger :onboard)
                    (bot/send-trigger!)))
             {:company company}))

  :handle-options (fn [ctx] (if (common/authenticated? ctx)
                              (common/options-response [:options :get :post])
                              (common/options-response [:options :get])))
  :handle-ok (fn [ctx] (company-rep/render-company-list (:companies ctx)))
  :handle-created (fn [ctx] (company-location-response conn (:company ctx))))

;; A resource for the available sections for a specific company.
(defresource section-list
  [conn slug]
  common/authenticated-resource ; verify validity and presence of required JWToken

  :allowed-methods [:options :get]

  :available-charsets [common/UTF8]
  :available-media-types [company-rep/section-list-media-type]

  :exists? (fn [ctx] (get-company conn slug ctx))

  :allowed? (fn [ctx] (common/allow-org-members conn slug ctx))

  :handle-options (if (company/get-company conn slug)
                    (common/options-response [:options :get])
                    (common/missing-response))
  :handle-not-acceptable (common/only-accept 406 company-rep/section-list-media-type)

  :handle-ok (fn [_] (sections-for slug)))

;; ----- Routes -----

(defn company-routes [sys]
  (let [db-pool (-> sys :db-pool :pool)]
    (compojure/routes
      (OPTIONS "/companies/:slug/section/new" [slug] (pool/with-pool [conn db-pool] (section-list conn slug)))
      (GET "/companies/:slug/section/new" [slug] (pool/with-pool [conn db-pool] (section-list conn slug)))
      (ANY "/companies/:slug" [slug] (pool/with-pool [conn db-pool] (company conn slug)))
      (OPTIONS "/companies/" [] (pool/with-pool [conn db-pool] (company-list conn)))
      (OPTIONS "/companies" [] (pool/with-pool [conn db-pool] (company-list conn)))
      (GET "/companies/" [] (pool/with-pool [conn db-pool] (company-list conn)))
      (GET "/companies" [] (pool/with-pool [conn db-pool] (company-list conn)))
      (POST "/companies/" [] (pool/with-pool [conn db-pool] (company-list conn)))
      (POST "/companies" [] (pool/with-pool [conn db-pool] (company-list conn))))))