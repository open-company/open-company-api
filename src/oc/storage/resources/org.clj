(ns oc.storage.resources.org
  (:require [clojure.walk :refer (keywordize-keys)]
            [clojure.set :as clj-set]
            [schema.core :as schema]
            [oc.lib.slugify :as slug]
            [oc.lib.schema :as lib-schema]
            [oc.lib.db.common :as db-common]
            [oc.storage.resources.common :as common]))

;; ----- RethinkDB metadata -----

(def table-name common/org-table-name)
(def primary-key :slug)

;; ----- Metadata -----

(def reserved-properties
  "Properties of a resource that can't be specified during a create and are ignored during an update."
  (clj-set/union common/reserved-properties #{:authors}))

(def ignored-properties
  "Properties of a resource that are ignored during an update."
  (clj-set/union reserved-properties #{:team-id :utm-data}))

;; ----- Data Defaults -----

(def default-promoted false)

(def content-visibility {:disallow-secure-links false
                         :disallow-public-board false
                         :disallow-public-share false})

;; ----- Utility functions -----

(defn clean
  "Remove any reserved properties from the org."
  [org]
  (apply dissoc org reserved-properties))

(defn ignore-props
  "Remove any ignored properties from the org."
  [org]
  (apply dissoc org ignored-properties))

(defn trunc
  "
  Truncate a string based on length
  "
  [s n]
  (subs s 0 (min (count s) n)))
;; ----- Organization Slug -----

;; Excluded slugs due to existing and potential URL routing conflicts
;; the person names are excluded because right now we're sending update emails as from: "slug@carrot.io"
(def reserved-slugs #{"about" "android" "api" "app" "careers" "companies"
                      "company" "contact" "create-company" "crowd" "developer"
                      "developers" "download" "entry" "entries" "email-confirmation"
                      "faq" "features" "forum" "forums" "founder" "founders" "help" "home"
                      "investor" "investors" "invite" "ios" "jobs" "login" "logout"
                      "news" "newsletter" "press" "press-kit" "pricing" "privacy" "profile" "register" "reset"
                      "section" "sections" "signin" "signout" "signup" "sign-up" "verify" "email-required" "login-wall"
                      "subscription-completed" "team" "terms" "500" "404" "slack-lander" "slack" "google"
                      "update" "updates" "stuart" "sean" "iacopo" "nathan" "ryan"})

(declare list-orgs)
(defn taken-slugs
  "Return all org slugs which are in use as a set."
  [conn]
  {:pre [(db-common/conn? conn)]}
  (into reserved-slugs (map :slug (list-orgs conn))))

(defn slug-available?
  "Return true if the slug is not used by any org in the system."
  [conn slug]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (not (contains? (taken-slugs conn) slug)))

;; ----- Org CRUD -----

(schema/defn ^:always-validate ->org :- common/Org
  "
  Take a minimal map describing an org and a user and an optional slug and 'fill the blanks' with
  any missing properties.
  "
  ([org-props user]
  (->org (or (:slug org-props) (slug/slugify (:name org-props))) org-props user))

  ([slug org-props user :- lib-schema/User]
  {:pre [(slug/valid-slug? slug)
         (map? org-props)]}
  (let [ts (db-common/current-timestamp)]
    (-> org-props
        keywordize-keys
        clean
        (assoc :slug (trunc slug 126)) ;; primary key length is 127
        (assoc :uuid (db-common/unique-id))
        (assoc :authors [(:user-id user)])
        (update :promoted #(or % default-promoted))
        (update :logo-width #(or % 0))
        (update :logo-height #(or % 0))
        (assoc :author (lib-schema/author-for-user user))
        (assoc :content-visibility content-visibility)
        (assoc :created-at ts)
        (assoc :updated-at ts)))))

(schema/defn ^:always-validate create-org!
  "
  Create an org in the system. Throws a runtime exception if the org doesn't conform to the common/Org schema.

  Check the slug in the response as it may change if there is a conflict with another org.
  "
  [conn org :- common/Org]
  {:pre [(db-common/conn? conn)]}
  (db-common/create-resource conn table-name (update org :slug #(slug/find-available-slug % (taken-slugs conn)))
    (db-common/current-timestamp)))

(schema/defn ^:always-validate get-org :- (schema/maybe common/Org)
  "Given the slug or UUID of the org, return the org, or return nil if it doesn't exist."
  [conn identifier]
  {:pre [(db-common/conn? conn)
         (or (lib-schema/unique-id? identifier)
             (slug/valid-slug? identifier))]}
  ;; if it looks like a UUID try retrieval by UUID, but also fallback to retrieval by slug
  ;; since we can have slugs that look like UUIDs
  (if-let [org (and (lib-schema/unique-id? identifier)
                    (first (db-common/read-resources conn table-name :uuid identifier)))]
    org
    (db-common/read-resource conn table-name identifier)))

(schema/defn ^:always-validate uuid-for :- (schema/maybe lib-schema/UniqueID)
  "Given an org slug, return the UUID of the org, or nil if it doesn't exist."
  [conn slug]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (:uuid (get-org conn slug)))

(schema/defn ^:always-validate update-org! :- (schema/maybe common/Org)
  "
  Given the slug of the org and an updated org property map, update the org and return the updated org on success.

  Throws an exception if the merge of the prior org and the updated org property map doesn't conform
  to the common/Org schema.
  
  NOTE: doesn't update authors, see: `add-author`, `remove-author`
  NOTE: doesn't handle case of slug change.
  "
  [conn slug org]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)
         (map? org)]}
  (when-let [original-org (get-org conn slug)]
    (let [updated-org (merge original-org (ignore-props org))]
      (schema/validate common/Org updated-org)
      (db-common/update-resource conn table-name primary-key original-org updated-org))))

(defn delete-org!
  "Given the slug of the org, delete it and all its boards, entries, and updates and return `true` on success."
  [conn slug]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (if-let [uuid (:uuid (get-org conn slug))]
    
    (do
      ;; Delete interactions
      (try
        (db-common/delete-resource conn common/interaction-table-name :org-uuid uuid)
        (catch java.lang.RuntimeException _)) ; OK if no interactions
      ;; Delete entries
      (try 
        (db-common/delete-resource conn common/entry-table-name :org-uuid uuid)
        (catch java.lang.RuntimeException _)) ; OK if no entries
      ;; Delete boards
      (try
        (db-common/delete-resource conn common/board-table-name :org-uuid uuid)
        (catch java.lang.RuntimeException _)) ; OK if no boards
      ;; Delete the org itself
      (db-common/delete-resource conn table-name slug))
    
    false)) ; it's OK if there is no org to delete

;; ----- Org's set operations -----

(schema/defn ^:always-validate add-author :- (schema/maybe common/Org)
  "
  Given the slug of the org, and the user-id of the user, add the user as an author of the org if it exists.
  Returns the updated org on success, nil on non-existence, and a RethinkDB error map on other errors.
  "
  [conn slug user-id :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (when (get-org conn slug)
    (db-common/add-to-set conn table-name slug "authors" user-id)))

(schema/defn ^:always-validate remove-author :- (schema/maybe common/Org)
  "
  Given the slug of the org, and the user-id of the user, remove the user as an author of the org if it exists.
  Returns the updated org on success, nil on non-existence, and a RethinkDB error map on other errors.
  "
  [conn slug user-id :- lib-schema/UniqueID]
  {:pre [(db-common/conn? conn)
         (slug/valid-slug? slug)]}
  (when (get-org conn slug)
    (db-common/remove-from-set conn table-name slug "authors" user-id)))

;; ----- Collection of orgs -----

(defn list-orgs
  "
  Return a sequence of maps with slugs, UUIDs and names, sorted by slug.
  
  NOTE: if additional-keys are supplied, they will be included in the map, and only orgs
  containing those keys will be returned.
  "
  ([conn] (list-orgs conn []))

  ([conn additional-keys]
  {:pre [(db-common/conn? conn)
         (sequential? additional-keys)
         (every? #(or (string? %) (keyword? %)) additional-keys)]}
  (->> (into [primary-key "uuid" "name"] additional-keys)
    (db-common/read-resources conn table-name)
    (sort-by primary-key)
    vec)))

(defn list-orgs-by-index
  "
  Given the name of a secondary index and a value, retrieve all matching orgs.

  Secondary indexes:
  `uuid`
  `team-id`
  `authors`
  "
  [conn index-key v]
  {:pre [(db-common/conn? conn)
         (string? index-key)]}
  (db-common/read-resources conn table-name index-key v))

(defn list-orgs-by-teams
  "
  Given a sequence of team-id's, retrieve all matching orgs, returning `slug` and `name`. 
  
  Additional fields to return can be optionally specified.
  "
  ([conn team-ids] (list-orgs-by-teams conn team-ids []))

  ([conn team-ids additional-fields]
  {:pre [(db-common/conn? conn)
         (schema/validate [lib-schema/UniqueID] team-ids)
         (sequential? additional-fields)
         (every? #(or (string? %) (keyword? %)) additional-fields)]}
    (db-common/read-resources conn table-name :team-id team-ids (concat [:slug :name] additional-fields))))

(defn list-orgs-by-team
  "
  Given a team-id, retrieve all matching orgs, returning `slug` and `name`. 
  
  Additional fields to return can be optionally specified.
  "
  ([conn team-id] (list-orgs-by-team conn team-id []))
  
  ([conn team-id additional-fields]
    {:pre [(schema/validate lib-schema/UniqueID team-id)]}
  (list-orgs-by-teams conn [team-id] additional-fields)))

;; ----- Armageddon -----

(defn delete-all-orgs!
  "Use with caution! Failure can result in partial deletes. Returns `true` if successful.
   Second parameter has to be delete-them-all! to avoid confusing this with the delete-org! function."
  [conn security-check]
  {:pre [(db-common/conn? conn)
         (= security-check "delete-them-all!")]}
  ;; Delete all interactions, entries, boards and orgs
  (db-common/delete-all-resources! conn common/interaction-table-name)
  (db-common/delete-all-resources! conn common/entry-table-name)
  (db-common/delete-all-resources! conn common/board-table-name)
  (db-common/delete-all-resources! conn table-name))