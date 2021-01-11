(ns oc.storage.api.access
  "Access control functions for storage API."
  (:require [if-let.core :refer (if-let*)]
            [defun.core :refer (defun)]
            [taoensso.timbre :as timbre]
            [oc.lib.schema :as lib-schema]
            [oc.lib.slugify :as slugify]
            [oc.storage.resources.org :as org-res]
            [oc.storage.resources.board :as board-res]))

;; ----- Validation -----

(defn malformed-user-id?
  "Read in the body param from the request and make sure it's a non-blank string
  that corresponds to a user-id. Otherwise just indicate it's malformed."
  [ctx]
  (try
    (if-let* [user-id (slurp (get-in ctx [:request :body]))
              valid? (lib-schema/unique-id? user-id)]
      [false {:data user-id}]
      true)
    (catch Exception e
      (timbre/warn "Request body not processable as a user-id: " e)
      true)))

(defn premium-org? [org user]
  ((set (:premium-teams user)) (:team-id org)))

;; ----- Authorization -----

(defun access-level-for
  "
  Given an org (or slug) and a user map, return the authorization level for the user on the org:
    :author
    :viewer
    :public
    false

  Or, given an org (or slug), board (or slug) and user map, return the authorization level for the user on the board:
    :author
    :viewer
    :public
    false
  "
  ;; Access to org
  ;; Invalid org slug
  ([_conn org-slug :guard #(and (string? %) (not (slugify/valid-slug? %))) _user]
    ;; Will fail existence checks later
    {:access-level :does-not-exist})

  ;; With org slug, check if it exists
  ([conn :guard lib-schema/conn? org-slug :guard slugify/valid-slug? user :guard #(or (map? %) (nil? %))]
  (if-let [org (org-res/get-org conn org-slug)]
    (access-level-for conn org user)
    ;; Will fail existence checks later
    {:access-level :does-not-exist}))

  ;; With org resource, return the access level
  ([conn :guard lib-schema/conn? org :guard map? user :guard #(or (map? %) (nil? %))]
  (let [user-id (:user-id user)
        teams (:teams user)
        admin (:admin user)
        org-uuid (:uuid org)
        org-authors (set (:authors org))
        premium? (premium-org? org user)
        member? ((set teams) (:team-id org))
        admin? ((set admin) (:team-id org))
        role (cond admin?  :admin
                   member? :member
                   :else   :anonymous)
        base-access {:role role
                     :premium? premium?}]
    (cond
      ;; an admin of this org's team
      ((set admin) (:team-id org))
      (merge base-access {:access-level :author})

      ;; a named author of this org
      (org-authors user-id)
      (merge base-access {:access-level :author})

      ;; a team member of this org
      ((set teams) (:team-id org))
      (merge base-access {:access-level :viewer})

      ;; public access to orgs w/ at least 1 public board AND that allow public boards
      (and
        (seq (board-res/list-boards-by-index conn "org-uuid-access" [[org-uuid "public"]]))
        (not (-> org :content-visibility :disallow-public-board)))
      (merge base-access {:access-level :public})

      ;; no access
      :else false)))


  ;; Access to board

  ;; Invalid org slug
  ([_conn org-slug :guard #(and (string? %) (not (slugify/valid-slug? %))) _board_slug _user]
    ;; Will fail existence checks later
    {:access-level :does-not-exist})

  ;; Invalid board slug
  ([_conn _org-slug _board_slug :guard #(and (string? %) (not (slugify/valid-slug? %))) _user]
    ;; Will fail existence checks later
    {:access-level :does-not-exist})

  ([conn :guard lib-schema/conn? org-slug :guard slugify/valid-slug? board-slug-or-uuid :guard slugify/valid-slug?
    user :guard #(or (map? %) (nil? %))]
  (if-let* [org (org-res/get-org conn org-slug)
            board (board-res/get-board conn (:uuid org) board-slug-or-uuid)]
    (access-level-for org board user)
    ;; Will fail existence checks later
    {:access-level :does-not-exist}))


  ([org :guard map? board :guard map? user :guard #(or (map? %) (nil? %))]
  (let [user-id (:user-id user)
        teams (:teams user)
        admin (:admin user)
        org-authors (set (:authors org))
        board-access (keyword (:access board))
        board-authors (set (:authors board))
        board-viewers (set (:viewers board))
        premium? (premium-org? org user)
        org-member? ((set teams) (:team-id org))
        admin? ((set admin) (:team-id org))
        role (cond admin?      :admin
                   org-member? :member
                   :else       :anonymous)
        publisher-board-role (if (= (str board-res/publisher-board-slug-prefix user-id) (:slug board))
                               :author
                               :viewer)
        base-access {:role     role
                     :premium? premium?}]
    (cond

      ;; a named author of this private board
      (and (= board-access :private)
           (board-authors user-id))
      (merge base-access {:access-level :author})

      (and (= board-access :team)
           (:publisher-board board))
      (merge base-access {:access-level publisher-board-role})

      ;; an admin of this org's team for this non-private board
      (and (not= board-access :private) ((set admin) (:team-id org)))
      (merge base-access {:access-level :author})

      ;; an org author of this non-private board
      (and (not= board-access :private) (org-authors user-id))
      (merge base-access {:access-level :author})

      ;; a named viewer of this board
      (and (= board-access :private) (board-viewers user-id))
      (merge base-access {:access-level :viewer})

      ;; a team member on a non-private board
      (and (not= board-access :private) ((set teams) (:team-id org)))
      (merge base-access {:access-level :viewer})

      ;; anyone else on a public board IF the org allows public boards
      (and (= board-access :public) (not (-> org :content-visibility :disallow-public-board)))
      (merge base-access {:access-level :public})

      ;; no access
      :else false))))

(defn allow-team-admins-or-no-org
  ""
  [_conn _user]
  ;; TODO
  {:access-level :author})

(defn allow-members
  "
  Given an org slug and a user map, return an access level of :author or :viewer if the user is a team member
  and false otherwise.
  "
  [conn org-slug user]
  (let [access (access-level-for conn org-slug user)]
    (if (= (:access-level access) :public)
      false
      access)))

(defn allow-premium
  "
  Given an org slug and a user map, return an access level of :author or :viewer if the user is a team member
  of a premium org, false otherwise.
  "
  [conn org-slug user]
  (let [access (allow-members conn org-slug user)]
    (if (and (:access-level access)
             (not= (:access-level access) :public)
             (:premium? access))
      access
      false)))

(defn allow-authors
  "
  Given an org slug, and user map, return true if the user is an author on the org.

  Or, given an org slug, board slug and user map, return true if the user is an author on the board.
  "
  ([conn org-slug user]
  (let [access (access-level-for conn org-slug user)
        access-level (:access-level access)]
    (if (or (= access-level :author)
            ;; Allow to fail existence check later
            (= access-level :does-not-exist))
      access
      false)))

  ([conn org-slug board-slug-or-uuid user]
  (let [access (access-level-for conn org-slug board-slug-or-uuid user)
        access-level (:access-level access)]
    (if (or (= access-level :author)
            (= access-level :does-not-exist))
      access
      false))))