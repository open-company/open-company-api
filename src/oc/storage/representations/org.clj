(ns oc.storage.representations.org
  "Resource representations for OpenCompany orgs."
  (:require [cheshire.core :as json]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.urls.org :as org-urls]
            [oc.storage.urls.board :as board-urls]
            [oc.storage.urls.entry :as entry-urls]
            [oc.storage.api.access :as access]
            [oc.storage.config :as config]
            [oc.storage.resources.common :as common]
            [oc.storage.representations.media-types :as mt]))

(def public-representation-props [:uuid :slug :name :team-id :logo-url :logo-width :logo-height
                                  :boards :created-at :updated-at :brand-color])
(def representation-props (concat public-representation-props [:author :authors :total-count :bookmarks-count
                                                               :content-visibility :inbox-count :why-carrot
                                                               :contributions-count :following-count :unfollowing-count
                                                               :following-inbox-count :unfollowing-inbox-count
                                                               :badge-following :badge-replies :brand-color
                                                               :new-entry-placeholder :new-entry-cta]))

(defn- self-link [org] (hateoas/self-link (org-urls/org org) {:accept mt/org-media-type}))

(defn- item-link [org] (hateoas/item-link (org-urls/org org) {:accept mt/org-media-type}))

(defn- active-users-link [org]
  (hateoas/link-map "active-users" hateoas/GET (org-urls/active-users org) {:accept mt/user-collection-media-type}))

(defn partial-update-link [org] (hateoas/partial-update-link (org-urls/org org) {:content-type mt/org-media-type
                                                                                :accept mt/org-media-type}))

(defn- create-board-link [org] (hateoas/create-link (board-urls/create (:slug org) :team)
                                                    {:content-type mt/board-media-type
                                                     :accept mt/board-media-type}))

(defn- create-private-board-link [org]
  (-> (board-urls/create (:slug org) :private)
      (hateoas/create-link {:content-type mt/board-media-type
                            :accept mt/board-media-type})
      (assoc :rel "create-private")))

(defn- create-public-board-link [org]
  (-> (board-urls/create (:slug org) :public)
      (hateoas/create-link {:content-type mt/board-media-type
                            :accept mt/board-media-type})
      (assoc :rel "create-public")))

(defn- create-board-preflight-link [org]
  (-> (board-urls/create-preflight org)
      (hateoas/create-link {:content-type mt/board-media-type
                            :accept mt/board-media-type})
      (assoc :rel "create-preflight")))

(defn- delete-samples-link [org]
  (hateoas/link-map "delete-samples" hateoas/DELETE (org-urls/sample-entries org) {:content-type mt/entry-collection-media-type}))

(defn- create-board-links [org premium?]
  (let [links [(create-board-preflight-link org)
               (create-board-link org)
               (when premium?
                 (create-private-board-link org))
               (when (and premium?
                          (-> org
                              :content-visibility
                              :disallow-public-board
                              not))
                 (create-public-board-link org))]]
    (remove nil? links)))

(defn- add-author-link [org] 
  (hateoas/add-link hateoas/POST (org-urls/org-authors org) {:content-type mt/org-author-media-type}))

(defn- remove-author-link [org user-id]
  (hateoas/remove-link (org-urls/org-author org user-id)))

(defn- org-collection-links [org]
  (assoc org :links [(item-link org)]))

(defn- replies-link [org]
  (hateoas/link-map "replies" hateoas/GET (org-urls/replies org) {:accept mt/entry-collection-media-type}))

(defn- activity-link [org]
  (hateoas/link-map "entries" hateoas/GET (org-urls/entries org) {:accept mt/entry-collection-media-type}))

;; (defn- recent-activity-link [org]
;;   (hateoas/link-map "activity" hateoas/GET (str (org-urls/entries org) "?sort=activity") {:accept mt/entry-collection-media-type}))

(defn- following-link [org]
  (hateoas/link-map "following" hateoas/GET (str (org-urls/entries org) "?following=true") {:accept mt/entry-collection-media-type}))

;; (defn- recent-following-link [org]
;;   (hateoas/link-map "recent-following" hateoas/GET (str (org-urls/entries org) "?sort=activity&following=true") {:accept mt/entry-collection-media-type}))

;; (defn- unfollowing-link [org]
;;   (hateoas/link-map "unfollowing" hateoas/GET (str (org-urls/entries org) "?unfollowing=true") {:accept mt/entry-collection-media-type}))

;; (defn- recent-unfollowing-link [org]
;;   (hateoas/link-map "recent-unfollowing" hateoas/GET (str (org-urls/entries org) "?sort=activity&unfollowing=true") {:accept mt/entry-collection-media-type}))

(defn- contributions-partial-link [org]
  (hateoas/link-map "partial-contributions" hateoas/GET (org-urls/contribution org "$0") {:accept mt/entry-collection-media-type}
   {:replace {:author-uuid "$0"}}))

;; (defn- recent-contributions-partial-link [org]
;;   (hateoas/link-map "recent-partial-contributions" hateoas/GET (str (org-urls/contribution org "$0") "?sort=activity") {:accept mt/entry-collection-media-type}
;;    {:replace {:author-uuid "$0"}}))

(defn- digest-partial-link [org]
  (hateoas/link-map "digest" hateoas/GET (str (org-urls/digest org) "?direction=after&start=$0") {:accept mt/entry-collection-media-type}
   {:replace {:start "$0"}}))

(defn- partial-secure-link []
  (hateoas/link-map "partial-secure" hateoas/GET (entry-urls/secure-entry "$0" "$1") {:accept mt/entry-media-type}
   {:replace {:org-slug "$0" :secure-uuid "$1"}}))

(defn- change-link [org access-level user]
  (if (or (= access-level :author) (= access-level :viewer))
    (update-in org [:links] conj
      (hateoas/link-map
        "changes"
        hateoas/GET
        (str config/change-server-ws-url "/change-socket/user/" (:user-id user))
        nil))
    org))

(defn- notify-link [org access-level user]
  (if (and (not (:id-token user)) (or (= access-level :author) (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "notifications"
        hateoas/GET
        (str config/notify-server-ws-url "/notify-socket/user/" (:user-id user))
        nil))
    org))

(defn- interactions-link [org access-level user]
  (if (and (not (:id-token user)) (or (= access-level :author) (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "interactions"
        hateoas/GET
        (str config/interaction-server-ws-url
             "/interaction-socket/user/"
             (:user-id user))
        nil))
    org))

(defn- viewer-is-private-board-author? [org user]
  (some #((set (:authors %)) (:user-id user)) (:boards org)))

(defn- reminders-link [org access-level user]
  (if (and config/reminders-enabled?
           (not (:id-token user))
           (or (= access-level :author)
               (and (= access-level :viewer)
                    (viewer-is-private-board-author? org user))))
    (update-in org [:links] conj
      (hateoas/link-map
        "reminders"
        hateoas/GET
        (str config/reminder-server-url
             "/orgs/"
             (:uuid org)
             "/reminders")
        {:accept mt/reminders-list-media-type}))
    org))

(defn- bookmarks-link [org access-level user]
  (if (and (not (:id-token user)) (or (= access-level :author) (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "bookmarks"
        hateoas/GET
        (org-urls/bookmarks org)
        {:accept mt/entry-collection-media-type}))
    org))

(defn- recent-bookmarks-link [org access-level user]
  (if (and (not (:id-token user)) (or (= access-level :author) (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "bookmarks-activity"
        hateoas/GET
        (str (org-urls/bookmarks org) "?sort=activity")
        {:accept mt/entry-collection-media-type}))
    org))

;; (defn- following-inbox-link [org access-level user]
;;   (if (and (not (:id-token user))
;;            (or (= access-level :author)
;;                (= access-level :viewer)))
;;     (update-in org [:links] conj
;;       (hateoas/link-map
;;         "following-inbox"
;;         hateoas/GET
;;         (str (org-urls/inbox org) "?following=true")
;;         {:accept mt/entry-collection-media-type}))
;;     org))

;; (defn- unfollowing-inbox-link [org access-level user]
;;   (if (and (not (:id-token user))
;;            (or (= access-level :author)
;;                (= access-level :viewer)))
;;     (update-in org [:links] conj
;;       (hateoas/link-map
;;         "unfollowing-inbox"
;;         hateoas/GET
;;         (str (org-urls/inbox org) "?unfollowing=true")
;;         {:accept mt/entry-collection-media-type}))
;;     org))

;; (defn- inbox-link [org access-level user]
;;   (if (and (not (:id-token user))
;;            (or (= access-level :author)
;;                (= access-level :viewer)))
;;     (update-in org [:links] conj
;;       (hateoas/link-map
;;         "inbox"
;;         hateoas/GET
;;         (org-urls/inbox org)
;;         {:accept mt/entry-collection-media-type}))
;;     org))

(defn- org-links [org access-level user sample-content?]
  (let [links [(self-link org)]
        id-token (:id-token user)
        premium? (access/premium-org? org user)
        activity-links (if (and (not id-token) (or (= access-level :author) (= access-level :viewer)))
                          (concat links [(active-users-link org)
                                         (activity-link org)
                                         ; (recent-activity-link org)
                                         ; (recent-contributions-partial-link org)
                                         (following-link org)
                                         ; (recent-following-link org)
                                         ; (unfollowing-link org)
                                         ; (recent-unfollowing-link org)
                                         (contributions-partial-link org)
                                         (replies-link org)
                                         (digest-partial-link org)]) ; (calendar-link org) - not currently used
                          links)
        board-links (create-board-links org premium?)
        author-links (if (and (not id-token) (= access-level :author) )
                       (concat activity-links
                               [(partial-update-link org)
                                (add-author-link org)]
                               board-links)
                       activity-links)
        delete-sample-links (if sample-content?
                              (concat author-links [(delete-samples-link org)])
                              author-links)]
    (assoc org :links delete-sample-links)))

(def auth-link (hateoas/link-map "authenticate" hateoas/GET config/auth-server-url {:accept "application/json"}))

(defn render-author-for-collection
  "Create a map of the org author for use in a collection in the REST API"
  [org user-id access-level]
  {:user-id user-id
   :links (if (= access-level :author) [(remove-author-link org user-id)] [])})

(defn- premium-filter
  ([org-map user]
   (premium-filter org-map user ((set (:premium-teams user)) (:team-id org-map))))
  ([org-map _user premium?]
  (if premium?
    org-map
    (dissoc org-map :brand-color))))

(defn render-org
  "Given an org, create a JSON representation of the org for the REST API."
  [org {:keys [access-level user premium?]} sample-content?]
  (let [rep-props (if (or (= :author access-level)
                          (= :viewer access-level))
                    representation-props
                    public-representation-props)
        org-repr (-> org
                     (update :new-entry-cta #(or % common/default-entry-cta))
                     (update :new-entry-placeholder #(or % common/default-entry-placeholder))
                     (org-links access-level user sample-content?)
                     (change-link access-level user)
                     (notify-link access-level user)
                     (interactions-link access-level user)
                     (reminders-link access-level user)
                     (bookmarks-link access-level user)
                     (recent-bookmarks-link access-level user)
                     ; (inbox-link access-level user)
                     ; (following-inbox-link access-level user)
                     ; (unfollowing-inbox-link access-level user)
                     (select-keys (conj rep-props :links))
                     (premium-filter user premium?))]
    (json/generate-string org-repr {:pretty config/pretty?})))

(defn render-org-list
  "Given a sequence of org maps, create a JSON representation of a list of orgs for the REST API."
  [orgs {user :user}]
  (let [links [(hateoas/self-link org-urls/entry-point {:accept mt/org-collection-media-type}) auth-link
               (partial-secure-link)]
        with-premium-filter (map #(premium-filter % user) orgs)
        full-links (if user
                     (conj links
                           (hateoas/create-link org-urls/orgs {:content-type mt/org-media-type
                                                               :accept mt/org-media-type}))
                     links)]
    (json/generate-string
      {:collection {:version hateoas/json-collection-version
                    :href "/"
                    :links full-links
                    :items (map org-collection-links with-premium-filter)}}
      {:pretty config/pretty?})))