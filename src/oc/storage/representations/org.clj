(ns oc.storage.representations.org
  "Resource representations for OpenCompany orgs."
  (:require [defun.core :refer (defun)]
            [cheshire.core :as json]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.urls.board :as board-url]
            [oc.storage.api.access :as access]
            [oc.storage.config :as config]
            [oc.storage.representations.media-types :as mt]))

(def public-representation-props [:uuid :slug :name :team-id :logo-url :logo-width :logo-height
                                  :boards :created-at :updated-at :brand-color])
(def representation-props (concat public-representation-props [:author :authors :total-count :bookmarks-count
                                                               :content-visibility :inbox-count :why-carrot
                                                               :contributions-count :following-count :unfollowing-count
                                                               :following-inbox-count :unfollowing-inbox-count
                                                               :badge-following :badge-replies :brand-color]))

(defun url
  ([slug :guard string?] (str "/orgs/" slug))
  ([org :guard map?] (url (:slug org))))

(defn- active-users-url
  [{:keys [team-id]}]
  (str config/auth-server-url "/teams/" team-id "/active-users"))

(defn- self-link [org] (hateoas/self-link (url org) {:accept mt/org-media-type}))

(defn- item-link [org] (hateoas/item-link (url org) {:accept mt/org-media-type}))

(defn- active-users-link [org]
  (hateoas/link-map "active-users" hateoas/GET (active-users-url org) {:accept mt/user-collection-media-type}))

(defn partial-update-link [org] (hateoas/partial-update-link (url org) {:content-type mt/org-media-type
                                                                        :accept mt/org-media-type}))

(defn- create-board-link [org] (hateoas/create-link (board-url/create-url (:slug org)) {:content-type mt/board-media-type
                                                                                        :accept mt/board-media-type}))

(defn- create-private-board-link [org]
  (-> (str (board-url/create-url (:slug org)) "/private")
      (hateoas/create-link {:content-type mt/board-media-type
                            :accept mt/board-media-type})
      (assoc :rel "create-private")))

(defn- create-public-board-link [org]
  (-> (str (board-url/create-url (:slug org)) "/public")
      (hateoas/create-link {:content-type mt/board-media-type
                            :accept mt/board-media-type})
      (assoc :rel "create-public")))

(defn- delete-samples-link [org]
  (hateoas/link-map "delete-samples" hateoas/DELETE (str (url org) "/entries/samples") {:content-type mt/entry-collection-media-type}))

(defn- create-board-pre-flight-link [org]
  (-> (create-board-link org)
      (assoc :rel "pre-flight-create")))

(defn- create-board-links [org premium?]
  (let [links [(create-board-pre-flight-link org)
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
  (hateoas/add-link hateoas/POST (str (url org) "/authors/") {:content-type mt/org-author-media-type}))

(defn- remove-author-link [org user-id]
  (hateoas/remove-link (str (url org) "/authors/" user-id)))

(defn- org-collection-links [org]
  (assoc org :links [(item-link org)]))

(defn- replies-link [org]
  (hateoas/link-map "replies" hateoas/GET (str (url org) "/replies") {:accept mt/entry-collection-media-type}))

(defn- activity-link [org]
  (hateoas/link-map "entries" hateoas/GET (str (url org) "/entries") {:accept mt/entry-collection-media-type}))

(defn- recent-activity-link [org]
  (hateoas/link-map "activity" hateoas/GET (str (url org) "/entries?sort=activity") {:accept mt/entry-collection-media-type}))

(defn- following-link [org]
  (hateoas/link-map "following" hateoas/GET (str (url org) "/entries?following=true") {:accept mt/entry-collection-media-type}))

(defn- recent-following-link [org]
  (hateoas/link-map "recent-following" hateoas/GET (str (url org) "/entries?sort=activity&following=true") {:accept mt/entry-collection-media-type}))

(defn- unfollowing-link [org]
  (hateoas/link-map "unfollowing" hateoas/GET (str (url org) "/entries?unfollowing=true") {:accept mt/entry-collection-media-type}))

(defn- recent-unfollowing-link [org]
  (hateoas/link-map "recent-unfollowing" hateoas/GET (str (url org) "/entries?sort=activity&unfollowing=true") {:accept mt/entry-collection-media-type}))

(defn- contributions-partial-link [org]
  (hateoas/link-map "partial-contributions" hateoas/GET (str (url org) "/contributions/$0") {:accept mt/entry-collection-media-type}
   {:replace {:author-uuid "$0"}}))

(defn- recent-contributions-partial-link [org]
  (hateoas/link-map "recent-partial-contributions" hateoas/GET (str (url org) "/contributions/$0?sort=activity") {:accept mt/entry-collection-media-type}
   {:replace {:author-uuid "$0"}}))

(defn- digest-partial-link [org]
  (hateoas/link-map "digest" hateoas/GET (str (url org) "/digest?direction=after&start=$0") {:accept mt/entry-collection-media-type}
   {:replace {:start "$0"}}))

(defn secure-url [org-slug secure-uuid] (str (url org-slug) "/entries/" secure-uuid))

(defn- partial-secure-link []
  (hateoas/link-map "partial-secure" hateoas/GET (secure-url "$0" "$1") {:accept mt/entry-media-type}
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
        (str (url org) "/bookmarks")
        {:accept mt/entry-collection-media-type}))
    org))

(defn- recent-bookmarks-link [org access-level user]
  (if (and (not (:id-token user)) (or (= access-level :author) (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "bookmarks-activity"
        hateoas/GET
        (str (url org) "/bookmarks?sort=activity")
        {:accept mt/entry-collection-media-type}))
    org))

(defn- following-inbox-link [org access-level user]
  (if (and (not (:id-token user))
           (or (= access-level :author)
               (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "following-inbox"
        hateoas/GET
        (str (url org) "/inbox?following=true")
        {:accept mt/entry-collection-media-type}))
    org))

(defn- unfollowing-inbox-link [org access-level user]
  (if (and (not (:id-token user))
           (or (= access-level :author)
               (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "unfollowing-inbox"
        hateoas/GET
        (str (url org) "/inbox?unfollowing=true")
        {:accept mt/entry-collection-media-type}))
    org))

(defn- inbox-link [org access-level user]
  (if (and (not (:id-token user))
           (or (= access-level :author)
               (= access-level :viewer)))
    (update-in org [:links] conj
      (hateoas/link-map
        "inbox"
        hateoas/GET
        (str (url org) "/inbox")
        {:accept mt/entry-collection-media-type}))
    org))

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

(defn render-org
  "Given an org, create a JSON representation of the org for the REST API."
  [org access-level user sample-content?]
  (let [slug (:slug org)
        rep-props (if (or (= :author access-level) (= :viewer access-level))
                    representation-props
                    public-representation-props)
        user-id (:user-id user)]
    (json/generate-string
      (-> org
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
        (select-keys (conj rep-props :links)))
      {:pretty config/pretty?})))

(defn render-org-list
  "Given a sequence of org maps, create a JSON representation of a list of orgs for the REST API."
  [orgs authed?]
  (let [links [(hateoas/self-link "/" {:accept mt/org-collection-media-type}) auth-link
               (partial-secure-link)]
        full-links (if authed?
                      (conj links (hateoas/create-link "/orgs/" {:content-type mt/org-media-type
                                                                 :accept mt/org-media-type}))
                      links)]
    (json/generate-string
      {:collection {:version hateoas/json-collection-version
                    :href "/"
                    :links full-links
                    :items (map org-collection-links orgs)}}
      {:pretty config/pretty?})))