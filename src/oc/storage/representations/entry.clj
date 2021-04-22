(ns oc.storage.representations.entry
  "Resource representations for OpenCompany entries."
  (:require [defun.core :refer (defun defun-)]
            [cheshire.core :as json]
            [clojure.set :as clj-set]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.config :as config]
            [oc.storage.urls.pin :as pin-urls]
            [oc.storage.urls.entry :as entry-urls]
            [oc.storage.urls.board :as board-urls]
            [oc.storage.urls.org :as org-urls]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.content :as content]
            [oc.storage.resources.reaction :as reaction-res]
            [oc.lib.change.resources.read :as read]))

(def org-prop-mapping {:uuid :org-uuid
                       :name :org-name
                       :slug :org-slug
                       :logo-url :org-logo-url
                       :logo-width :org-logo-width
                       :logo-height :org-logo-height})

(def representation-props [:uuid :headline :body :attachments :status :must-see :sample
                           :org-uuid :org-name :org-slug :org-logo-url :org-logo-width :org-logo-height
                           :board-uuid :board-slug :board-name :board-access :publisher-board
                           :team-id :author :publisher :published-at :video-id :video-processed
                           :video-image :video-duration :created-at :updated-at :revision-id
                           :new-comments-count :bookmarked-at :polls :last-read-at :last-activity-at :sort-value
                           :unseen-comments :pins :labels])

;; ----- Utility functions -----

(defn comments
  "Return a sequence of just the comments for an entry."
  [{interactions :interactions}]
  (filter :body interactions))

(defn reactions
  "Return a sequence of just the reactions for an entry."
  [{interactions :interactions}]
  (filter :reaction interactions))

(defun- board-of
  ([boards :guard map? entry]
    (boards (:board-uuid entry)))
  ([board _entry] board))

;; ----- Representation -----

(defn self-link [org-slug board-slug entry-uuid]
  (hateoas/self-link (entry-urls/entry org-slug board-slug entry-uuid) {:accept mt/entry-media-type}))

(defn- secure-self-link [org-slug entry-uuid]
  (hateoas/self-link (entry-urls/secure-entry org-slug entry-uuid) {:accept mt/entry-media-type}))

(defn secure-link [org-slug secure-uuid]
  (hateoas/link-map "secure" hateoas/GET (entry-urls/secure-entry org-slug secure-uuid) {:accept mt/entry-media-type}))

(defn- create-link [org-slug board-slug]
  (hateoas/create-link (str (entry-urls/entries org-slug board-slug) "/") {:content-type mt/entry-media-type
                                                                           :accept mt/entry-media-type}))

(defn- partial-update-link [org-slug board-slug entry-uuid]
  (hateoas/partial-update-link (entry-urls/entry org-slug board-slug entry-uuid) {:content-type mt/entry-media-type
                                                                                  :accept mt/entry-media-type}))

(defn- delete-link [org-slug board-slug entry-uuid]
  (hateoas/delete-link (entry-urls/entry org-slug board-slug entry-uuid)))

(defn- publish-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "publish" hateoas/POST (entry-urls/publish org-slug board-slug entry-uuid)
    {:content-type mt/entry-media-type
     :accept mt/entry-media-type}))

(defn- react-link [org board entry-uuid]
  (hateoas/link-map "react" hateoas/POST
    (str config/interaction-server-url "/orgs/" (:uuid org) "/boards/" (:uuid board)
                                          "/resources/" entry-uuid "/reactions")
    {:content-type "text/plain"
     :accept mt/reaction-media-type}))

(defn- share-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "share" hateoas/POST (entry-urls/share org-slug board-slug entry-uuid)
    {:content-type mt/share-request-media-type
     :accept mt/entry-media-type}))

(defun up-link 
  ([org-slug nil]
  (hateoas/up-link (org-urls/org org-slug) {:accept mt/org-media-type}))
  ([org-slug board-slug]
  (hateoas/up-link (board-urls/board org-slug board-slug) {:accept mt/board-media-type})))

(defn- revert-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "revert" hateoas/POST (entry-urls/revert org-slug board-slug entry-uuid)
    {:content-type mt/revert-request-media-type
     :accept mt/entry-media-type}))

(defn- add-bookmark-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "bookmark" hateoas/POST (entry-urls/bookmark org-slug board-slug entry-uuid)
    {:accept mt/entry-media-type}))

(defn- remove-bookmark-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "bookmark" hateoas/DELETE (entry-urls/bookmark org-slug board-slug entry-uuid)
    {:accept mt/entry-media-type}))

(defn- follow-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "follow" hateoas/POST (entry-urls/inbox-follow org-slug board-slug entry-uuid)
    {:accept mt/entry-media-type
     :content-type "text/plain"}))

(defn- unfollow-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "unfollow" hateoas/POST (entry-urls/inbox-unfollow org-slug board-slug entry-uuid)
    {:accept mt/entry-media-type
     :content-type "text/plain"}))

(defn- poll-add-reply-link [org-slug board-slug entry-uuid poll-uuid]
  (hateoas/link-map "reply" hateoas/POST (entry-urls/poll-reply org-slug board-slug entry-uuid poll-uuid)
    {:accept mt/poll-reply-media-type
     :content-type "text/plain"}))

(defn- poll-delete-reply-link [org-slug board-slug entry-uuid poll-uuid reply-id]
  (hateoas/link-map "delete" hateoas/DELETE (entry-urls/poll-reply org-slug board-slug entry-uuid poll-uuid reply-id)
    {:accept mt/poll-reply-media-type}))

(defn- poll-vote-link [org-slug board-slug entry-uuid poll-uuid reply-id]
  (hateoas/link-map "vote" hateoas/POST (entry-urls/poll-reply-vote org-slug board-slug entry-uuid poll-uuid reply-id)
    {:accept mt/poll-media-type}))

(defn- poll-unvote-link [org-slug board-slug entry-uuid poll-uuid reply-id]
  (hateoas/link-map "unvote" hateoas/DELETE (entry-urls/poll-reply-vote org-slug board-slug entry-uuid poll-uuid reply-id)
    {:accept mt/poll-media-type}))

(defn- pin-link [rel org-slug board-slug entry-uuid pin-container-uuid]
  (hateoas/link-map rel hateoas/POST (pin-urls/pin org-slug board-slug entry-uuid pin-container-uuid)
   {:accept mt/pin-media-type}))

(defn- label-changes-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "label-changes" hateoas/POST (entry-urls/labels org-slug board-slug entry-uuid)
                    {:accept mt/entry-media-type
                     :content-type mt/json-media-type}))

(defn- add-label-link [org-slug board-slug entry-uuid]
  (hateoas/link-map "partial-add-label" hateoas/POST (entry-urls/label org-slug board-slug entry-uuid "$0")
                    {:accept mt/entry-media-type}
                    {:replace {:label-uuid "$0"}}))

(defn- remove-label-link
  ([org-slug board-slug entry-uuid]
   (hateoas/link-map "partial-remove-label" hateoas/DELETE (entry-urls/label org-slug board-slug entry-uuid "$0")
                     {:accept mt/entry-media-type}
                     {:replace {:label-uuid "$0"}}))
  ([org-slug board-slug entry-uuid label-slug-or-uuid]
   (hateoas/link-map "remove-label" hateoas/DELETE (entry-urls/label org-slug board-slug entry-uuid label-slug-or-uuid)
                     {:accept mt/entry-media-type})))

(defn include-secure-uuid
  "Include secure UUID property for authors."
  [entry secure-uuid _access-level]
  (assoc entry :secure-uuid secure-uuid))

(defn include-interactions
  "Include interactions only if we have some."
  [entry collection key-name]
  (if (empty? collection)
    entry
    (assoc entry key-name collection)))

(defun new-comments-count

  ([entry user-id entry-read :guard map?]
   (new-comments-count (:read-at entry-read)))

  ([entry user-id entry-read-at :guard #(or (nil? %) (string? %))]
  (let [all-comments (filterv :body (:interactions entry))
        filtered-comments (filterv #(not= (-> % :author :user-id) user-id) all-comments)]
    (if (and filtered-comments
             entry-read-at)
      (count (filter #(pos? (compare (:created-at %) entry-read-at)) filtered-comments))
      (count filtered-comments)))))

(defn- unseen-comments? [entry user-id container-seen-at]
  (let [all-comments (filter :body (:interactions entry))
        filtered-comments (filter #(not= (-> % :author :user-id) user-id) all-comments)
        all-unseens (if (and filtered-comments
                             (seq container-seen-at))
                      (filter #(pos? (compare (:created-at %) container-seen-at)) filtered-comments)
                      filtered-comments)]
    (pos? (count all-unseens))))

(defn entry-last-activity-at
  "Return the most recent created-at of the comments, exclude comments from current user if needed."
  [user-id entry]
  (let [all-comments (filterv :body (:interactions entry))
        filtered-comments (filterv #(not= (-> % :author :user-id) user-id) all-comments)
        sorted-comments (sort-by :created-at filtered-comments)]
    (when (seq sorted-comments)
      (-> sorted-comments last :created-at))))

(defn- poll-replies-with-links [poll-replies org-slug board-slug entry-uuid poll-uuid user-id]
  (zipmap
   (mapv (comp keyword :reply-id) (vals poll-replies))
   (mapv (fn [reply]
          (let [user-voted? (and user-id
                                 (seq (filterv #(= % user-id) (:votes reply))))]
            (assoc reply :links (remove nil?
             [(if user-voted?
                (poll-unvote-link org-slug board-slug entry-uuid poll-uuid (:reply-id reply))
                (poll-vote-link org-slug board-slug entry-uuid poll-uuid (:reply-id reply)))
              (when (= user-id (-> reply :author :user-id))
                (poll-delete-reply-link org-slug board-slug entry-uuid poll-uuid (:reply-id reply)))]))))
     (vals poll-replies))))

(defn- polls-with-links [polls org-slug board-slug entry-uuid user-id]
  (zipmap
   (mapv (comp keyword :poll-uuid) (vals polls))
   (mapv (fn [poll] (-> poll
          (update :replies #(poll-replies-with-links % org-slug board-slug entry-uuid (:poll-uuid poll) user-id))
          (assoc :links
           (remove nil?
            [(when (:can-add-reply poll)
               (poll-add-reply-link org-slug board-slug entry-uuid (:poll-uuid poll)))]))))
     (vals polls))))


(defn- filter-pins [entry member?]
  (if member?
    (if-not (= (:board-access entry) "private")
      entry
      (update entry
              :pins
              #(into {} (filter (fn [[k _]] (not= k (keyword config/seen-home-container-id))) %))))
    ;; Remove pins for non members
    (dissoc entry :pins)))

(defn- entry-and-links
  "
  Given an entry and all the metadata about it, render an access level appropriate rendition of the entry
  for use in an API response.
  "
  ([org board entry comments reactions access-map user-id]
   (entry-and-links org board entry comments reactions access-map user-id false))

  ([org board entry comments reactions {:keys [access-level role]} user-id secure-access?]
  (let [entry-uuid (:uuid entry)
        secure-uuid (:secure-uuid entry)
        org-uuid (:org-uuid entry)
        org-slug (:slug org)
        board-uuid (:uuid board)
        board-slug (:slug board)
        board-access (:access board)
        draft? (= :draft (keyword (:status entry)))
        entry-with-comments (assoc entry :interactions comments)
        bookmark (some #(when (= (:user-id %) user-id) %) (:bookmarks entry))
        member? (or (= role :member) (#{:author :viewer} access-level))
        enrich-entry? (and (not draft?)
                           user-id)
        entry-read (when enrich-entry?
                     (read/retrieve-by-user-item config/dynamodb-opts user-id (:uuid entry)))
        rendered-polls (when (seq (:polls entry))
                         (polls-with-links (:polls entry) org-slug board-slug entry-uuid user-id))
        full-entry (merge {:board-slug board-slug
                           :board-access board-access
                           :board-name (:name board)
                           :publisher-board (:publisher-board board)
                           :bookmarked-at (:bookmarked-at bookmark)
                           :last-read-at (:read-at entry-read)
                           :new-comments-count (when enrich-entry?
                                                 (new-comments-count entry-with-comments user-id (:read-at entry-read)))
                           :unseen-comments (when enrich-entry?
                                               (unseen-comments? entry-with-comments user-id (:container-seen-at entry)))
                           :last-activity-at (when enrich-entry?
                                               (entry-last-activity-at user-id entry-with-comments))}
                          entry)
        reaction-list (if (or draft? (= access-level :public))
                        []
                        (content/reactions-and-links org-uuid board-uuid entry-uuid reactions user-id))
        comment-list (if (or draft? (= access-level :public))
                        []
                        (take config/inline-comment-count (reverse (sort-by :created-at comments))))
        bookmarks-link (when (and (not draft?) (not= access-level :public))
                         (if bookmark
                           (remove-bookmark-link org-slug board-slug entry-uuid)
                           (add-bookmark-link org-slug board-slug entry-uuid)))
        user-visibility (when user-id
                          (some (fn [[k v]] (when (= k (keyword user-id)) v)) (:user-visibility entry)))
        user-visibility-link (when-not draft?
                               (if (:unfollow user-visibility)
                                 (follow-link org-slug board-slug entry-uuid)
                                 (unfollow-link org-slug board-slug entry-uuid)))
        home-pin-link (when-not draft?
                        (pin-link "home-pin" org-slug board-slug entry-uuid config/seen-home-container-id))
        board-pin-link (when-not draft?
                         (pin-link "board-pin" org-slug board-slug entry-uuid board-uuid))
        entry-author? (= user-id (:user-id (first (:author entry))))
        entry-publisher? (= user-id (-> entry :publisher :user-id))
        links (cond-> []
               ;; secure UUID access
               secure-access?
               (conj (secure-self-link org-slug secure-uuid))
               ;; normal access
               (and (seq board-slug)
                    (not secure-access?))
               (concat [(self-link org-slug board-slug entry-uuid)
                        (up-link org-slug board-slug)])
               ;; Generic links for all team members: share, bookmark and user-visibility
               (and (not draft?)
                    (not secure-access?)
                    member?)
               (concat [(share-link org-slug board-slug entry-uuid)
                        bookmarks-link
                        user-visibility-link])
               ;; Add home and board pin links if user is a member (clients will limit home pins from private boards)
               (and (or home-pin-link board-pin-link)
                    (not secure-access?)
                    member?)
               (concat [home-pin-link board-pin-link])
               ;; Edit/delete can be done by:
               ;; - admins if the post is published
               ;; - only the author if he has still author access to the board
               ;; In case the user was removed from the board they can still see the draft and copy the content
               ;; no edit/publish/delete though
               (and (not secure-access?)
                    (= access-level :author)
                    (or (and (not draft?)
                             (or (= role :admin)
                                 entry-publisher?))
                        (and draft?
                             entry-author?)))
               (concat [(partial-update-link org-slug board-slug entry-uuid)
                        (delete-link org-slug board-slug entry-uuid)
                        (revert-link org-slug board-slug entry-uuid)])
               ;; Secure link only for authors
               (and (not draft?)
                    (not secure-access?)
                    (= access-level :author))
               (conj (secure-link org-slug secure-uuid))
               ;; Team members always get links to read and add comments
               (and (not draft?)
                    member?)
               (concat [(content/comment-link org-uuid board-uuid entry-uuid)
                        (content/comments-link org-uuid board-uuid entry-uuid comments)])
               ;; Team members always get the mark unread except on secure access
               (and (not draft?)
                    member?
                    (not secure-access?))
               (conj (content/mark-unread-link entry-uuid))
               ;; All memebers can react but only if there are less than x reactions
               (and (not draft?)
                    member?
                    (< (count reaction-list) config/max-reaction-count))
               (conj (react-link org board entry-uuid))
               ;; Drafts need a publish link if user is the author of the post
               ;; and has still access to the board
               (and draft?
                    (= access-level :author)
                    entry-author?)
               (conj (publish-link org board entry-uuid))
               ;; Labels: multiple changes
               (and member? (not secure-access?))
               (conj (label-changes-link org board entry-uuid))
               ;; Label: partial add label link
               (and member? (not secure-access?))
               (concat [(add-label-link org board entry-uuid)
                        (remove-label-link org board entry-uuid)]))]
    (-> (if secure-access?
          ;; "stand-alone", so include extra props
          (-> org
            (clj-set/rename-keys org-prop-mapping)
            (merge full-entry))
          full-entry)
      (assoc :polls rendered-polls)
      (select-keys representation-props)
      (filter-pins member?)
      (include-secure-uuid secure-uuid access-level)
      (include-interactions reaction-list :reactions)
      (include-interactions comment-list :comments)
      (assoc :links links)))))

(defn render-entry-for-collection
  "Create a map of the entry for use in a collection in the API"
  ([org board entry comments reactions access-map user-id]
  (render-entry-for-collection org board entry comments reactions access-map user-id false))

  ([org board entry comments reactions access-map user-id secure-access?]
  (entry-and-links org board entry comments reactions access-map user-id secure-access?)))

(defn render-entry
  "Create a JSON representation of the entry for the API"
  ([org board entry comments reactions access-map user-id]
  (render-entry org board entry comments reactions access-map user-id false))

  ([org board entry comments reactions access-map user-id secure-access?]
  (json/generate-string
    (render-entry-for-collection org board entry comments reactions access-map user-id secure-access?)
    {:pretty config/pretty?})))

(defn render-entry-list
  "
  Given an org and a board or a map of boards, a sequence of entry maps, and access control levels, 
  create a JSON representation of a list of entries for the API.
  "
  [org board-or-boards entries ctx]
  (let [org-slug (:slug org)
        board-slug (:slug board-or-boards)
        collection-url (entry-urls/entries org-slug board-slug)
        links [(hateoas/self-link collection-url {:accept mt/entry-collection-media-type})
               (up-link org-slug board-slug)]
        full-links (if (= (:access-level ctx) :author)
                      (conj links (create-link org-slug board-slug))
                      links)
        user (:user ctx)]
    (json/generate-string
      {:collection {:version hateoas/json-collection-version
                    :href collection-url
                    :links full-links
                    :items (map #(let [board (board-of board-or-boards %)]
                                   (entry-and-links org board %
                                    (or (filter :body (:interactions %)) [])  ; comments only
                                    (reaction-res/aggregate-reactions (or (filter :reaction (:interactions %)) [])) ; reactions only
                                    (select-keys ctx [:access-level :role]) (:user-id user)))
                             entries)}}
      {:pretty config/pretty?})))

(defn render-entry-poll [_entry poll _ct]
  (json/generate-string
    poll
    {:pretty config/pretty?}))