(ns oc.storage.representations.board
  "Resource representations for OpenCompany boards."
  (:require [cheshire.core :as json]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.config :as config]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.resources.board :as board-res]
            [oc.storage.urls.entry :as entry-urls]
            [oc.storage.urls.board :as board-urls]
            [oc.storage.urls.org :as org-urls]
            [oc.storage.representations.entry :as entry-rep]
            [oc.storage.representations.activity :as activity-rep]
            [oc.storage.resources.reaction :as reaction-res]))

(defn drafts-board? [board]
  (or (= (:uuid board) (:uuid board-res/default-drafts-board))
      (= (:slug board) (:slug board-res/default-drafts-board))))

(def public-representation-props [:uuid :slug :name :access :promoted :entries :created-at :updated-at :links :description :last-entry-at :direction :start])
(def representation-props (concat public-representation-props [:slack-mirror :author :authors :viewers :draft :publisher-board :total-count]))
(def drafts-board-representation-props (conj public-representation-props :total-count))

(defn- self-link 
  ([org-slug slug]
    (self-link org-slug slug {}))
  ([org-slug slug options]
  (let [rel "self"
        board-url (board-urls/board org-slug slug)]
    (hateoas/link-map rel hateoas/GET board-url {:accept mt/board-media-type} options))))

(defn- create-entry-link [org-slug slug] (hateoas/create-link (str (entry-urls/entries org-slug slug) "/")
                                                              {:content-type mt/entry-media-type
                                                               :accept mt/entry-media-type}))

(defn- partial-update-link [org-slug slug] (hateoas/partial-update-link (board-urls/board org-slug slug)
                                                                        {:content-type mt/board-media-type
                                                                         :accept mt/board-media-type}))

(defn- delete-link [org-slug slug]
  (hateoas/delete-link (board-urls/board org-slug slug)))

(defn- add-author-link [org-slug slug] 
  (hateoas/add-link hateoas/POST (str (board-urls/author org-slug slug) "/") {:content-type mt/board-author-media-type}))

(defn- add-viewer-link [org-slug slug] 
  (hateoas/add-link hateoas/POST (str (board-urls/viewer org-slug slug) "/") {:content-type mt/board-viewer-media-type}))

(defn- remove-author-link [org-slug slug user-id]
  (hateoas/remove-link (board-urls/author org-slug slug user-id)))

(defn- remove-viewer-link [org-slug slug user-id]
  (hateoas/remove-link (board-urls/viewer org-slug slug user-id)))

(defn- up-link [org-slug] (hateoas/up-link (org-urls/org org-slug) {:accept mt/org-media-type}))

(defn- pagination-link
  "Add `next` links for pagination as needed."
  [org board {:keys [direction refresh limit]} {total-count :total-count next-count :next-count :as data}]
  (when (and (seq (:entries data))
             (not= next-count total-count)
             (or refresh
                 (= next-count limit)))
    (let [pagination-start (-> data :entries last :sort-value)
          pagination-direction (if refresh (activity-rep/invert-direction direction) direction)
          next-url (board-urls/board org board {:start pagination-start
                                                :direction pagination-direction})]
      (hateoas/link-map "next" hateoas/GET next-url {:accept mt/board-media-type}))))

(defn- refresh-link
  "Add `refresh` links for pagination as needed to reload the whole set the client holds."
  [org board {refresh :refresh direction :direction} data]
  (when (seq (:entries data))
    (let [refresh-start (-> data :entries last :sort-value)
          refresh-direction (if refresh direction (activity-rep/invert-direction direction))
          refresh-url (board-urls/board org board {:start refresh-start
                                                   :direction refresh-direction
                                                   :refresh true})]
      (hateoas/link-map "refresh" hateoas/GET refresh-url {:accept mt/board-media-type}))))

(defn- board-collection-links [board org-slug draft-count ctx]
  (let [board-slug (:slug board)
        options (if (zero? draft-count) {} {:count draft-count})
        is-drafts-board? (drafts-board? board)
        links [(self-link org-slug board-slug options)]
        author? (= :author (:access-level board))
        can-create-entry? (or (:premium? ctx)
                              (= "team" (:access board)))
        full-links (cond-> links
                     (and (not is-drafts-board?)
                          author?
                          can-create-entry?)
                     ;; User is author and can create posts (premium org or team board)
                     (concat [(create-entry-link org-slug board-slug)
                              (partial-update-link org-slug board-slug)
                              (delete-link org-slug board-slug)])
                     (and (not is-drafts-board?)
                          author?)
                     ;; User is author but no premium (no team board)
                     (concat [(partial-update-link org-slug board-slug)
                              (delete-link org-slug board-slug)]))]
    (assoc board :links full-links)))

(defn- board-links
  [board org-slug access-level params]
  (let [slug (:slug board)
        is-drafts-board? (drafts-board? board)
        page-link (when-not is-drafts-board? (pagination-link org-slug slug params board))
        ref-link (when-not is-drafts-board? (refresh-link org-slug slug params board))
        ;; Everyone gets these
        links (remove nil? [page-link
                            ref-link
                            (self-link org-slug slug)
                            (up-link org-slug)])
        ;; Authors get board management links
        full-links (if (= access-level :author)
                     (concat links [(create-entry-link org-slug slug)
                                    (partial-update-link org-slug slug)
                                    (delete-link org-slug slug)
                                    (add-author-link org-slug slug)
                                    (add-viewer-link org-slug slug)])
                     links)]
    (assoc board :links full-links)))

(defn render-author-for-collection
  "Create a map of the board author for use in a collection in the REST API"
  [org-slug board-slug user-id access-level]
  {:user-id user-id
   :links (if (= access-level :author) [(remove-author-link org-slug board-slug user-id)] [])})

(defn render-viewer-for-collection
  "Create a map of the board viewer for use in a collection in the REST API"
  [org-slug slug user-id access-level]
  {:user-id user-id
   :links (if (= access-level :author) [(remove-viewer-link org-slug slug user-id)] [])})

(defn render-entry-for-collection
  "Create a map of the activity for use in a collection in the API"
  [org board entry access-level user-id]
  (entry-rep/render-entry-for-collection org board entry (entry-rep/comments entry) (reaction-res/aggregate-reactions (entry-rep/reactions entry)) access-level user-id))

(defn render-board-for-collection
  "Create a map of the board for use in a collection in the REST API"
  ([org-slug board ctx] (render-board-for-collection org-slug board ctx 0))

  ([org-slug board ctx draft-entry-count]
  (let [this-board-count (if (drafts-board? board) draft-entry-count 0)]
    (-> board
      (select-keys (conj representation-props :access-level))
      (board-collection-links org-slug this-board-count ctx)
      (dissoc :access-level)))))

(defn render-board
  "Create a JSON representation of the board for the REST API"
  [org board ctx params]
  (let [{access-level :access-level :as board-access} (select-keys ctx [:access-level :role :premium?])
        viewer-or-author? (#{:author :viewer} access-level)
        is-drafts-board? (drafts-board? board)
        rep-props (cond is-drafts-board?
                        drafts-board-representation-props
                        viewer-or-author?
                        representation-props
                        :else
                        public-representation-props)
        boards-map (:existing-org-boards ctx)
        author-reps (when-not is-drafts-board?
                      (map #(render-author-for-collection (:slug org) (:slug board) % access-level) (:authors board)))
        viewer-reps (when-not is-drafts-board?
                      (map #(render-viewer-for-collection (:slug org) (:slug board) % access-level) (:viewers board)))]
    (json/generate-string
      (as-> board b
        (if is-drafts-board?
          b
          (assoc b :authors author-reps))
        (if is-drafts-board?
          b
          (assoc b :viewers viewer-reps))
        (assoc b :direction (:direction params))
        (assoc b :start (:start params))
        (board-links b (:slug org) access-level params)
        (assoc b :entries (remove nil?
                                  (map (fn [entry]
                                         (let [entry-board (if is-drafts-board? (boards-map (:board-uuid entry)) board)
                                               entry-board-access (if is-drafts-board?
                                                                    (assoc board-access :access-level (:access-level entry-board))
                                                                    board-access)]
                                           (when entry-board
                                             (render-entry-for-collection org entry-board entry entry-board-access (-> ctx :user :user-id)))))
                                       (:entries board))))
        (select-keys b rep-props))
      {:pretty config/pretty?})))