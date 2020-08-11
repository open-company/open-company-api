(ns oc.storage.representations.digest
  "Resource representations for OpenCompany digest."
  (:require [cheshire.core :as json]
            [oc.lib.hateoas :as hateoas]
            [oc.storage.api.access :as access]
            [oc.storage.representations.media-types :as mt]
            [oc.storage.representations.org :as org-rep]
            [oc.storage.representations.entry :as entry-rep]
            [oc.storage.config :as config]))

(defn- entry-for-digest
  "
  Given an entry and all the metadata about it, render it with only the data needed by the digest.
  Return a subset of the response of oc.storage.representations.entry/entry-and-links
  "
  [org board entry comments {:keys [access-level role] :as access} user-id]
  (let [entry-uuid (:uuid entry)
        secure-uuid (:secure-uuid entry)
        org-uuid (:org-uuid entry)
        org-slug (:slug org)
        board-uuid (:uuid board)
        board-slug (:slug board)
        board-access (:access board)
        entry-with-comments (assoc entry :interactions comments)
        full-entry (merge {:board-slug board-slug
                           :board-access board-access
                           :board-name (:name board)
                           :new-comments-count (entry-rep/new-comments-count entry-with-comments user-id (:last-read-at entry))
                           :last-activity-at (entry-rep/entry-last-activity-at user-id entry-with-comments)}
                          entry)
        comment-list (if (= access-level :public)
                        []
                        (take config/inline-comment-count (reverse (sort-by :created-at comments))))
        links [(entry-rep/self-link org-slug board-slug entry-uuid)
               (entry-rep/up-link org-slug board-slug)]]
    (-> full-entry
      (select-keys entry-rep/representation-props)
      (entry-rep/include-secure-uuid secure-uuid access-level)
      (entry-rep/include-interactions comment-list :comments)
      (assoc :links links))))

(defn render-digest
  ""
  [params org collection-type results boards user]
  (let [links [(hateoas/up-link (org-rep/url org) {:accept mt/org-media-type})]]
    (json/generate-string
      {:collection {:version hateoas/json-collection-version
                    :links links
                    :direction (:direction params)
                    :start (:start params)
                    :following (map (fn [entry]
                                     (let [board (first (filterv #(= (:slug %) (:board-slug entry)) boards))
                                           access-level (access/access-level-for org board user)]
                                       (entry-for-digest org board entry (entry-rep/comments entry) access-level (:user-id user))))
                                (:following results))
                    :replies (:replies results)
                    :new-boards (:new-boards results)
                    :unfollowing (:unfollowing results)}}
      {:pretty config/pretty?})))