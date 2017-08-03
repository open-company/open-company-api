(ns oc.storage.representations.media-types)

;; Org media types
(def org-media-type "application/vnd.open-company.org.v1+json")
(def org-collection-media-type "application/vnd.collection+vnd.open-company.org+json;version=1")
(def org-author-media-type "application/vnd.open-company.org.author.v1")

;; Activity media types
(def activity-collection-media-type "application/vnd.collection+vnd.open-company.activity+json;version=1")

;; Board media types
(def board-media-type "application/vnd.open-company.board.v1+json")
(def board-collection-media-type "application/vnd.collection+vnd.open-company.board+json;version=1")
(def board-author-media-type "application/vnd.open-company.board.author.v1")
(def board-viewer-media-type "application/vnd.open-company.board.viewer.v1")
(def topic-list-media-type "application/vnd.open-company.topic-list.v1+json")

;; Entry media types
(def entry-media-type "application/vnd.open-company.entry.v1+json")
(def entry-collection-media-type "application/vnd.collection+vnd.open-company.entry+json;version=1")

;; Interaction media types
(def comment-media-type "application/vnd.open-company.comment.v1+json")
(def comment-collection-media-type "application/vnd.collection+vnd.open-company.comment+json;version=1")
(def reaction-media-type "application/vnd.open-company.reaction.v1+json")
(def reaction-collection-media-type "application/vnd.collection+vnd.open-company.reaction+json;version=1")

;; Story media types
(def story-media-type "application/vnd.open-company.story.v1+json")
(def story-collection-media-type "application/vnd.collection+vnd.open-company.story+json;version=1")
(def share-request-media-type "application/vnd.open-company.share-request.v1+json")