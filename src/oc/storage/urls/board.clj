(ns oc.storage.urls.board
  (:require [defun.core :refer (defun)]
            [oc.storage.urls.org :as org-urls]))

(defun board
  ([org board-slug :guard string? sort-type :guard keyword? params :guard map?]
    (let [concat-str (if (= sort-type :recent-activity) "&" "?")]
      (str (board org board-slug sort-type) concat-str "start=" (:start params) "&direction=" (name (:direction params)))))

  ([org board-slug :guard string? sort-type :guard keyword?]
   (let [sort-path (when (= sort-type :recent-activity) "?sort=activity")]
     (str (org-urls/boards org) "/" board-slug sort-path)))

  ([org board-map :guard map?]
    (board org (:slug board-map) :recently-posted))

  ([org slug :guard string?]
    (board org slug :recently-posted)))

(defn create
  [org board-type]
  (str (org-urls/boards org)
       "/create-board/"
       (if (keyword? board-type) (name board-type) board-type)))

(defn create-preflight
  [org]
  (str (create org :team) "/preflight"))

(defn viewer
  ([org board-slug]
   (str (board org board-slug) "/viewers"))
  ([org board-slug viewer-uuid]
   (str (viewer org board-slug)
        (when viewer-uuid
          (str "/" (if (keyword? viewer-uuid) (name viewer-uuid) viewer-uuid))))))

(defn author
  ([org board-slug]
   (str (board org board-slug) "/authors"))
  ([org board-slug author-uuid]
   (str (author org board-slug)
        (when author-uuid
          (str "/" (if (keyword? author-uuid) (name author-uuid) author-uuid))))))