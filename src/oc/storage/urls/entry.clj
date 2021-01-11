(ns oc.storage.urls.entry
  (:require [defun.core :refer (defun)]
            [oc.storage.urls.org :as org-urls]
            [oc.storage.urls.board :as board-urls]))

(defn entries
  ([org-slug]
   (org-urls/entries org-slug))
  ([org-slug board-slug]
   (str (board-urls/board org-slug board-slug) "/entries")))

(defun entry
  ([org-slug board-slug entry-map :guard map?]
   (entry org-slug board-slug (:uuid entry-map)))

  ([org-slug board-slug entry-uuid]
   (str (entries org-slug board-slug) "/" entry-uuid)))

(defn secure-entry
  ([org-slug secure-uuid] (str (entries org-slug) "/" secure-uuid)))

(defn bookmark
   [org-slug board-slug entry-uuid]
   (str (entry org-slug board-slug entry-uuid) "/bookmark"))

(defn revert
  [org-slug board-slug entry-uuid]
  (str (entry org-slug board-slug entry-uuid) "/revert"))

(defun inbox-action
  [org-slug board-slug entry-uuid inbox-action :guard #{:dismiss :unread :follow :unfollow}]
  (str (entry org-slug board-slug entry-uuid) "/" (name inbox-action)))

(defn inbox-dismiss [org-slug board-slug entry-uuid]
  (inbox-action org-slug board-slug entry-uuid :dismiss))

(defn inbox-unread [org-slug board-slug entry-uuid]
  (inbox-action org-slug board-slug entry-uuid :unread))

(defn inbox-follow [org-slug board-slug entry-uuid]
  (inbox-action org-slug board-slug entry-uuid :follow))

(defn inbox-unfollow [org-slug board-slug entry-uuid]
  (inbox-action org-slug board-slug entry-uuid :unfollow))

(defn publish
  [org-slug board-slug entry-uuid]
  (str (entry org-slug board-slug entry-uuid) "/publish"))

(defn share
  [org-slug board-slug entry-uuid]
  (str (entry org-slug board-slug entry-uuid) "/share"))


;; Polls

(defn polls [org-slug board-slug entry-uuid]
  (str (entry org-slug board-slug entry-uuid) "/polls"))

(defn poll [org-slug board-slug entry-uuid poll-uuid]
  (str (polls org-slug board-slug entry-uuid) "/" poll-uuid))

(defn poll-replies [org-slug board-slug entry-uuid poll-uuid]
  (str (poll org-slug board-slug entry-uuid poll-uuid) "/replies"))

(defn poll-reply [org-slug board-slug entry-uuid poll-uuid reply-id]
  (str (poll-replies org-slug board-slug entry-uuid poll-uuid) "/" reply-id))

(defn poll-reply-vote [org-slug board-slug entry-uuid poll-uuid reply-id]
  (str (poll-reply org-slug board-slug entry-uuid poll-uuid reply-id) "/vote"))