(ns oc.storage.urls.activity
    (:require [oc.storage.urls.org :as org-urls]
              [cuerdas.core :as string]))

(def allowed-parameter-keys  [:start :sort-type :direction :following :unfollowing])

(defn parametrize [url parameters-dict]
  (let [concat? (#{"?" "&"} (last url))
        has-param? (string/includes? url "?")
        concat-url (cond concat? url
                         has-param? (str url "&")
                         :else (str url "?"))
        url-params (select-keys parameters-dict allowed-parameter-keys)]
    (str concat-url
         (apply str
                (for [[k v] url-params
                      :when (and v k)]
                  (str (if (keyword? k) (name k) k) "=" (if (keyword? v) (name v) v) "&"))))))

;; Urls

(defn collection ;; inbox-url

  ([collection-type org]
   (org-urls/org-container org collection-type))

  ([collection-type org url-params]
   (parametrize (collection collection-type org) url-params)))

(defn inbox-dismiss-all [org] ;; dismiss-all-url
  (str (org-urls/inbox org) "/dismiss"))

(defn activity ;; url
  ([collection-type {slug :slug} sort-type]
   (let [sort-path (when (= sort-type :recent-activity) "?sort=activity")]
     (str (collection collection-type slug) sort-path)))

  ([collection-type org sort-type url-params]
   (parametrize (activity collection-type org sort-type) url-params)))

(defn replies
  ([org] (org-urls/replies org))
  ([org url-params]
   (parametrize (replies org) url-params)))

(defn follow
  ([collection-type org-slug sort-type]
   (let [sort-path (when (= sort-type :recent-activity) "?sort=activity&")]
     (str (org-urls/org-container org-slug collection-type) sort-path)))

  ([collection-type org-slug sort-type url-params]
   (parametrize (follow collection-type org-slug sort-type) url-params)))

(defn following
  ([collection-type {slug :slug} sort-type]
   (follow collection-type slug sort-type {:following true}))

  ([collection-type {slug :slug} sort-type params]
   (follow collection-type slug sort-type params)))

(defn unfollowing
  ([collection-type {slug :slug} sort-type]
   (follow collection-type slug sort-type {:unfollowing true}))

  ([collection-type {slug :slug} sort-type params]
   (follow collection-type slug sort-type params)))

(defn contributions

  ([{slug :slug} author-uuid sort-type]
   (let [sort-path (when (= sort-type :recent-activity) "?sort=activity&")]
     (str (org-urls/contribution slug author-uuid) sort-path)))

  ([org author-uuid sort-type url-params]
   (parametrize (contributions org author-uuid sort-type) url-params)))