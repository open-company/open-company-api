(ns oc.storage.util.import
  "
  Commandline client to import data into OpenCompany.

  Usage:

  lein run -m oc.storage.util.import -- -d ./opt/samples/buff.edn

  lein run -m oc.storage.util.import -- -d ./opt/samples/
  "
  (:require [clojure.string :as s]
            [clojure.tools.cli :refer (parse-opts)]
            [clojure.walk :refer (keywordize-keys)]
            [oc.lib.db.pool :as db]
            [oc.lib.db.common :as db-common]
            [oc.storage.resources.org :as org]
            [oc.storage.resources.board :as board]
            [oc.storage.resources.entry :as entry]
            [oc.storage.resources.story :as story]
            [oc.storage.config :as c])
  (:gen-class))

(defn exit [status msg]
  (println msg)
  (System/exit status))

;; ----- Resource import -----

(defn import-story [conn org board story-props author]
  (let [board-uuid (:uuid board)
        timestamp (:created-at story-props)
        story-authors (or (:author story-props) [(assoc (dissoc author :teams) :updated-at timestamp)])
        authors (map #(assoc % :teams [(:team-id org)]) story-authors)
        story (story/->story conn board-uuid story-props (first authors))
        fixed-story (-> story
                      (assoc :author story-authors)
                      (assoc :status :published)
                      (assoc :published-at timestamp))]
    (println (str "Creating story '" (:title story) "' on board '" (:name board) "'."))
    (db-common/create-resource conn story/table-name fixed-story timestamp)))

(defn- import-entry [conn org board entry-props author]
  (let [board-uuid (:uuid board)
        timestamp (:created-at entry-props)
        entry-authors (or (:author entry-props) [(assoc (dissoc author :teams) :updated-at timestamp)])
        authors (map #(assoc % :teams [(:team-id org)]) entry-authors)
        entry (entry/->entry conn board-uuid entry-props (first authors))
        fixed-entry (assoc entry :author entry-authors)]
    (println (str "Creating entry at " timestamp " on board '" (:name board) "'"))
    (db-common/create-resource conn entry/table-name fixed-entry timestamp)))

(defn- import-board [conn org board author]
  (println (str "Creating board '" (:name board) "'."))
  (let [storyboard? (:type board)
        empty-board (dissoc board :entries :stories)
        board-props (if storyboard?
                      (board/->storyboard (:uuid org) empty-board author)
                      (board/->board (:uuid org) empty-board author))]
    (if-let [new-board (board/create-board! conn board-props)]
      
      (if storyboard?
        ;; Create the stories
        (do
          (println (str "Creating " (count (:stories board)) " stories."))
          (doseq [story (:stories board)]
            (import-story conn org new-board story author)))

        ;; Create the entries
        (do
          (println (str "Creating " (count (:entries board)) " entries."))
          (doseq [entry (:entries board)]
            (import-entry conn org new-board entry author))))
      
      (do
        (println "\nFailed to create the board!")
        (exit 1 "Board creation failed.")))))

(defn- import-org [conn options data]
  (let [delete (:delete options)
        org (dissoc data :boards :stories)
        boards (:boards data)
        updates (:stories data)
        org-slug (:slug org)
        author (assoc (:author org) :teams [(:team-id org)])
        prior-org (org/get-org conn org-slug)]

    ;; Delete the org if needed
    (when (and delete prior-org)
      (println (str "Deleting org '" org-slug "'."))
      (org/delete-org! conn org-slug))
    
    ;; Conflicting org?
    (when (and (not delete) prior-org)
      (exit 1 (str "An org for '" org-slug "' already exists. Use the -d flag to delete the org on import.")))
    
    ;; Create the org
    (println (str "Creating org '" org-slug "."))
    (if-let [new-org (org/create-org! conn (org/->org org author))]

      (do
        ;; Create the boards
        (println (str "Creating " (count boards) " boards."))
        (doseq [board boards]
          (import-board conn new-org board author)))
        ;; Create the updates
        ; (println (str "Creating " (count updates) " updates."))
        ; (doseq [update updates]
        ;   (import-update conn new-org update author)))

      (do
        (println "\nFailed to create the org!")
        (exit 1 "Org creation failed."))))

  (println "\nImport complete!\n"))

;; ----- CLI -----

(def cli-options
  [["-d" "--delete" "Delete the org if it already exists"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (s/join \newline
     ["\nThis program imports OpenCompany data from EDN file(s)."
      ""
      "Usage:"
      "  lein run -m oc.storage.util.import -- [options] org-data.edn"
      "  lein run -m oc.storage.util.import -- [options] /directory/"
      ""
      "Options:"
      options-summary
      ""
      "Org data: an EDN file with an org, consisting of board(s) with topic entries and stories."
      "Directory: a directory of org data EDN files"
      ""
      "Please refer to ./opt/samples for more information on the file format."
      ""]))

(defn data-msg []
  "\nNOTE: The data file(s) must be an EDN file with an Org map with a sequence of Boards in a :boards property.\n")

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (s/join \newline errors)))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;; Handle help and error conditions
    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 1) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))
    ;; Get the list of files to import
    (try
      (let [arg (first arguments)
            conn (db/init-conn c/db-options)
            edn-file #".*\.edn$"
            filenames (if (re-matches edn-file arg)
                        [arg] ; they specified just 1 file
                        (filter #(re-matches edn-file %) (map str (file-seq (clojure.java.io/file arg)))))] ; a dir
        ;; Import each file
        (doseq [filename filenames]
          (let [data (read-string (slurp filename))]
            (if (and (map? data) (:slug data) (:boards data))
              (import-org conn options (keywordize-keys data))
              (exit 1 (data-msg))))))
      (catch Exception e
        (println e)
        (exit 1 "Exception importing.")))))

(comment

(defn iframe-tag
  [data-thumbnail data-type id-attribute id-value attribs]
  (str "<p><iframe "
            "data-thumbnail='" data-thumbnail "' "
            "data-media-type='" data-type "' "
            id-attribute "='" id-value "' "
            attribs " "
            "class='carrot-no-preview' width='560' height='315' frameborder='0' "
            "webkitallowfullscreen='' mozallowfullscreen='' allowfullscreen=''>"
          "</iframe></p>"))

(defn chart-tag [sheet-id oid]
  (let [thumbnail (str "https://docs.google.com/spreadsheets/d/" sheet-id "/embed/oimg?id=" sheet-id "&amp;oid=" oid "&amp;disposition=ATTACHMENT&amp;bo=false&amp;zx=sohupy30u1p")
        source (str "src='/_/sheets-proxy/spreadsheets/d/" sheet-id "/pubchart?oid=" oid "&amp;format=interactive'")]
    (iframe-tag thumbnail "chart" "data-chart-id" sheet-id source)))

)