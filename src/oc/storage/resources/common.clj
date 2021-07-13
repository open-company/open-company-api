(ns oc.storage.resources.common
  "Resources are any thing stored in the open company platform: orgs, boards, and entries"
  (:require [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.slugify :as slug]))

;; ----- RethinkDB metadata -----

(def org-table-name "orgs")
(def board-table-name "boards")
(def entry-table-name "entries")
(def interaction-table-name "interactions")
(def label-table-name "labels")
(def versions-table-name (str "versions_" entry-table-name))

;; ----- Properties common to all resources -----

(def reserved-properties
  "Properties of a resource that can't be specified during a create and are ignored during an update."
  #{:id :slug :uuid :board-uuid :org-uuid :author :links :created-at :updated-at :pins})

;; ----- Data Schemas -----

(defn- allowed-org-name? [name]
  (and (string? name)
       (not (re-matches #".*\d(\s*)\d(\s*)\d(\s*)\d(\s*)\d.*" name)) ; don't allow any more than 4 numerals in a row
       (= (count name) (.codePointCount name 0 (count name))))) ; same # of characters as Unicode points

; (defn- allowed-board-name? [name]
;   (and (string? name)
;        (not (re-matches #".*\d\d\d\d.*" name)))) ; don't allow any more than 3 numerals in a row

(def iso8601-re "\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d(\\.\\d+)?(([+-]\\d\\d:\\d\\d)|Z)?")

(def sort-value-pattern (str "(?i)^(" iso8601-re "){0,2}$"))

(def sort-value-re (re-pattern sort-value-pattern))

(defn sort-value? [v]
  (or (nil? v) (re-matches sort-value-re v)))

(def SortValue (schema/pred sort-value?))

(def Slug "Valid slug used to uniquely identify a resource in a visible URL." (schema/pred slug/valid-slug?))

(def Attachment {
  :file-name lib-schema/NonBlankStr
  :file-type (schema/maybe schema/Str)
  :file-size schema/Num
  :file-url lib-schema/NonBlankStr
  :created-at lib-schema/ISO8601})

(def ContributingAuthor
  "An author in a sequence of Authors involved in creating content."
  (merge lib-schema/Author {:updated-at lib-schema/ISO8601}))

(def AccessLevel (schema/pred #(#{:private :team :public} (keyword %))))

(def AllowedBoard
  {:uuid lib-schema/UniqueID
   :access AccessLevel
   schema/Keyword schema/Any})

(def SlackMirror
  (schema/if map? lib-schema/SlackChannel [lib-schema/SlackChannel]))

(def Board
  "An container of entries."
  {
  :uuid lib-schema/UniqueID
  :slug Slug
  :name lib-schema/NonBlankStr
  (schema/optional-key :description) (schema/maybe schema/Str)
  :org-uuid lib-schema/UniqueID
  :access AccessLevel
  :authors [lib-schema/UniqueID]
  :viewers [lib-schema/UniqueID]
  (schema/optional-key :slack-mirror) (schema/maybe SlackMirror)
  :author lib-schema/Author
  (schema/optional-key :draft) schema/Bool
  :created-at lib-schema/ISO8601
  :updated-at lib-schema/ISO8601
  (schema/optional-key :publisher-board) schema/Bool})

(def ContentVisibility {
  (schema/optional-key :disallow-secure-links) schema/Bool
  (schema/optional-key :disallow-public-board) schema/Bool
  (schema/optional-key :disallow-public-share) schema/Bool
  (schema/optional-key :disallow-wrt-download) schema/Bool})

(def Org {
  :uuid lib-schema/UniqueID
  :name (schema/pred allowed-org-name?)
  :slug Slug
  :team-id lib-schema/UniqueID
  (schema/optional-key :logo-url) (schema/maybe schema/Str)
  (schema/optional-key :logo-width) schema/Int
  (schema/optional-key :logo-height) schema/Int
  :promoted schema/Bool
  (schema/optional-key :content-visibility) ContentVisibility
  (schema/optional-key :why-carrot) (schema/maybe schema/Str)
  (schema/optional-key :utm-data) schema/Any
  :authors [lib-schema/UniqueID]
  :author lib-schema/Author
  (schema/optional-key :brand-color) lib-schema/BrandColor
  (schema/optional-key :new-entry-placeholder) (schema/maybe schema/Str)
  (schema/optional-key :new-entry-cta) (schema/maybe schema/Str)
  :created-at lib-schema/ISO8601
  :updated-at lib-schema/ISO8601})

(def ShareMedium (schema/pred #(#{:email :slack} (keyword %))))

(def ShareRequest {
  :medium ShareMedium
  (schema/optional-key :note) (schema/maybe schema/Str)
  
  ;; Email medium
  (schema/optional-key :to) [lib-schema/EmailAddress]
  (schema/optional-key :subject) (schema/maybe schema/Str)
  
  ;; Slack medium
  (schema/optional-key :channel) lib-schema/SlackChannel

  :shared-at lib-schema/ISO8601})

(def Status (schema/pred #(#{:draft :published} (keyword %))))

(def PollReply
  "An option you can vote on in a poll."
  {:body schema/Str
   :author lib-schema/Author
   :reply-id lib-schema/UniqueID
   :created-at lib-schema/ISO8601
   :votes [lib-schema/UniqueID]})

(def Poll
  "A user poll for voting."
  {:poll-uuid lib-schema/UniqueID
   :question schema/Str
   :can-add-reply schema/Bool
   :author lib-schema/Author
   :created-at lib-schema/ISO8601
   :updated-at lib-schema/ISO8601
   :replies {schema/Keyword PollReply}})

;; Labels

(defn valid-label-name? [label-name]
  (and (string? label-name)
       (<= 1 (count label-name) 40)))

(def LabelName (schema/pred valid-label-name?))

(def EntryLabel
  "A label is an object composed by a name and a slug."
  {:uuid lib-schema/NonBlankStr
   :name LabelName
   :slug lib-schema/NonBlankStr})

(def LabelUsedBy
  "An object representing a user that used a label with the number of times."
  {:user-id lib-schema/UniqueID
   :count schema/Num})

(def Label
  "Complete label object like it's stored in the labels table of the db."
  (merge EntryLabel
         {:org-uuid lib-schema/NonBlankStr
          :created-at lib-schema/ISO8601
          :updated-at lib-schema/ISO8601
          :author lib-schema/Author
          (lib-schema/o-k :used-by) (schema/maybe [LabelUsedBy])}))

(def UserVisibility
  "A user-visibility item."
  {(schema/optional-key :dismiss-at) (schema/maybe lib-schema/ISO8601)
   (schema/optional-key :unfollow) schema/Bool
   (schema/optional-key :follow) schema/Bool})

(def FollowUp
  "A follow-up item."
  {
  :uuid lib-schema/UniqueID
  :created-at lib-schema/ISO8601
  :assignee lib-schema/Author
  :author lib-schema/Author
  :completed? schema/Bool
  (schema/optional-key :completed-at) (schema/maybe lib-schema/ISO8601)})

(def PinValue
  {:author lib-schema/Author
   :pinned-at lib-schema/ISO8601})

(def PinKey
  (schema/pred #(and (keyword? %)
                     (-> %
                         name
                         lib-schema/unique-id?))))

(def EntryPins
  {PinKey PinValue})

(def Bookmark
  "A bookmark item"
  {:user-id lib-schema/UniqueID
   :bookmarked-at lib-schema/ISO8601})

(def Entry 
  "An entry on a board."
  {
  :uuid lib-schema/UniqueID
  :secure-uuid lib-schema/UniqueID
  :org-uuid lib-schema/UniqueID
  :board-uuid lib-schema/UniqueID
  
  :status Status

  :headline schema/Str
  :body schema/Str
  (schema/optional-key :abstract) (schema/maybe schema/Str)
  (schema/optional-key :abstract-merged) (schema/maybe schema/Bool)
  
  ;; Attachments
  (schema/optional-key :attachments) [Attachment]

  ;; Comment sync
  (schema/optional-key :slack-thread) lib-schema/SlackThread

  ;; NUX samples
  (schema/optional-key :sample) schema/Bool

  :author [ContributingAuthor]
  (schema/optional-key :published-at) lib-schema/ISO8601
  (schema/optional-key :publisher) lib-schema/Author
  (schema/optional-key :must-see) schema/Bool
  (schema/optional-key :shared) [ShareRequest]

  (schema/optional-key :video-id) (schema/maybe lib-schema/NonBlankStr)
  (schema/optional-key :video-image) (schema/maybe lib-schema/NonBlankStr)
  (schema/optional-key :video-duration) (schema/maybe schema/Str)
  (schema/optional-key :video-transcript) (schema/maybe schema/Str)
  (schema/optional-key :video-processed) (schema/maybe schema/Bool)
  (schema/optional-key :video-error) (schema/maybe schema/Bool)
  :revision-id schema/Int
  :created-at lib-schema/ISO8601
  :updated-at lib-schema/ISO8601

  (schema/optional-key :bookmarks) [Bookmark]
  (schema/optional-key :user-visibility) (schema/maybe {schema/Keyword UserVisibility})

  (schema/optional-key :polls) (schema/maybe {schema/Keyword Poll})

  (schema/optional-key :pins) (schema/maybe EntryPins)
  
  (schema/optional-key :labels) (schema/maybe [EntryLabel])})

(def NewBoard
  "A new board for creation, can have new or existing entries already."
  (merge Board {
    :entries [Entry]}))

(def User
  "User info to notify via email/slack"
  {:updated-at lib-schema/ISO8601
   :email schema/Str
   :last-name schema/Str
   :avatar-url schema/Str
   :user-id lib-schema/UniqueID
   :first-name schema/Str
   :status schema/Str
   :created-at lib-schema/ISO8601
   schema/Keyword schema/Any}) ; and whatever else

;; Defaults

(def default-entry-placeholder "What's happening...")
(def default-entry-cta "New update")

;; Direction
(def Order (schema/enum :desc :asc))

(def Direction (schema/enum :before :after))