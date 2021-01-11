(ns oc.storage.async.email
  "Publish email triggers to AWS SQS."
  (:require [amazonica.aws.sqs :as sqs]
            [taoensso.timbre :as timbre]
            [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.text :as str]
            [oc.storage.config :as config]))

(def EmailTrigger
  {:type (schema/enum "share-entry")
   :to [lib-schema/EmailAddress]
   :subject (schema/maybe schema/Str)
   :note (schema/maybe schema/Str)
   :reply-to (schema/maybe schema/Str)
   :sharer-name (schema/maybe schema/Str)
   :sharer-avatar-url (schema/maybe schema/Str)
   :org-slug lib-schema/NonBlankStr
   :org-name (schema/maybe schema/Str)
   :org-logo-url (schema/maybe schema/Str)
   :org-logo-width (schema/maybe schema/Int)
   :org-logo-height (schema/maybe schema/Int)
   :board-name (schema/maybe schema/Str)
   :board-access (schema/maybe schema/Str)
   :headline schema/Str
   :body (schema/maybe schema/Str)
   :must-see (schema/maybe schema/Bool)
   :video-id (schema/maybe schema/Str)
   :video-image (schema/maybe schema/Str)
   :video-duration (schema/maybe schema/Str)
   :secure-uuid lib-schema/UniqueID
   :publisher lib-schema/Author
   :published-at lib-schema/ISO8601
   :shared-at lib-schema/ISO8601})

(defn ->trigger [org board entry share-request user]
  {:type "share-entry"
   :to (vec (:to share-request))
   :subject (:subject share-request)
   :note (str/strip-xss-tags (:note share-request))
   :reply-to (:email user)
   :sharer-name (:name user)
   :sharer-avatar-url (:avatar-url user)
   :org-slug (:slug org)
   :org-name (:name org)
   :org-logo-url (:logo-url org)
   :org-logo-width (:logo-width org)
   :org-logo-height (:logo-height org)
   :board-name (:name board)
   :board-access (:access board)
   :headline (:headline entry)
   :body (:body entry)
   :must-see (:must-see entry)
   :video-id (:video-id entry)
   :video-image (or (:video-image entry) "")
   :video-duration (or (:video-duration entry) "")
   :secure-uuid (:secure-uuid entry)
   :publisher (lib-schema/author-for-user (:publisher entry))
   :published-at (:published-at entry)
   :shared-at (:shared-at share-request)})

(defn send-trigger! [trigger]
  (timbre/info "Email request to queue:" config/aws-sqs-email-queue)
  (timbre/trace "Email request:" (dissoc trigger :entries))
  (schema/validate EmailTrigger trigger)
  (timbre/info "Sending request to queue:" config/aws-sqs-email-queue)
  (sqs/send-message
   {:access-key config/aws-access-key-id
    :secret-key config/aws-secret-access-key}
   config/aws-sqs-email-queue
   trigger)
  (timbre/info "Request sent to:" config/aws-sqs-email-queue))
