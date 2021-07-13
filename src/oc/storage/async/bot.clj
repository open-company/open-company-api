(ns oc.storage.async.bot
  "Publish Slack bot triggers to AWS SQS."
  (:require [amazonica.aws.sqs :as sqs]
            [taoensso.timbre :as timbre]
            [schema.core :as schema]
            [oc.lib.schema :as lib-schema]
            [oc.lib.html :as html-lib]
            [oc.storage.config :as config]))

(def BotTrigger 
  "All Slack bot triggers have the following properties."
  {
    :type (schema/enum "share-entry")
    :bot {
       :token lib-schema/NonBlankStr
       :id lib-schema/NonBlankStr
    }
    :receiver {
      :type (schema/enum :all-members :user :channel)
      :slack-org-id lib-schema/NonBlankStr
      (schema/optional-key :needs-join) (schema/maybe schema/Bool)
      (schema/optional-key :id) schema/Str
  }})

(def ShareEntryTrigger 
  "A Slack bot trigger to share an Entry."
  (merge BotTrigger {
    :note (schema/maybe schema/Str)
    :org-slug lib-schema/NonBlankStr
    :org-name (schema/maybe schema/Str)
    :org-logo-url (schema/maybe schema/Str)
    :board-name (schema/maybe schema/Str)
    :board-access (schema/maybe schema/Str)
    :board-slug lib-schema/NonBlankStr
    :entry-uuid lib-schema/UniqueID
    :headline (schema/maybe schema/Str)
    :body (schema/maybe schema/Str)
    :must-see (schema/maybe schema/Bool)
    :video-id (schema/maybe lib-schema/NonBlankStr)
    :comment-count (schema/maybe schema/Str)
    :publisher lib-schema/Author
    :secure-uuid lib-schema/UniqueID
    :published-at lib-schema/ISO8601
    :auto-share schema/Bool
    :sharer lib-schema/Author
    :shared-at lib-schema/ISO8601
    }))

(defn- get-slack-bot-for [slack-org-id user]
  (let [slack-bots (flatten (vals (:slack-bots user)))
        slack-bots-by-slack-org (zipmap (map :slack-org-id slack-bots) slack-bots)
        slack-bot (get slack-bots-by-slack-org slack-org-id)]
    slack-bot))

(defn has-slack-bot-for? [slack-org-id user]
  (let [slack-bot (get-slack-bot-for slack-org-id user)]
    (and (map? slack-bot)
         (seq (keys slack-bot)))))

(defn- bot-for
  "Extract the right bot from the JWToken for the specified Slack org ID."
  [slack-org-id user]
  (let [slack-bot (get-slack-bot-for slack-org-id user)]
    (if slack-bot
      {:token (:token slack-bot) :id (:id slack-bot)}
      false)))

(schema/defn ^:always-validate ->share-entry-trigger :- ShareEntryTrigger
  "Given an entry for an org and a share request, create the share trigger."
  [org board entry share-request user]
  (let [slack-org-id (-> share-request :channel :slack-org-id)
        channel-type (or (-> share-request :channel :type keyword)
                         :channel)
        needs-join (-> share-request :channel :needs-join)
        comments (:existing-comments entry)
        comment-count (str (count comments))]
    {
      :type "share-entry"
      :receiver {
        :type channel-type
        :slack-org-id slack-org-id
        :needs-join needs-join
        :id (-> share-request :channel :channel-id)
      }
      :bot (bot-for slack-org-id user)
      :note (html-lib/strip-xss-tags (:note share-request))
      :org-slug (:slug org)
      :org-name (:name org)
      :board-name (:name board)
      :board-access (:access board)
      :board-slug (:slug board)
      :org-logo-url (:logo-url org)
      :entry-uuid (:uuid entry)
      :headline (:headline entry)
      :body (:body entry)
      :must-see (:must-see entry)
      :video-id (:video-id entry)
      :comment-count comment-count
      :secure-uuid (:secure-uuid entry)
      :published-at (:published-at entry)
      :publisher (:publisher entry)
      :auto-share (if (:auto-share entry) true false)
      :sharer (lib-schema/author-for-user user)
      :shared-at (:shared-at share-request)
    }))

(defn- send-trigger! [trigger]
  (timbre/trace "Bot request:" (dissoc trigger :entries))
  (timbre/debug "Sending request to:" config/aws-sqs-bot-queue)
  (sqs/send-message
   {:access-key config/aws-access-key-id
    :secret-key config/aws-secret-access-key}
   config/aws-sqs-bot-queue
   trigger)
  (timbre/info "Request sent to:" config/aws-sqs-bot-queue))

(schema/defn ^:always-validate send-share-entry-trigger! [trigger :- ShareEntryTrigger]
  (send-trigger! trigger))
