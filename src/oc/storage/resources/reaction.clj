(ns oc.storage.resources.reaction)

(defn- reaction-collapse
  "Reducer function that collapses reactions into count and sequence of author based on common reaction unicode."
  [reactions reaction]
  (let [unicode (:reaction reaction)
        author (-> reaction :author :name)
        author-id (-> reaction :author :user-id)]
    (if-let [existing-reaction (reactions unicode)]
      ;; have this unicode already, so add this reaction to it
      (assoc reactions unicode (-> existing-reaction
                                  (update :count + (:count reaction))
                                  (update :authors #(remove nil? (conj % author)))
                                  (update :author-ids #(remove nil? (conj % author-id)))))
      ;; haven't seen this reaction unicode before, so init a new one
      (assoc reactions unicode {:reaction unicode
                                :count (if author 1 0)
                                :authors (remove nil? [author])
                                :author-ids (remove nil? [author-id])}))))

(defn aggregate-reactions
  "
  Given a sequence of individual reaction interactions, collapse it down to a set of distinct reactions
  that include the count of how many times reacted and the list of author names and IDs that reacted.
  "
  [entry-reactions]
  (let [thumb-reaction "👍"
        fixed-entry-reactions (concat [{:reaction thumb-reaction :count 0}] (map #(assoc % :count 1) entry-reactions))
        collapsed-reactions (vals (reduce reaction-collapse {} fixed-entry-reactions))
        check-fn #(= (:reaction %) thumb-reaction)
        thumb-reactions (some #(when (check-fn %) %) collapsed-reactions)
        rest-reactions (filter #(not (check-fn %)) collapsed-reactions)]
    (or (concat [thumb-reactions] rest-reactions) [])))