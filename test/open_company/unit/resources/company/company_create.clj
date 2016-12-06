(ns open-company.unit.resources.company.company-create
  (:require [midje.sweet :refer :all]
            [open-company.lib.check :as check]
            [open-company.lib.resources :as r]
            [open-company.lib.db :as db]
            [oc.lib.rethinkdb.pool :as pool]
            [open-company.lib.test-setup :as ts]
            [open-company.resources.common :as common]
            [open-company.resources.company :as c]))

;; ----- Tests -----

(with-state-changes [(before :contents (ts/setup-system!))
                     (after :contents (ts/teardown-system!))
                     (before :facts (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
                                      (c/delete-all-companies! conn)))
                     (after :facts (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
                                     (c/delete-all-companies! conn)))]

  (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
  (facts "about company creation"

    (fact "it fails to create a company if no org-id is provided"
      (c/create-company! conn (c/->company r/open (dissoc r/coyote :org-id))) => (throws Exception))

    (facts "it returns the company after successful creation"
      (c/create-company! conn (c/->company r/open r/coyote)) => (contains r/open)
      (c/get-company conn r/slug) => (contains r/open))

    (fact "it adds a UUID to the company during successful creation"
      (string? (:uuid (c/create-company! conn (c/->company r/open r/coyote)))) => true
      (string? (:uuid (c/get-company conn r/slug))) => true)

    (facts "it accepts unicode company names"
      (doseq [good-name r/names]
        (let [new-oc (assoc r/open :name good-name)]
          (c/create-company! conn (c/->company new-oc r/coyote)) => (contains new-oc)
          (c/get-company conn r/slug) => (contains new-oc)
          (c/delete-company! conn r/slug))))

    (facts "it creates timestamps"
      (let [company (c/create-company! conn (c/->company r/open r/coyote))
            created-at (:created-at company)
            updated-at (:updated-at company)
            retrieved-company (c/get-company conn r/slug)]
        (check/timestamp? created-at) => true
        (check/about-now? created-at) => true
        (= created-at updated-at) => true
        (= created-at (:created-at retrieved-company)) => true
        (= updated-at (:updated-at retrieved-company)) => true))

    (let [add-section (fn [c section-name] (assoc c section-name (merge {:title (name section-name)
                                                                         :headline ""
                                                                         :body ""})))]
      (facts "it returns the sections in the company in the pre-defined order"
        (:sections (c/create-company! conn (c/->company r/open r/coyote))) => []
        (c/delete-company! conn r/slug)
        (:sections (c/create-company! conn (c/->company (add-section r/open :update) r/coyote))) => [:update]
        (c/delete-company! conn r/slug)
        (:sections (c/create-company! conn (c/->company (add-section r/open :values) r/coyote))) => [:values]
        (c/delete-company! conn r/slug)
        (:sections (c/create-company!
                    conn
                    (c/->company (-> r/open
                                     (add-section :mission) (add-section :help)
                                     (add-section :challenges) (add-section :diversity) (add-section :update))
                                 r/coyote))) => (just #{:update :challenges :help :mission :diversity}))))))