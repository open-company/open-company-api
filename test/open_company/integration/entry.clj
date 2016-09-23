(ns open-company.integration.entry
  (:require [midje.sweet :refer :all]
            [open-company.db.pool :as pool]
            [open-company.lib.test-setup :as ts]
            [open-company.lib.rest-api-mock :as mock]
            [open-company.lib.hateoas :as hateoas]
            [open-company.lib.resources :as r]
            [open-company.lib.db :as db]
            [oc.lib.slugify :refer (slugify)]
            [open-company.api.common :as common]
            [open-company.resources.company :as company]
            [open-company.representations.company :as company-rep]))

(defn get-links-by-rel [rel response]
  (let [links (:links (mock/body-from-response response))]
    (-> (group-by :rel links)
        (get rel))))

(def options  "OPTIONS, GET")

(with-state-changes [(before :contents (ts/setup-system!))
                     (after :contents (ts/teardown-system!))
                     (before :facts (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
                                      (company/delete-all-companies! conn)
                                      (company/create-company! conn (company/->company r/open r/coyote))
                                      (company/create-company! conn (company/->company r/uni r/camus (slugify (:name r/uni))))
                                      (company/create-company! conn (company/->company r/buffer r/sartre))))
                     (after :facts (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
                                     (company/delete-all-companies! conn)))]

  (facts "about available options in entry point"
    (fact "with a bad JWToken"
      (let [response (mock/api-request :options "/" {:auth mock/jwtoken-bad})]
        (:status response) => 401
        (:body response) => common/unauthorized))

    (fact "with no JWToken"
      (let [response (mock/api-request :options "/" {:skip-auth true})]
        (:status response) => 204
        (:body response) => ""
        ((:headers response) "Allow") => options))

    (fact "with a valid JWToken"
      (let [response (mock/api-request :options "/")]
        (:status response) => 204
        (:body response) => ""
        ((:headers response) "Allow") => options)))

  (fact "about failing GET with a bad JWToken"
    (let [response (mock/api-request :get "/" {:auth mock/jwtoken-bad})]
      (:status response) => 401
      (:body response) => common/unauthorized))

  (facts "about links provided by entry point"

    (fact "link to create company"
      (fact "provided if authenticated"
        (let [res (mock/api-request :get "/" {:auth mock/jwtoken-sartre})]
          (:status res) => 200
          (first (get-links-by-rel "company-create" res)) => truthy))
      (fact "not provided if not authenticated"
        (let [res (mock/api-request :get "/" {:skip-auth true})]
          (:status res) => 200
          (first (get-links-by-rel "company-create" res)) => falsey)))

    (fact "link to company list is provided"
      (fact "if authenticated"
        (let [res (mock/api-request :get "/" {:auth mock/jwtoken-sartre})]
          (:status res) => 200
          (first (get-links-by-rel "company-list" res)) => truthy))
      (fact "if not authenticated"
        (let [res (mock/api-request :get "/" {:skip-auth true})]
          (:status res) => 200
          (first (get-links-by-rel "company-list" res)) => truthy)))

    (fact "companies associated with user are listed"
      (let [res (mock/api-request :get "/" {:auth mock/jwtoken-sartre})]
        (:status res) => 200
        (get-links-by-rel "company" res) => seq
        (-> (get-links-by-rel "company" res) first :name) => (:name r/buffer)))))