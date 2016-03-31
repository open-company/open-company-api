(ns open-company.integration.section.section-manipulation
  (:require [midje.sweet :refer :all]
            [open-company.lib.rest-api-mock :as mock]
            [open-company.lib.resources :as r]
            [open-company.lib.db :as db]
            [open-company.db.pool :as pool]
            [open-company.lib.test-setup :as ts]
            [open-company.api.common :as common]
            [open-company.resources.common :as common-res]
            [open-company.resources.company :as company]
            [open-company.resources.section :as section]
            [open-company.representations.company :as company-rep]))

;; ----- Test Cases -----

;; PATCHing company's :sections properties with the REST API.

;; The system should support PATCHing the company's :sections property, and handle the following scenarios:

;; fail - invalid JWToken - 401 Unauthorized
;; fail - no JWToken - 401 Unauthorized
;; fail - organization doesn't match companies - 403 Forbidden

;; fail - no matching company slug - 404 Not Found

;; success - reorder sections
;; success - remove sections
;; success - add sections (blank)
;; success - add sections (with content)

;; TODO
;; no accept
;; no content type
;; no charset
;; wrong accept
;; wrong content type
;; wrong charset

;; ----- Tests -----

(def categories (map name common-res/category-names))

(with-state-changes [(around :facts (schema.core/with-fn-validation ?form))
                     (before :contents (ts/setup-system!))
                     (after :contents (ts/teardown-system!))
                     (before :facts (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
                                      (company/delete-all-companies! conn)
                                      (company/create-company! conn (company/->company r/open r/coyote))
                                      (section/put-section conn r/slug :update r/text-section-1 r/coyote)
                                      (section/put-section conn r/slug :finances r/finances-section-1 r/coyote)
                                      (section/put-section conn r/slug :team r/text-section-2 r/coyote)
                                      (section/put-section conn r/slug :help r/text-section-1 r/coyote)
                                      (section/put-section conn r/slug :diversity r/text-section-2 r/coyote)
                                      (section/put-section conn r/slug :values r/text-section-1 r/coyote)))
                     (after :facts (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
                                     (company/delete-all-companies! conn)))]

  (pool/with-pool [conn (-> @ts/test-system :db-pool :pool)]
  (facts "about failing to reorder sections"

    (fact "with an invalid JWToken"
      (let [new-order {:progress ["team" "update" "finances" "help"]
                       :company ["diversity" "values"]}
            response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}
                                                                        :auth mock/jwtoken-bad})]
        (:status response) => 401
        (:body response) => common/unauthorized)
      ;; verify the initial order is unchanged
      (let [db-company (company/get-company conn r/slug)]
        (:categories db-company) => categories
        (:sections db-company) => {:company ["help" "diversity" "values"]
                                   :progress ["update" "team"]
                                   :financial ["finances"]}))

    (fact "with no JWToken"
      (let [new-order {:progress ["team" "update" "finances" "help"]
                       :company ["diversity" "values"]}
            response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}
                                                                        :skip-auth true})]
        (:status response) => 401
        (:body response) => common/unauthorized)
      ;; verify the initial order is unchanged
      (let [db-company (company/get-company conn r/slug)]
        (:categories db-company) => categories
        (:sections db-company) => {:company [ "help" "diversity" "values"]
                                   :progress ["update" "team"]
                                   :financial ["finances"]}))

    (fact "with an organization that doesn't match the company"
      (let [new-order {:progress ["team" "update" "finances" "help"]
                       :company ["diversity" "values"]}
            response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}
                                                                        :auth mock/jwtoken-sartre})]
        (:status response) => 403
        (:body response) => common/forbidden)
      ;; verify the initial order is unchanged
      (let [db-company (company/get-company conn r/slug)]
        (:categories db-company) => categories
        (:sections db-company) => {:company [ "help" "diversity" "values"]
                                   :progress ["update" "team"]
                                   :financial ["finances"]}))

    (fact "with no company matching the company slug"
      (let [new-order {:company ["diversity" "values"]
                       :progress ["team" "update" "finances" "help"]
                       :financial []}
            response (mock/api-request :patch (company-rep/url "foo") {:body {:sections new-order}})]
        (:status response) => 404
        (:body response) => "")
      ;; verify the initial order is unchanged
      (let [db-company (company/get-company conn r/slug)]
        (:categories db-company) => categories
        (:sections db-company) => {:company [ "help" "diversity" "values"]
                                   :progress ["update" "team"]
                                   :financial ["finances"]})))

  (facts "about section reordering"

    ;; verify the initial order
    (let [db-company (company/get-company conn r/slug)]
      (:categories db-company) => categories
      (:sections db-company) => {:company [ "help" "diversity" "values"]
                                 :progress ["update" "team"]
                                 :financial ["finances"]})

    (facts "when the new order is valid"

      (fact "the section order in the progress category can be gently adjusted"
        (let [new-order {:progress ["team" "update" "help"]
                         :financial ["finances"]
                         :company ["diversity" "values"]}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}})]
          (:status response) => 200
          (:sections (mock/body-from-response response)) => new-order
          ;; verify the new order
          (:sections (company/get-company conn r/slug)) => new-order))

      (fact "the sections order in the progress category can be greatly adjusted"
        (let [new-order {:progress ["help" "team" "update"]
                         :financial ["finances"]
                         :company ["diversity" "values"]}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}})]
          (:status response) => 200
          (:sections (mock/body-from-response response)) => new-order
          ;; verify the new order
          (:sections (company/get-company conn r/slug)) => new-order))

      (fact "the section order in the company category can be adjusted"
        (let [new-order {:progress ["update" "team" "help"]
                         :financial ["finances"]
                         :company ["values" "diversity"]}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}})]
          (:status response) => 200
          (:sections (mock/body-from-response response)) => new-order
          ;; verify the new order
          (:sections (company/get-company conn r/slug)) => new-order))

      (fact "the section order in the progress and company category can both be adjusted at once"
        (let [new-order {:progress ["help" "team" "update"]
                         :financial ["finances"]
                         :company ["values" "diversity"]}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}})]
          (:status response) => 200
          (:sections (mock/body-from-response response)) => new-order
          ;; verify the new order
          (:sections (company/get-company conn r/slug)) => new-order)))

    (future-facts "when the new order is NOT valid"))

  (facts "about placeholder section removal"
    (let [slug     "hello-world"
          payload  {:name "Hello World" :description "x"}
          response (mock/api-request :post "/companies" {:body payload})
          company  (company/get-company conn slug)]
      ;; ensure all placeholder sections are in company
      (:sections company) => {:progress ["update" "growth" "challenges" "team" "product"]
                              :financial ["finances"]
                              :company ["mission" "values"]}
      (:growth company) => truthy
      (:challenges company) => truthy
      (:team company) => truthy
      (:product company) => truthy
      (:mission company) => truthy
      (let [new-set {:company ["values"]
                     :progress ["update" "finances" "help"]
                     :financial []}
            response (mock/api-request :patch (company-rep/url slug) {:body {:sections new-set}})
            body     (mock/body-from-response response)
            company  (company/get-company conn slug)]
        (:status response) => 200
        (:sections company) => new-set
        (:growth company) => falsey
        (:challenges company) => falsey
        (:team company) => falsey
        (:product company) => falsey
        (:mission company) => falsey)))

  (facts "about section removal"

    ;; verify the initial set of sections
    (:sections (company/get-company conn r/slug)) => {:company ["help" "diversity" "values"]
                                                 :progress ["update" "team"]
                                                 :financial ["finances"]}

      (fact "a section can be removed from the progress category"
        (let [new-set {:company ["help" "diversity" "values"]
                       :progress ["update"]
                       :financial ["finances"]}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-set}})
              body (mock/body-from-response response)
              db-company (company/get-company conn r/slug)]
          (:status response) => 200
          (:sections body) => new-set
          (:update body) => (contains r/text-section-1)
          (:finances body) => (contains r/finances-section-1)
          (:team body) => nil
          (:help body) => (contains r/text-section-1)
          (:diversity body) => (contains r/text-section-2)
          (:values body) => (contains r/text-section-1)
          ;; verify the new set
          (:sections db-company) => new-set
          (:update db-company) => (contains r/text-section-1)
          (:finances db-company) => (contains r/finances-section-1)
          (:team db-company) => nil
          (:help db-company) => (contains r/text-section-1)
          (:diversity db-company) => (contains r/text-section-2)
          (:values db-company) => (contains r/text-section-1)))

      (fact "multiple sections can be removed from the progress category"
        (let [new-set {:company ["help" "diversity" "values"]
                       :progress []
                       :financial ["finances"]}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-set}})
              body (mock/body-from-response response)
              db-company (company/get-company conn r/slug)]
          (:status response) => 200
          (:sections body) => new-set
          (:update body) => nil
          (:team body) => nil
          (:finances body) => (contains r/finances-section-1)
          (:help body) => (contains r/text-section-1)
          (:diversity body) => (contains r/text-section-2)
          (:values body) => (contains r/text-section-1)
          ;; verify the new set
          (:sections db-company) => new-set
          (:update db-company) => nil
          (:team db-company) => nil
          (:finances db-company) => (contains r/finances-section-1)
          (:help db-company) => (contains r/text-section-1)
          (:diversity db-company) => (contains r/text-section-2)
          (:values db-company) => (contains r/text-section-1)))

      (fact "a section can be removed from the company category"
        (let [new-order {:company ["help" "diversity"]
                         :progress ["update" "team"]
                         :financial ["finances"]}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}})
              body (mock/body-from-response response)
              db-company (company/get-company conn r/slug)]
          (:status response) => 200
          (:sections body) => new-order
          (:update body) => (contains r/text-section-1)
          (:finances body) => (contains r/finances-section-1)
          (:team body) => (contains r/text-section-2)
          (:help body) => (contains r/text-section-1)
          (:diversity body) => (contains r/text-section-2)
          (:values body) => nil
          ;; verify the new set
          (:sections db-company) => new-order
          (:update db-company) => (contains r/text-section-1)
          (:finances db-company) => (contains r/finances-section-1)
          (:team db-company) => (contains r/text-section-2)
          (:help db-company) => (contains r/text-section-1)
          (:diversity db-company) => (contains r/text-section-2)
          (:values db-company) => nil))

      (fact "sections can be removed from all the categories at once"
        (let [new-order {:company ["values"]
                         :progress ["update" "help"]
                         :financial []}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-order}})
              body (mock/body-from-response response)
              db-company (company/get-company conn r/slug)]
          (:status response) => 200
          (:sections body) => new-order
          (:update body) => (contains r/text-section-1)
          (:finances body) => nil
          (:team body) => nil
          (:help body) => (contains r/text-section-1)
          (:diversity body) => nil
          (:values body) => (contains r/text-section-1)
          ;; verify the new set
          (:sections db-company) => new-order
          (:update db-company) => (contains r/text-section-1)
          (:finances db-company) => nil
          (:team db-company) => nil
          (:help db-company) => (contains r/text-section-1)
          (:diversity db-company) => nil
          (:values db-company) => (contains r/text-section-1))))
  
  (facts "about adding sections"

    (fact "that don't really exist"
      (let [new-sections {:company [] :progress ["health"] :financial []}
            response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-sections}})
            body (mock/body-from-response response)]
        (:status response) => 422))

    (facts "without any section content"
    
      (fact "that never existed"
        (let [new-sections {:company [] :progress ["highlights"] :financial []}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-sections}})
              body (mock/body-from-response response)
              resp-highlights (:highlights body)
              db-company (company/get-company conn r/slug)
              db-highlights (:highlights db-company)
              placeholder (dissoc (common-res/section-by-name :highlights) :section-name :core)]
          (:status response) => 200
          (:sections body) => new-sections
          ; verify placeholder flag and content in response
          (:placeholder resp-highlights) => true 
          resp-highlights => (contains placeholder)
          ; verify placeholder flag and content in DB
          (:placeholder db-highlights) => true
          db-highlights => (contains placeholder)))

      (future-fact "that used to exist"
        (let [new-sections {:company [] :progress [] :financial []}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-sections}})
              body (mock/body-from-response response)
              db-company (company/get-company conn r/slug)]
          (:status response) => 200
          (:sections body) => new-sections
          ; verify update is not in the response
          (:update body) => nil
          ; verify update not in the DB
          (:update db-company) => nil)
        (let [new-sections {:company [] :progress ["update"] :financial []}
              response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-sections}})
              body (mock/body-from-response response)
              db-company (company/get-company conn r/slug)]
          (:status response) => 200
          (:sections body) => new-sections
          ; verify update is not in the response
          (:update body) => (contains r/text-section-1)
          ; verify update not in the DB
          (:update db-company) => (contains r/text-section-1))))

    (facts "with section content"

      (let [new-sections {:company ["help" "diversity" "values"]
                            :progress ["update" "team" "kudos"]
                            :financial ["finances"]}
            kudos-placeholder (common-res/section-by-name "kudos")]

        (future-fact "with minimal content"
          (let [kudos-content {:body "Fred is killing it"}
                response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-sections
                                                                                 :kudos kudos-content}})
                body (mock/body-from-response response)
                db-company (company/get-company conn r/slug)]
            (:status response) => 200
            (:body response) => nil))

        (future-fact "with maximal content"
          (let [kudos-content {:title "Great Jobs!" :headline "Good stuff" :body "Fred is killing it"}
                response (mock/api-request :patch (company-rep/url r/slug) {:body {:sections new-sections
                                                                                   :kudos kudos-content}})
                body (mock/body-from-response response)
                db-company (company/get-company conn r/slug)]
            (:status response) => 200
            (:body response) => nil))

        (future-fact "with too much content"
      
          (future-fact "extra properties aren't allowed")

          (future-fact "read/only properties are ignored")))))))