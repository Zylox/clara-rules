#?(:clj
   (ns clara.test-testing-utils
     (:require [clara.tools.testing-utils :refer [def-rules-test]]
               [clara.rules :as r]

               [clara.rules.testfacts :refer [->Temperature ->Cold]]
               [clojure.test :refer [is deftest run-tests testing] :as t])
     (:import [clara.rules.testfacts
               Temperature
               Cold]
              [clojure.lang
               ExceptionInfo]))

   :cljs
   (ns clara.test-testing-utils
     (:require [clara.rules :as r]
               [clara.rules.testfacts :refer [->Temperature Temperature
                                              ->Cold Cold]]
               [cljs.test :as t])
     (:require-macros [clara.tools.testing-utils :refer [def-rules-test]]
                      [cljs.test :refer (is deftest run-tests testing)])))

(def test-ran-atom (atom false))

;; This test fixture validates that def-rules-test actually executed the test bodies it
;; is provided.  If the test bodies were not executed test-ran-atom would have a value of false
;; after test execution.
(t/use-fixtures :once (fn [t]
                        (reset! test-ran-atom false)
                        (t)
                        (is (true? @test-ran-atom))))

(def-rules-test basic-tests
  {:rules [rule1 [[[?t <- Temperature (< temperature 0)]]
                  (r/insert! (->Cold (:temperature ?t)))]]

   :queries [query1 [[]
                     [[Cold (= ?t temperature)]]]]

   :sessions [session1 [rule1 query1] {}
              session2 [rule1 query1] {:fact-type-fn (fn [fact] :bogus)}]}

  (reset! test-ran-atom true)
  (is (= [{:?t -50}]
         (-> session1
             (r/insert (->Temperature -50 "MCI"))
             r/fire-rules
             (r/query query1))))

  ;; Since we validate later (outside the scope of this test) that the state
  ;; change occurred put it in the middle so that it would fail if we took either
  ;; the first or last test form, rather than all test forms.
  (reset! test-ran-atom true)

  (is (empty? (-> session2
                  (r/insert (->Temperature -50 "MCI"))
                  r/fire-rules
                  (r/query query1)))))

(deftest argument-validation-tests
  (let [partition-counts {:rules 2
                          :queries 2
                          :sessions 3}
        ->msg-pattern #(re-pattern
                         (str "Malformed def-rules-test. " %1 " expects a number of forms divisible by " %2))]

    (doseq [[section failure-test-forms] {:rules '[[rule1]
                                                   [rule1 [[]
                                                           (r/insert! "Why is it called clara anyway?")]
                                                    rule2]]
                                          :queries '[[query1]
                                                     [query1 [[]
                                                              [[Cold (= ?t temperature)]]]
                                                      query2]]
                                          :sessions '[[session1]
                                                      [session1 []]
                                                      [session1 [] {}
                                                       session-without-associated-bindings]
                                                      [session1 [] {}
                                                       session-without-associated-bindings []]]}
            form failure-test-forms]
      (testing (str "Testing " section " for form: " form)
        (is (thrown-with-msg?
            ExceptionInfo
            (->msg-pattern section (section partition-counts))
            (eval `(def-rules-test test-name#
                     {~section ~form}
                     (reset! ~'test-ran-atom true)))))))))