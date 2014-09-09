(ns sablono-tools.core
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testing test-var)])
  (:require [cemerick.cljs.test :as t] ;; cemerick.cljs.test must be required even if not explicitly used
            [sablono-tools.transforms :refer [replace-vars content any-node template]]))


(deftest tag-and-content-test
  (let [source [:div
                [:p "Replace me"]
                [:p "Me Too"]]

        forms [[:p] (content "New Text")]

        expected [:div
                  [:p nil "New Text"]
                  [:p nil "New Text"]]]

    (is (= (template source forms) expected)
        "tag-and-content replacement failed")))


(deftest id-and-content-test
  (let [source [:html
                [:div {:id "noonie"} "Replace Me}"]
                [:div {:id "nunie"} "Not Me"]]

        forms [[:#noonie] (content "foo")]

        expected [:html
                  [:div {:id "noonie"} "foo"]
                  [:div {:id "nunie"} "Not Me"]]]

    (is (= (template source forms) expected)
        "id-and-content replacement failed")))


(deftest any-node-replace-vars-test
  (let [source [:html
                [:div "Tipping Points ${report-date}"
                 [:a {:href "${model-predictions}"} "Click here"]]]

        vars {:report-date "2014-01-01"
              :model-predictions "http://example.com/model-predictions.csv"}

        forms [[:html any-node] (replace-vars vars)]

        expected [:html
                  [:div "Tipping Points 2014-01-01"
                   [:a {:href "http://example.com/model-predictions.csv"} "Click here"]]]]

    (is (= (template source forms) expected)
        "replace-vars failed")))




