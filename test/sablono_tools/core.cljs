(ns sablono-tools.core
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testing test-var)])
  (:require [cemerick.cljs.test :as t] ;; cemerick.cljs.test must be required even if not explicitly used
            [sablono-tools.transforms :refer [replace-vars content any-node template]]))


(deftest any-node-test
  (let [source-template [:html
                         [:div "Tipping Points ${report-date}"
                          [:a {:href "${model-predictions}"} "Click here"]]]
        vars {:n-days 7
              :report-date "2014-01-01"
              :model-predictions "http://example.com/model-predictions.csv"}
        forms [[:html any-node] (replace-vars vars)]]

    (is (= (template source-template forms)
           [:html
            [:div "Tipping Points 2014-01-01"
             [:a {:href "http://example.com/model-predictions.csv"} "Click here"]]]))))


(deftest tag-test
  (let [source-template [:html
                         [:div {:id "noonie"} "Tipping Points ${report-date}"]]
        forms [[:#noonie] (content "foo")]]

    (is (= (template source-template forms)
           [:html
            [:div {:id "noonie"} "foo"]]))))


