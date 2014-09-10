(ns sablono-tools.core
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testing test-var)])
  (:require [cemerick.cljs.test :as t] ;; cemerick.cljs.test must be required even if not explicitly used
            [sablono-tools.transforms :refer [attr? replace-vars content any-node template]]))



(deftest p-test
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[:p] (content "Hello")]

        expected [:div
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "p-test failed")))


(deftest fn-test
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[(attr? :lang)] (content "Hello")]

        expected [:div
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "fn-test failed")))


(deftest descendant-test
  ":p and (attr? lang) appear as separate steps so they are chained.
  This will match any elements with a 'lang' attribute
  inside a :p element."
  (let [source [:div
                [:p {:lang "EN"}
                 [:a {:href "http://link.com" :lang "FR"} "Click-bête"]]]

        forms [[:p (attr? :lang)] (content "Hello")]

        expected [:div
                  [:p {:lang "EN"}
                   [:a {:href "http://link.com" :lang "FR"} "Hello"]]]]
    (is (= (template source forms) expected)
        "descendant-test failed")))


(deftest and-test
  ":p and (attr? lang) appear within one step so they are anded
  (note the extra set of [] surrounding them) rather than chained.
  This will match any element that is a :p AND has a 'lang' attribute.
  "
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[[:p (attr? :lang)]] (content "Hello")]

        expected [:div
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "and-test failed")))


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
  "selector will select any node descended from :html"
  (let [source [:html
                [:div "Tipping Points ${report-date}"
                 [:a {:href "${model-predictions}"} "Click here"]]]

        vars {:report-date "2014-01-01"
              :model-predictions "http://example.com/model-predictions.csv"}

        forms [[any-node] (replace-vars vars)]

        expected [:html
                  [:div "Tipping Points 2014-01-01"
                   [:a {:href "http://example.com/model-predictions.csv"} "Click here"]]]]

    (is (= (template source forms) expected)
        "replace-vars failed")))

;;Later:
#_
(deftest any-node-children-replace-vars-test
  "selector will select only immediate children of :html"
  (let [source [:html
                [:div "Tipping Points ${report-date}"
                 [:a {:href "${model-predictions}"} "Click here"]]]

        vars {:report-date "2014-01-01"
              :model-predictions "http://example.com/model-predictions.csv"}

        forms [[:html :> any-node] (replace-vars vars)]

        expected [:html
                  [:div "Tipping Points 2014-01-01"
                   [:a {:href "${model-predictions}"} "Click here"]]]]

    (is (= (template source forms) expected)
        "children replace-vars failed")))




