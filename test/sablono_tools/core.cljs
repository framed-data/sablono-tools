(ns sablono-tools.core
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testing test-var)])
  (:require [cemerick.cljs.test :as t] ;; cemerick.cljs.test must be required even if not explicitly used
            [clojure.zip :as zip]
            [sablono-tools.transforms :refer [id-matcher tag-matcher conjunction disjunction
                                              attr? replace-vars content any-node template]]))


(deftest conjunction-test
  (let [pred1 (id-matcher "that")
        pred2 (tag-matcher :p)
        c (conjunction pred1 pred2)
        node1 [:p {:id "that"}]
        node2 [:p]
        node3 [:div {:id "that"}]]
    (is (= (c node1) true))
    (is (= (c node2) false))
    (is (= (c node3) false))))

(deftest disjunction-test
  (let [pred1 (id-matcher "that")
        pred2 (tag-matcher :p)
        d (disjunction pred1 pred2)
        node1 [:p {:id "that"}]
        node2 [:p]
        node3 [:div {:id "that"}]
        node4 [:div {:id "what"}]]
    (is (= (d node1) true))
    (is (= (d node2) true))
    (is (= (d node3) true))
    (is (= (d node4) false))))


;; We don't require an explicit {} for a node's empty attributes on input
;; but we do insert it on output.


(deftest p-test
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[:p] (content "Hello")]

        expected [:div {}
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "p-test failed")))


(deftest fn-test
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[(attr? :lang)] (content "Hello")]

        expected [:div {}
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "fn-test failed")))


(deftest and-test
  "One step is a vector containing :p and (attr? lang) so they are anded.
  This will match any element that is a :p AND has a 'lang' attribute.
  "
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[[:p (attr? :lang)]] (content "Hello")]

        expected [:div {}
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "and-test failed")))


(deftest or-test
  "One step is a set containing :p and (attr? lang) so they are ored.
  This will match any element that is a :p OR has a 'lang' attribute.
  "
  (let [source [:div
                [:p {:lang "EN"}]
                [:section {:lang "FR"}]
                [:p "Me too"]
                [:section {:no-lang "Not Me"}]]

        forms [[#{:p (attr? :lang)}] (content "Hello")]

        expected [:div {}
                  [:p {:lang "EN"} "Hello"]
                  [:section {:lang "FR"} "Hello"]
                  [:p {} "Hello"]
                  [:section {:no-lang "Not Me"}]]]
    (is (= (template source forms) expected)
        "or-test failed")))


(deftest tag-and-content-test
  (let [source [:div
                [:p "Replace me"]
                [:p "Me Too"]]

        forms [[:p] (content "New Text")]

        expected [:div {}
                  [:p {} "New Text"]
                  [:p {} "New Text"]]]

    (is (= (template source forms) expected)
        "tag-and-content replacement failed")))


(deftest id-and-content-test
  (let [source [:html
                [:div {:id "noonie"} "Replace Me}"]
                [:div {:id "nunie"} "Not Me"]]

        forms [[:#noonie] (content "foo")]

        expected [:html {}
                  [:div {:id "noonie"} "foo"]
                  [:div {:id "nunie"} "Not Me"]]]

    (is (= (template source forms) expected)
        "id-and-content replacement failed")))


(deftest any-node-replace-vars-test
  "selector will select any node"
  (let [source [:div
                [:div "Tipping Points ${report-date}"
                 [:a {:href "${model-predictions}"} "Click here"]]]

        vars {:report-date "2014-01-01"
              :model-predictions "http://example.com/model-predictions.csv"}

        forms [[any-node] (replace-vars vars)]

        expected [:div {}
                  [:div {} "Tipping Points 2014-01-01"
                   [:a {:href "http://example.com/model-predictions.csv"} "Click here"]]]]

    (is (= (template source forms) expected)
        "replace-vars failed")))



(deftest descendant-test
  ":p and (attr? lang) appear as separate steps so they are chained.
  This will match any element with a 'lang' attribute
  inside a :p element."
  (let [source [:div
                [:p {:id "french"}
                 [:a {:href "http://link.com" :lang "FR"} "Clique-bête"]]]

        forms [[:p (attr? :lang)] (content "SVP!")]

        expected [:div {}
                  [:p {:id "french"}
                   [:a {:href "http://link.com" :lang "FR"} "SVP!"]]]]
    (is (= (template source forms) expected)
        "descendant-test failed")))


(deftest any-node-children-replace-vars-test
  "selector will select only immediate children of a :p"
  (let [source [:div
                [:p "Tipping Points ${report-date}"
                 [:a {:href "${model-predictions}"} "Click here"]]]

        vars {:report-date "2014-01-01"
              :model-predictions "http://example.com/model-predictions.csv"}

        forms [[:p :> any-node] (replace-vars vars)]

        ;; Both the text node and the :a node are direct children of a :p node
        expected [:div {}
                  [:p {} "Tipping Points 2014-01-01"
                   [:a {:href "http://example.com/model-predictions.csv"} "Click here"]]]]

    (is (= (template source forms) expected)
        "children replace-vars failed")))




