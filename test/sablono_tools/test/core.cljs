(ns sablono-tools.test.core
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testing test-var)])
  (:require [cemerick.cljs.test :as t] ;; cemerick.cljs.test must be required even if not explicitly used
            [sablono-tools.node-accessors :refer [attr? attr=]]
            [sablono-tools.node-predicates :refer [any-node id-matcher tag-matcher
                                                   conjunction disjunction]]
            [sablono-tools.transformations :refer [set-attr remove-attr
                                                   add-class remove-class
                                                   replace-vars content]]
            [sablono-tools.core :refer [template]]))


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


(deftest p-test
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[:p] (content "Hello")]

        expected [:div
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "p-test failed")))


(deftest fn-predicate-test
  (let [source [:div
                [:p {:lang "EN"}]]

        forms [[(attr? :lang)] (content "Hello")]

        expected [:div
                  [:p {:lang "EN"} "Hello"]]]
    (is (= (template source forms) expected)
        "fn-predicate-test failed")))


(deftest and-test
  "One step is a vector containing :p and (attr? lang) so they are anded.
  This will match any element that is a :p AND has a 'lang' attribute.
  "
  (let [source [:div
                [:p {:lang "EN"}]
                [:section {:lang "EN"} "Not a :p!"]
                [:p {:no-lang "EN"} "I have no lang!"]]

        forms [[[:p (attr? :lang)]] (content "Hello")]

        expected [:div
                  [:p {:lang "EN"} "Hello"]
                  [:section {:lang "EN"} "Not a :p!"]
                  [:p {:no-lang "EN"} "I have no lang!"]]]
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

        expected [:div
                  [:p {:lang "EN"} "Hello"]
                  [:section {:lang "FR"} "Hello"]
                  [:p "Hello"]
                  [:section {:no-lang "Not Me"}]]]
    (is (= (template source forms) expected)
        "or-test failed")))


(deftest set-attr-test
  (let [source [:div {:level "beginner"}]

        forms [[any-node] (set-attr :level "advanced" :locale "zh-TW")]

        expected [:div {:level "advanced" :locale "zh-TW"}]]

    (is (= (template source forms) expected)
        "set-attr-test failed")))


(deftest remove-attr-test
  (let [source [:div {:status "pending"}
                [:p {:status "approved"}]]

        forms [[(attr= :status "approved")] (remove-attr :status)]

        expected [:div {:status "pending"}
                  [:p]]]

    (is (= (template source forms) expected)
        "remove-attr-test failed")))


(deftest add-and-remove-class-test
  (let [source [:section {:class "a b"}
                [:div {:class "a"}]]

        forms [[:section] (add-class "c" "d" "b")
               [:div] (add-class "f")
               [any-node] (remove-class "a")]

        expected [:section {:class "b c d"}
                  [:div {:class "f"}]]]

    (is (= (template source forms) expected)
        "add-and-remove-class-test failed")))


(deftest tag-and-content-test
  (let [source [:div
                [:p "Replace me"]
                [:p "Me Too"]]

        forms [[:p] (content "New Text")]

        expected [:div
                  [:p "New Text"]
                  [:p "New Text"]]]

    (is (= (template source forms) expected)
        "tag-and-content replacement failed")))


(deftest id-and-content-test
  (let [source [:html
                [:div {:id "noonie"} "Replace Me"]
                [:div {:id "nunie"} "Not Me"]]

        forms [[:#noonie] (content "Is That You?")]

        expected [:html
                  [:div {:id "noonie"} "Is That You?"]
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

        expected [:div
                  [:div "Tipping Points 2014-01-01"
                   [:a {:href "http://example.com/model-predictions.csv"} "Click here"]]]]

    (is (= (template source forms) expected)
        "replace-vars failed")))



(deftest descendant-test
  ":p and (attr? lang) appear as separate steps so they are chained.
  This will match any element with a 'lang' attribute
  inside a :p element."
  (let [source [:div
                [:p {:id "french"}
                 [:a {:href "http://link.com" :lang "FR"} "Clique-bête"]]
                [:section {:lang "EN"} "I'm not inside a :p!"]]

        forms [[:p (attr? :lang)] (content "Merci!")]

        expected [:div
                  [:p {:id "french"}
                   [:a {:href "http://link.com" :lang "FR"} "Merci!"]]
                  [:section {:lang "EN"} "I'm not inside a :p!"]]]
    (is (= (template source forms) expected)
        "descendant-test failed")))


(deftest any-node-children-replace-vars-test
  "The chain with :> will select only immediate children of a :p"
  (let [source [:div
                [:p "Tipping Points ${report-date}"
                 [:a {:href "${model-predictions}"} "Click here"]
                 [:ol
                  [:li "${report-date} unchanged; I'm not an immediate child"]]]]

        vars {:report-date "2014-01-01"
              :model-predictions "http://example.com/model-predictions.csv"}

        forms [[:p :> any-node] (replace-vars vars)]

        ;; Both the text node and the :a node are direct children of a :p node
        expected [:div
                  [:p "Tipping Points 2014-01-01"
                   [:a {:href "http://example.com/model-predictions.csv"} "Click here"]
                   [:ol
                    [:li "${report-date} unchanged; I'm not an immediate child"]]]]]

    (is (= (template source forms) expected)
        "children replace-vars failed")))


(deftest multi-test
  "Test with more than one selector-transformer pair.
  (The nested lists in the output will be flattened by the html macro.)"
  (let [source [:section
                [:header
                 [:h3
                  [:i {:class "fa fa-table"}] "Tipping Points ${report-date}"]]
                [:table
                 [:thead
                  [:tr {:id "feature-summary-head"}
                   [:th "Feature"]
                   [:th {:col-span 5} "Non-retained Percentiles"]
                   [:th {:col-span 5} "Retained Percentiles"]]
                  [:tr
                   [:th ]
                   [:th "Min"]
                   [:th "25"]
                   [:th "50"]
                   [:th "75"]
                   [:th "Max"]
                   [:th "Min"]
                   [:th "25"]
                   [:th "50"]
                   [:th "75"]
                   [:th "Max"]]]
                 [:tbody {:id "feature-summary-body"}]]]

        vars {:n-days 7
              :report-date "2014-01-01"
              :report-id 12345
              :model-predictions "http://example.com/model-predictions.csv"
              :feature-ranking "http://example.com/feature-ranking.csv"
              :feature-summary-body [[1 2 3 4 5 6 7 8 9 10]
                                     [10 20 30 40 50 60 70 80 90 100]]}

        forms [[:section any-node] (replace-vars vars)
               [:#feature-summary-body] (let [td (fn [content] [:td content])
                                              row (fn [cols] [:tr (map td cols)])]
                                          (content (map row (:feature-summary-body vars))))]

        expected [:section
                  [:header
                   [:h3
                    [:i {:class "fa fa-table"}] "Tipping Points 2014-01-01"]]
                  [:table
                   [:thead
                    [:tr {:id "feature-summary-head"}
                     [:th "Feature"]
                     [:th {:col-span 5} "Non-retained Percentiles"]
                     [:th {:col-span 5} "Retained Percentiles"]]
                    [:tr
                     [:th]
                     [:th "Min"]
                     [:th "25"]
                     [:th "50"]
                     [:th "75"]
                     [:th "Max"]
                     [:th "Min"]
                     [:th "25"]
                     [:th "50"]
                     [:th "75"]
                     [:th "Max"]]]
                   [:tbody {:id "feature-summary-body"}
                    '([:tr
                       ([:td 1]
                        [:td 2]
                        [:td 3]
                        [:td 4]
                        [:td 5]
                        [:td 6]
                        [:td 7]
                        [:td 8]
                        [:td 9]
                        [:td 10])]
                      [:tr
                       ([:td 10]
                        [:td 20]
                        [:td 30]
                        [:td 40]
                        [:td 50]
                        [:td 60]
                        [:td 70]
                        [:td 80]
                        [:td 90]
                        [:td 100])])]]]]

    (is (= (template source forms) expected)
        "multi-test failed")))




