(ns sablono-tools.core
  (:require [sablono-tools.zip :as zip]
            [sablono-tools.node-predicates :as node]))


(defn transform-nodes
  [template steps f]
  (let [zipper (zip/node-zip template)]

    (if (= 1 (count steps))
      ;; convert the single step to a node visitor;
      ;; pass it and the transformer f to a tree-visitor:
      (zip/tree-visitor zipper (vec (concat (map zip/step->node-visitor steps) [f])))

      ;; otherwise steps is a chain, a sequence of selector steps that
      ;; depends on relationships among nodes. Convert it to a loc predicate,
      ;; and pass it and the transformer to a loc-visitor:
      (let [loc-pred (zip/chain->loc-pred steps)]
        (zip/loc-visitor zipper loc-pred f)))))


(defn parse-pair
  [steps f]
  #(transform-nodes % (mapv node/step->node-pred steps) f))


(defn template
  [source-template forms]
  (let [fns (map #(apply parse-pair %) (partition 2 forms))
        context (reduce (fn [acc f] (:node (f acc))) source-template fns)]
    context))




