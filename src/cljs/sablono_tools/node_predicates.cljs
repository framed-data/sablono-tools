(ns sablono-tools.node-predicates
  (:require [sablono-tools.node-accessors :as a]))

;; Node Predicates ;;;;;;;;;;;;
;;
;; A node predicate is a Boolean function of a node.
;;

(def any-node (constantly true))


(defn tag-matcher
  [tag]
  (fn [node]
    (a/tag= node tag)))


(defn id-matcher
  [id]
  (fn [node]
    (a/id= node id)))


(defn conjunction
  "AND the predicates together. We can't map 'and' itself because it is a macro."
  [& preds]
   (fn [node]
    (reduce (fn [acc pred] (and acc (pred node)))
            true
            preds)))

(defn disjunction
  [& preds]
  (fn [node]
    (reduce (fn [acc pred] (or acc (pred node)))
            false
            preds)))
;;
;; Node Predicates ;;;;;;;;;;;;

(defn step->node-pred
  "Convert step into a single predicate Node -> Bool.
  A step may be:
  an explicit node predicate function,
  a keyword such as :div (matching nodes with that tag),
  a keyword whose name begins with a # such as :#foo
  (matching nodes with the id 'foo'),
  a vector or a set of any of the above
  (representing the conjunction or disjunction of the enclosed predicates),
  or the special form :> (representing direct children of the preceding step)."
  [step]
  (cond
   (vector? step) (apply conjunction (map step->node-pred step))
   (set? step) (apply disjunction (map step->node-pred step))
   ;; a keyword is also an IFn so check keyword? first:
   (keyword? step) (let [name (name step)]
                         (if-let [v (re-find #"\#(.+)" name)] ; does tag start with a #
                           ; yes: id matcher
                           (let [id (second v)]
                             (id-matcher id))
                           ; no: tag matcher or special form
                           (let [tag step]
                             (if (= :> tag)
                               tag
                               (tag-matcher tag)))))
   (ifn? step) step
   :else step ;; error
   ))
