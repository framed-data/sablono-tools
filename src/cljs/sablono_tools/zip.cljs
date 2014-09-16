(ns sablono-tools.zip
  (:require [clojure.zip :as z]
            [sablono-tools.node-predicates :as pred]
            [sablono-tools.transformations :as tr]))

;; Loc Predicates ;;;;;;;;
;;
;; A loc predicate is a Boolean function of a loc.
;;

(defn parent-node-is?
  "Does loc's parent's node satisfy node-pred?"
  [node-pred]
  (fn [loc]
    (let [parent (z/up loc)]
      (and parent
           (node-pred (z/node parent))))))

(defn ancestor-node-is?
  "Does some ancestor node of loc satisfy node-pred?"
  [node-pred]
  (fn [loc]
    (some node-pred (z/path loc))))

(defn my-node-is?
  "Does loc's own node satisfy node-pred?"
  [node-pred]
  (fn [loc]
    (node-pred (z/node loc))))
;;
;; Loc Predicates ;;;;;;;;


(defn node-zip
  "A zipper that regards only vectors (ordinary nodes) and strings
  (text nodes) as nodes, not maps (attrs of nodes)."
  [root]
  (z/zipper vector?
              #(filter (fn [x] (or (vector? x) (string? x))) (seq %))
              (fn [node children]
                (with-meta
                (let [f (apply tr/content children)]
                  (get (f node true) :node))
                  (meta node)))
              root))


;; Visitors copped from Alex Miller's article:
;; http://www.ibm.com/developerworks/library/j-treevisit/index.html

;; Tree traversal ;;;;;;;;
;;
;; A visitor is a function of node that returns a map which may contain:
;; :node replacing the current :node
;; :next indicating an instruction to skip the rest of the visitors at this :node
;; :stop indicating an instruction to stop processing the tree.
;;
(defn visit-node
  [start-node visitors]
  (loop [node start-node
         [first-visitor & rest-visitors] visitors]
    (let [new-context (first-visitor node)
          context (merge {:node node, :stop false, :next false}
                         new-context)
          {new-node :node
           :keys (stop next)} context]
      (if (or next stop (nil? rest-visitors))
        (let [return-value {:node new-node, :stop stop}]
          return-value)
        (recur new-node rest-visitors)))))


(defn tree-visitor
  ([zipper visitors]
     (loop [loc zipper]
       (let [context (visit-node (z/node loc) visitors)
             new-node (:node context)
             stop (:stop context)
             new-loc (if (= new-node (z/node loc))
                       loc
                       (z/replace loc new-node))
             next-loc (z/next new-loc)]
         (if (or (z/end? next-loc) (= stop true))
           {:node (z/root new-loc)}
           (recur next-loc))))))

  (defn step->node-visitor
  "Convert step into a visitor that returns a map with
  :next (skip succeeding visitors on the current node) set to true
  if the predicate is not satisfied."
  [step]
  (let [f (pred/step->node-pred step)]
    (fn [node]
      {:next (not (f node))})))


(defn chain->loc-pred
  "Convert a chain into a single loc predicate."
  [chain]
  (loop [preds chain
         loc-pred (constantly true)]
    (let [[first-pred second-pred & rest-preds] preds
          [new-pred rest-preds] (condp = second-pred
                                  :> ;=>
                                  [(parent-node-is? (and first-pred loc-pred))
                                   rest-preds]
                                  nil ;=>
                                  [(fn [loc] (and ((my-node-is? first-pred) loc)
                                                  (loc-pred loc)))
                                   nil]
                                  ;; else:
                                  [(ancestor-node-is? (and first-pred loc-pred))
                                   (concat [second-pred] rest-preds)])]
      (if (and (> (count rest-preds) 0)
               (some? (first rest-preds)))
        (recur rest-preds
               new-pred)
        new-pred))))

#_
(defn chain->loc-pred
  "A version of chain->loc-pred that returns plain data structures
  for testing at the repl."
  [chain]
  (loop [preds chain
         loc-pred true]
    (println "preds:" preds "loc-pred:" loc-pred)
    (let [[first-pred second-pred & rest-preds] preds
          _ (println "first-pred:" first-pred)
          _ (println "second-pred:" second-pred)
          _ (println "rest-preds:" rest-preds)
          [new-pred rest-preds] (condp = second-pred
                                  :> ;;:
                                  [[:parent-is? [:and first-pred loc-pred]]
                                   rest-preds]
                                  nil ;;:
                                  [[:and first-pred loc-pred]
                                   nil]
                                  ;; else:
                                  [[:ancestor-is? [:and first-pred loc-pred]]
                                   (concat [second-pred] rest-preds)])]
      (println "new-pred:" new-pred)
      (println "rest-preds:" rest-preds)
      (if (and (> (count rest-preds) 0)
               (some? (first rest-preds)))
        (recur rest-preds
               new-pred)
        new-pred))))


(defn loc-visitor
  "Like tree-visitor but tests a predicate on locs, not nodes.
  Runs the transformer f on the nodes of locs that satisfy the predicate."
  [zipper loc-pred f]
  (loop [loc zipper]
    (let [new-loc (if (loc-pred loc)
                    (let [new-node (:node (f (z/node loc)))]
                      (z/replace loc new-node))
                    loc)
          next-loc (z/next new-loc)]
      (if (z/end? next-loc)
        {:node (z/root new-loc)}
        (recur next-loc)))))
