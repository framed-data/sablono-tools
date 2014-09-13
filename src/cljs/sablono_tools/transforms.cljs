(ns sablono-tools.transforms
  (:refer-clojure :exclude [descendants ancestors])
  (:require [clojure.zip :as zip]))

;; based on enlive-html but using the sablono template format rather than html:
;; [tag attrs? content*]


;; Here are three functions stolen from clojure.data.zip:
;;
(defn right-locs
  "Returns a lazy sequence of locations to the right of loc, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (right-locs (zip/right loc))))))

(defn children
  "Returns a lazy sequence of all immediate children of location loc,
  left-to-right."
  [loc]
  (println "children: loc:" (zip/node loc))
  (when (zip/branch? loc)
    (let [val (right-locs (zip/down loc))]
      (println "children:" (map zip/node val))
      val)))

(defn descendants
  "Returns a lazy sequence of all descendants of location loc, in
  depth-first order, left-to-right, starting with loc. Yes, INCLUDING loc.
  That's needed to make the recursion work. You can trim it off afterward
  if you want."
  [loc]
  (lazy-seq (cons loc (mapcat descendants (children loc)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














;; Node accessors ;;;;;;;;
;;
(defn tag
  "Get the tag of a node."
  [node]
  (node 0))


(defn tag=
  [node tag]
  (and (sequential? node)
       (first node)
       (= (first node) tag)))


(defn attrs
  "Get the attrs of a node."
  [node]
  (when (and (sequential? node)
             (second node)
             (map? (second node)))
    (second node)))

(defn attr?
  "Does the node have the specified attr?"
  [kw]
  (fn [node]
    (let [attrs (attrs node)]
      (and attrs
           (get attrs kw)))))


(defn id=
  [node id]
  (let [attrs (attrs node)]
    (and (some? attrs)
         (= (:id attrs) id))))
;;
;; Node accessors ;;;;;;;;

#_
(defn do->
"Chains (composes) several transformations. Applies functions from left to right."
[& fns]
#(reduce (fn [nodes f] (flatmap f nodes)) (as-nodes %) fns))



;; Predicates ;;;;;;;;;;;;
;;
;; A predicate is a Boolean function of a node.
;;

(def any-node (constantly true))


(defn tag-matcher
  [tag]
  (fn [node]
    (tag= node tag)))


(defn id-matcher
  [id]
  (fn [node]
    (id= node id)))


(defn conjunction
  "AND the predicates together. We can't map 'and' itself because it is a macro."
  [& preds]
   (fn [node]
    (reduce (fn [acc pred] (and acc (pred node)))
            true
            preds)))
;;
;; Predicates ;;;;;;;;;;;;



;; Transformations ;;;;;;;
;;
;; A transformation is a visitor that returns a map with a
;; :node property representing the replacement value for the current node.
;;


(defn content-bare
  [& values]
  (fn [node]
    (vec (concat [(tag node) (attrs node)] values))))


(defn content
  "Replaces the content of the node."
  [& values]
  (fn [node state]
    {:node ((apply content-bare values) node)}))


;; REMINDER: this function uses the JS string.replace:
#_
(defn replace-vars
  "Replace text strings with properties from the map m.
  If m has a property :something, then all occurrences of ${something}
  in the target will be replaced with m's value of :something.
  If the node is a text node, then replacements occur directly in it.
  Otherwise replacements are done in the node's attrs.
  "
  ([m] (replace-vars #"\$\{\s*([^}]*[^\s}])\s*}" m))
  ([re m] (replace-vars re m keyword))
  ([re m f]
    (let [replacement (fn [match p1]
                        (m (f p1)))
          substitute-vars (fn [x]
                            (cond
                             (string? x) (.replace x re replacement)
                             :else x))]
      (fn [node state]
        (cond
         (string? node) {:node (substitute-vars node)}
         (vector? node) {:node (let [[tag attrs & rest] node]
                                 (if (map? attrs)
                                   (assoc node 1
                                     (into {} (for [[k v] attrs]
                                                [k (substitute-vars v)])))
                            node))}
         :else {:node node})))))


(defn replace-vars-bare
  "Replace text strings with properties from the map m.
  If m has a property :something, then all occurrences of ${something}
  in the target will be replaced with m's value of :something.
  If the node is a text node, then replacements occur directly in it.
  Otherwise replacements are done in the node's attrs.
  "
  ([m] (replace-vars-bare #"\$\{\s*([^}]*[^\s}])\s*}" m))
  ([re m] (replace-vars-bare re m keyword))
  ([re m f]
    (let [replacement (fn [match p1]
                        (m (f p1)))
          substitute-vars (fn [x]
                            (cond
                             (string? x) (.replace x re replacement)
                             :else x))]
      (fn [node]
        (cond
         (string? node) (substitute-vars node)
         (vector? node) (let [[tag attrs & rest] node]
                                 (if (map? attrs)
                                   (assoc node 1
                                     (into {} (for [[k v] attrs]
                                                [k (substitute-vars v)])))
                            node))
         :else node)))))

(defn replace-vars
  ([m] (replace-vars #"\$\{\s*([^}]*[^\s}])\s*}" m))
  ([re m] (replace-vars re m keyword))
  ([re m f]
   (fn [node state]
     {:node ((replace-vars-bare re m f) node)})))
;;
;; Transformations ;;;;;;;



(defn step->pred
  "Convert step into a single predicate Node -> Bool.
  A step may be:
  an explicit predicate function,
  a keyword such as :div (matching nodes with that tag),
  a keyword whose name begins with a # such as :#foo
  (matching nodes with the id 'foo'),
  or a vector of any of the above
  (representing the conjunction of the enclosed predicates)."
  [step]
  (cond
   (vector? step) (apply conjunction (map step->pred step))
   ;; a keyword is also an IFn so check keyword? first:
   (keyword? step) (let [name (name step)]
                         (if-let [v (re-find #"\#(.+)" name)] ; does tag start with a #
                           (let [id (second v)] ; yes: id matcher
                             (id-matcher id))
                           (let [tag step] ; no: tag matcher
                             (tag-matcher tag))))
   (ifn? step) step
   :else step ;; error
   ))


(defn step->node-visitor
  "Convert step into a visitor that returns a map with
  :next (skip succeeding visitors on the current node) set to true
  if the predicate is not satisfied."
  [step]
  (let [f (step->pred step)]
    (fn [node state]
      {:next (not (f node))})))




(defn node-zip
  "A zipper that regards only vectors (ordinary nodes) and strings
  (text nodes) as nodes, not maps (attrs of nodes)."
  [root]
  (zip/zipper vector?
              #(filter (fn [x] (or (vector? x) (string? x))) (seq %))
              (fn [node children]
                (with-meta
                (let [f (apply content children)]
                  (get (f node true) :node))
                  (meta node)))
              root))





;; Visitors copped from Alex Miller's article:
;; http://www.ibm.com/developerworks/library/j-treevisit/index.html

;; Tree traversal ;;;;;;;;
;;
;; A visitor is a function of [node state] that returns a map which may contain:
;; :node replacing the current :node
;; :state replacing the current :state
;; :next indicating an instruction to skip the rest of the visitors at this :node
;; :stop indicating an instruction to stop processing the tree.
;;




(defn visit-node
  [start-node start-state visitors]
  (println "Visiting:" start-node)
  (loop [node start-node
         state start-state
         [first-visitor & rest-visitors] visitors]
    (println "node:" node)
    (println "state:" state)
    (println "first-visitor:" first-visitor)
    (let [new-context (first-visitor node state)
          _ (println "new-context:" new-context)


          context (merge {:node node, :state state, :stop false, :next false}
                         new-context)
          _ (println "context:" context)
          {new-node :node
           new-state :state
           :keys (stop next)} context]
      (println "next:" next)
      (if (or next stop (nil? rest-visitors))
        (let [return-value {:node new-node, :state new-state, :stop stop}]
          (println "returning:" return-value)
          return-value)
        (recur new-node new-state rest-visitors)))))


(defn tree-visitor
  ([zipper visitors]
     (tree-visitor zipper nil visitors))
  ([zipper initial-state visitors]
     (loop [loc zipper
            state initial-state]
       (let [context (visit-node (zip/node loc) state visitors)
             new-node (:node context)
             new-state (:state context)
             stop (:stop context)
             new-loc (if (= new-node (zip/node loc))
                       loc
                       (zip/replace loc new-node))
             next-loc (zip/next new-loc)]
         (if (or (zip/end? next-loc) (= stop true))
           {:node (zip/root new-loc) :state new-state}
           (recur next-loc new-state))))))


(defn pred->loc-pred
  [pred]
  (fn [loc] (pred (zip/node loc))))

(defn zip-filter
  "Return all locs in zipper whose nodes satisfy pred."
  [pred zipper]
  (loop [loc zipper
         locs []]
    (let [new-locs (if (pred (zip/node loc))
                     (conj locs loc)
                     locs)
          next-loc (zip/next loc)]
      (if (zip/end? next-loc)
        new-locs
        (recur next-loc new-locs)))))


(defn chain->
  [zipper preds]
  ;; we're here because (count preds) is more than 1
  (loop [locs [zipper]
         preds preds]
    (let [[first-pred second-pred & rest-preds] preds
          [target-fn next-pred rest-preds] (if (= :> second-pred)
                                             [children (first rest-preds) (rest rest-preds)]
                                             [#(rest (descendants %)) second-pred rest-preds])

          parents (mapcat #(zip-filter first-pred %) locs)
          targets (mapcat target-fn parents)
          filtered (filter (pred->loc-pred next-pred) targets)]
      (println "parents:" (map zip/node parents))
      (println "targets:" (map zip/node targets))
      (println "filtered:" (map zip/node filtered))
      (if (pos? (count rest-preds))
        (recur filtered rest-preds)
        (set filtered)))))


(defn loc-visitor
  [zipper loc-pred f]
  (loop [loc zipper]
    (let [new-loc (if (loc-pred loc)
                    (let [new-node (f (zip/node loc))]
                      (println "new-node:" new-node)
                      (zip/replace loc new-node))
                    loc)
          next-loc (zip/next new-loc)]
      (if (zip/end? next-loc)
        (zip/root new-loc)
        (recur next-loc)))))


(defn transform-nodes
  [template steps f]
  (let [zipper (node-zip template)]
    (if (= 1 (count steps))
      (tree-visitor zipper (vec (concat (map step->node-visitor steps) [f])))
      ;; otherwise preds is a chain
      (let [targets (chain-> zipper steps)
            _ (println "Targets:" targets)
            loc-pred #(some targets %)]
        ;; now we have to run f on targets
        (loc-visitor zipper loc-pred (fn [node] (:node (f node :dummy))))))))



;;
;; Tree traversal ;;;;;;;;







(defn parse-pair
  [steps f]
  #(transform-nodes % (mapv step->pred steps) f))


(defn template ;; compare kioo/common.cljx do->
  [source-template forms]
  (let [fns (map #(apply parse-pair %) (partition 2 forms))
        context (reduce (fn [acc f] (f acc)) source-template fns)]
    (:node context)))




