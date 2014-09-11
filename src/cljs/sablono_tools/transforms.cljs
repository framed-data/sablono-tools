(ns sablono-tools.transforms
  (:require [clojure.zip :as zip]))

;; based on enlive-html but using the sablono template format rather than html:
;; [tag attrs? content*]




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
  (loop [node start-node
         state start-state
         [first-visitor & rest-visitors] visitors]
    (let [new-context (first-visitor node state)
          _ (println "new-context:" new-context)


          context (merge {:node node, :state state, :stop false, :next false}
                         new-context)
          _ (println "context:" context)
          {new-node :node
           new-state :state
           :keys (stop next)} context]
      (if (or next stop (nil? rest-visitors))
        {:node new-node, :state new-state, :stop stop}
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

(declare content)

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

(defn transform-nodes
  [template pred f]
  (let [zipper (node-zip template)]
    (tree-visitor zipper [pred f])))
;;
;; Tree traversal ;;;;;;;;



;; Node accessors ;;;;
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
    (and attrs
         (= (:id attrs) id))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Predicates ;;;;;;;;;;;;
;;
;; A predicate is a visitor that returns a map with a new Boolean
;; :state property indicating whether the rest of the visitors
;; at the current node will be applied or not.
;;

(def any-node (constantly true))


(defn tag-matcher
  [tag]
  (fn [node state]
    {:state (tag= node tag)}))


(defn id-matcher
  [id]
  (fn [node state]
    {:state (id= node id)}))


(defn and-state
  [m1 m2]
  (let [state1 (:state m1)
        state2 (:state m2)
        state (and state1 state2)]
    {:state state}))

(defn conjunction
  "AND the predicates together. We can't map 'and' itself because it is a macro."
  [preds]
  (fn [node state]
    (reduce (fn [acc pred] (and-state acc (pred node state)))
            {:state true}
            preds)))
;;
;; Predicates ;;;;;;;;;;;;



;; Transformations ;;;;;;;
;;
;; A transformation is a visitor that returns a map with a new
;; :node property representing the replacement value for the current node.
;;

(defn content
  "Replaces the content of the node."
  [& values]
  (fn [node state]
    {:node (if state
             (vec (concat [(tag node) (attrs node)] values))
             node)}))


;; REMINDER: this function uses the JS string.replace:
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
         (false? state) {:node node}
         (string? node) {:node (substitute-vars node)}
         (vector? node) {:node (let [[tag attrs & rest] node]
                                 (if (map? attrs)
                                   (assoc node 1
                                     (into {} (for [[k v] attrs]
                                                [k (substitute-vars v)])))
                            node))}
         :else {:node node})))))
;;
;; Transformations ;;;;;;;



(defn parse-selector
  "Convert selector into a predicate function"
  [selector]
  (cond
   (vector? selector) (conjunction (map parse-selector selector))
   ;; a keyword is an IFn so check keyword? first:
   (keyword? selector) (let [name (name selector)]
                         (if-let [v (re-find #"\#(.+)" name)] ; does tag start with a #
                          (let [id (second v)] ; yes: id matcher
                            (id-matcher id))
                           (let [tag selector] ; no: tag matcher
                             (tag-matcher tag))))
   (ifn? selector) (fn [node state] {:state (selector node)})
   (get selector :chain) selector
   :else selector
   ))

(defn chain
  [preds]
  (let [[first-pred second-pred] preds]

    first-pred))


(defn parse-pair
  [selectors f]
  (let [pred (chain (map parse-selector selectors))]
    #(transform-nodes % pred f)))


(defn template
  [source-template forms]
  (let [fns (map #(apply parse-pair %) (partition 2 forms))
        context (reduce (fn [acc f] (f acc)) source-template fns)]
    (:node context)))




