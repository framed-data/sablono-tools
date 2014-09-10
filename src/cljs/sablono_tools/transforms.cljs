(ns sablono-tools.transforms
  (:require [clojure.zip :as zip]))

;; based on enlive-html but using the sablono template format rather than html:
;; [tag attrs? content*]

;; Node accessors ;;;;
;;
(defn tag
  "Get the tag of a node."
  [node]
  (node 0))

(defn attrs
  "Get the attrs of a node."
  [node]
  (when (and (sequential? node)
             (second node)
             (map? (second node)))
    (second node)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Predicates ;;;;;;;;;;;;
;;
(def any-node (constantly true))


(defn tag=
  [node tag]
  (and (sequential? node)
       (first node)
       (= (first node) tag)))


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


(defn conjunction
  "AND the predicates together. We can't map 'and' itself because it is a macro."
  [preds]
  (fn [node]
    (reduce (fn [acc pred] (and acc (pred node))) true preds)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Transformations ;;;;;;;
;;
(defn content
  "Replaces the content of the node."
  [& values]
  (fn [node]
    (vec (concat [(tag node) (attrs node)] values))))


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


(defn tree-edit
  [zipper matcher editor]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next (zip/edit loc editor)))
        (recur (zip/next loc))))))


(defn transform-nodes
  [template pred f]
  (let [zipper (zip/vector-zip template)]
    (tree-edit zipper pred f)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn parse-selector
  "Convert selector into a predicate function"
  [selector]
  (cond
   (vector? selector) (conjunction (map parse-selector selector))
   ;; a keyword is an IFn so check keyword? first:
   (keyword? selector) (let [name (name selector)]
                         (if-let [v (re-find #"\#(.+)" name)] ;; id matcher; tag starts with a #
                          (let [id (second v)]
                            (fn [node] (id= node id)))
                           (let [tag selector]
                             (fn [node] (tag= node tag)))))
   (ifn? selector) selector
   :else selector ;; error
   ))

(def chain conjunction) ;; This will change

(defn parse-pair
  [selectors f]
  (let [pred (chain (map parse-selector selectors))]
    #(transform-nodes % pred f)))


(defn template
  [source-template forms]
  (let [fns (map #(apply parse-pair %) (partition 2 forms))]
    (reduce (fn [acc f] (f acc)) source-template fns)))




