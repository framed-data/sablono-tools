(ns sablono-tools.transforms
  (:require [clojure.walk :as walk]))

;; based on enlive-html but using the sablono template format rather than html:
;; [tag attrs? content*]

(def any-node (constantly true))

(defn tag=
  [elt tag]
  (and (sequential? elt)
       (first elt)
       (= (first elt) tag)))

(defn attrs
  [elt]
  (when (and (sequential? elt)
             (second elt)
             (map? (second elt)))
    (second elt)))

(defn id=
  [elt id]
  (let [attrs (attrs elt)]
    (and attrs
         (= (:id attrs) id))))


(defn content
  "Replaces the content of the element."
  [& values]
  (fn [elt]
    (let [tag (elt 0)
          attrs (attrs elt)]
      (vec (concat [tag attrs] values)))))


;; REMINDER: this function uses the JS string.replace:
(defn replace-vars
  "Replace text strings with properties from the map m.
  If m has a property :something, then all occurrences of ${something}
  in the target will be replaced with the value of :something.
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


(defn transform-nodes
  [template pred f]
  (walk/prewalk #(if (pred %)
                   (f %)
                   %)
                template))

(defn transform-all-nodes
  [template f]
  (transform-nodes template (constantly true) f))

(defn transform-nodes-with-tag
  [template tag f]
  (transform-nodes template (fn [node] (tag= node tag)) f))

(defn transform-node-with-id
  [template id f]
  (transform-nodes template (fn [node] (id= node id)) f))


(defn parse-pair
  [[selector] f] ;; TODO there may be more than one selector
  (let [tag (name selector)] ;; assuming selector is a keyword
    (if-let [v (re-find #"\#(.+)" tag)] ;; id matcher; tag starts with a #
      (let [id (second v)]
          #(transform-node-with-id % id f))
      (let [tag selector]
        (if (= tag :html) ;; we'll interpret this as a universal selector, thus making 'any-node' redundant
          #(transform-all-nodes % f)
          #(transform-nodes-with-tag % tag f))))))


(defn template
  [source-template forms]
  (let [fns (map #(apply parse-pair %) (partition 2 forms))]
    (reduce (fn [acc f] (f acc)) source-template fns)))




