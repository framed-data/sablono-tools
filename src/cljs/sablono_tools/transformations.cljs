(ns sablono-tools.transformations
  (:require [sablono-tools.node-accessors :as node]
            [sablono.util :as util]))


;; A transformation is a visitor function of the node
;; that returns a map with a :node property representing the
;; replacement value for the current node.


(defn set-attr'
  "Assocs attributes on the node."
  [& kvs]
  (fn [node]
    (let [attrs (node/attrs node)
          new-attrs (apply assoc attrs kvs)]
      (vec (concat [(node/tag node) new-attrs] (node/body node))))))

(defn set-attr
  [& kvs]
  (fn [node]
    {:node ((apply set-attr' kvs) node)}))


(defn remove-attr'
  [& attr-names]
  (fn [node]
    (let [attrs (node/attrs node)
          new-attrs (apply dissoc attrs attr-names)]
      (vec (concat [(node/tag node) new-attrs] (node/body node))))))

(defn remove-attr
  [& attr-names]
  (fn [node]
    {:node ((apply remove-attr' attr-names) node)}))


(defn add-class'
  [& classes]
  (fn [node]
    (let [attrs (node/attrs node)
          orig-classes (set (clojure.string/split " " (:class attrs)))
          new-classes (apply conj orig-classes classes)
          new-attrs (merge attrs {:class (util/join-classes new-classes)})]
      (vec (concat [(node/tag node) new-attrs] (node/body node))))))

(defn add-class
  [& classes]
  (fn [node]
    {:node ((apply add-class' classes) node)}))


(defn content'
  [& values]
  (fn [node]
    (vec (concat [(node/tag node) (node/attrs node)] values))))

(defn content
  "Replaces the content of the node."
  [& values]
  (fn [node]
    {:node ((apply content' values) node)}))


;; REMINDER: this function uses the JS string.replace:
(defn replace-vars'
  "Replace text strings with properties from the map m.
  If m has a property :something, then all occurrences of ${something}
  in the target will be replaced with m's value of :something.
  If the node is a text node, then replacements occur directly in it.
  Otherwise replacements are done in the node's attrs.
  "
  [& [re m f]]
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
         :else node))))

(defn replace-vars
  ([m] (replace-vars #"\$\{\s*([^}]*[^\s}])\s*}" m))
  ([re m] (replace-vars re m keyword))
  ([re m f]
   (fn [node]
     {:node ((replace-vars' re m f) node)})))
