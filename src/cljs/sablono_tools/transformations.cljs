(ns sablono-tools.transformations
  (:require [sablono-tools.node-accessors :as node]
            [sablono.util :as util]))


;; A transformation is a visitor function of the node
;; that returns a map with a :node property representing the
;; replacement value for the current node.

(defn visitor
  [f]
  (fn [& args]
    (fn [node]
      {:node ((apply f args) node)})))

(defn set-attr'
  "Assocs attributes on the node."
  [& kvs]
  (fn [node]
    (let [attrs (node/attrs node)
          new-attrs (apply assoc attrs kvs)]
      (vec (concat [(node/tag node) new-attrs] (node/body node))))))

(def set-attr (visitor set-attr'))


(defn remove-attr'
  [& attr-names]
  (fn [node]
    (let [attrs (node/attrs node)
          new-attrs (apply dissoc attrs attr-names)]
      (vec (concat [(node/tag node) new-attrs] (node/body node))))))

(def remove-attr (visitor remove-attr'))


(defn add-class'
  [& classes]
  (fn [node]
    (let [attrs (node/attrs node)
          orig-classes (set (clojure.string/split " " (:class attrs)))
          new-classes (apply conj orig-classes classes)
          new-attrs (merge attrs {:class (util/join-classes new-classes)})]
      (vec (concat [(node/tag node) new-attrs] (node/body node))))))

(def add-class (visitor add-class'))


(defn content'
  "Replaces the content of the node."
  [& values]
  (fn [node]
    (vec (concat [(node/tag node) (node/attrs node)] values))))

(def content (visitor content'))


;; REMINDER: this function uses the JS string.replace:
(defn replace-vars'
  "Replace text strings with properties from the map m.
  If m has a property :something, then all occurrences of ${something}
  in the target will be replaced with m's value of :something.
  If the node is a text node, then replacements occur directly in it.
  Otherwise replacements are done in the node's attrs.
  "
  [m]
    (let [re #"\$\{\s*([^}]*[^\s}])\s*}"
          replacement (fn [match p1]
                        (m (keyword p1)))
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

(def replace-vars (visitor replace-vars'))
