(ns sablono-tools.transformations
  (:require [sablono-tools.node-accessors :as node]
            [sablono.util :as util]))


;; A transformation is a visitor function of a node
;; that returns a map with a :node property representing the
;; replacement value for the node.


(defn- make-node
  [tag attrs body]
  ;; Omit attrs if it is empty or nil:
  (if (and (some? attrs) (seq attrs))
    (vec (concat [tag attrs] body))
    (vec (concat [tag] body))))

(defn- visitor
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
      (make-node (node/tag node) new-attrs (node/body node)))))

(def set-attr (visitor set-attr'))


(defn remove-attr'
  [& attr-names]
  (fn [node]
    (let [attrs (node/attrs node)
          new-attrs (apply dissoc attrs attr-names)]
      (make-node (node/tag node) new-attrs (node/body node)))))

(def remove-attr (visitor remove-attr'))


(defn modify-classes
  [add-or-remove & classes]
  (let [f (condp = add-or-remove
            :add conj
            :remove disj)]
    (fn [node]
      (let [attrs (node/attrs node)
            orig-classes (set (clojure.string/split " " (:class attrs)))
            new-classes (apply f orig-classes classes)
            new-attrs (merge attrs {:class (util/join-classes new-classes)})]
        (make-node (node/tag node) new-attrs (node/body node))))))

(defn add-class'
  [& classes]
  (modify-classes :add classes))

(def add-class (visitor add-class'))


(defn remove-class'
  [& classes]
  (modify-classes :remove classes))

(def remove-class (visitor remove-class'))


(defn content'
  "Replaces the content of the node."
  [& values]
  (fn [node]
    (make-node (node/tag node) (node/attrs node) values)))

(def content (visitor content'))


;; REMINDER: this function uses the JS string.replace:
(defn replace-vars'
  "Replace text strings with properties from the map m.
  If m has a property :something, then all occurrences of ${something}
  in the target will be replaced with m's value of :something.
  If the node is a text node, then replacements occur directly in the text.
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
