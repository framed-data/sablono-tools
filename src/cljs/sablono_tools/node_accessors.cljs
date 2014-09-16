(ns sablono-tools.node-accessors)


(defn tag
  "Get the tag of a node."
  [node]
  (node 0))


(defn tag=
  [node tag]
  (and (sequential? node)
       (first node)
       (= (first node) tag)))

(defn has-attrs?
  [node]
  (and (sequential? node)
       (second node)
       (map? (second node))))


(defn attrs
  "Get the attrs of a node."
  [node]
  (if (has-attrs? node)
    (second node)
    {}))

(defn attr?
  "Does the node have the specified attr?"
  [kw]
  (fn [node]
    (let [attrs (attrs node)]
      (get attrs kw))))


(defn id=
  [node id]
  (let [attrs (attrs node)]
    (and (some? attrs)
         (= (:id attrs) id))))

(defn body
  [node]
  (if (has-attrs? node)
      (rest (rest node))
      (rest node)))
