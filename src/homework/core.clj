(ns homework.core)

(defn find-node [predicate tree]
  (if (predicate tree)
    tree
    (if tree
      (->> (map #(find-node predicate %) (:children tree))
           (drop-while not)
           first))))

(defn find-all [predicate tree]
  (let [found (if (predicate tree) #{tree} #{})]
    (->> (map #(find-all predicate %) (:children tree))
         (reduce clojure.set/union #{})
         (clojure.set/union found))))

(defn find-node-custom [predicate get-children tree]
  (let [children (get-children tree)]
    (if (predicate tree)
      tree
      (if tree
        (->> (map #(find-node-custom predicate get-children %) children)
             (drop-while not)
             first)))))

(defn find-node-with-path [path-so-far path-predicate get-children tree]
  (let [so-far (conj path-so-far tree)]
    (if (path-predicate so-far)
      tree
      (if tree
        (->> (map #(find-node-with-path so-far path-predicate get-children %) (get-children tree))
             (drop-while not)
             first)))))

