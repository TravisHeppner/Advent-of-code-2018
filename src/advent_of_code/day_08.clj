(ns advent-of-code.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def input
  (->> (io/resource "day08-input.txt")
       slurp
       (#(str "[" % "]"))
       read-string))

(def s (atom input))
(def tree
  ((fn assemble-tree []
     (let [[n m] @s]
       (when (and n m)
         (swap! s #(drop 2 %))
         (let [children (doall (mapv (fn [i] (do (assemble-tree))) (range n)))
               meta (take m @s)]
           (swap! s #(drop m %))
           {:children children
            :meta     meta}))))))

(defn get-node-meta
  [node]
  (+ (reduce + (:meta node))
     (reduce + (map get-node-meta (:children node)))))

(def part-1 (get-node-meta tree))


(defn get-node-val
  [{:keys [children meta] :as node}]
  (if (empty? children)
    (reduce + meta)
    (->> (keep #(get children (dec %)) meta)
         (map get-node-val)
         (reduce +))))

(def part-2 (get-node-val tree))