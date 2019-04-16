(ns advent-of-code.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))


(def input
  (->> (io/resource "day06-input.txt")
       slurp
       cs/split-lines
       (map (fn [s] (->> (re-find #"(\d+), (\d+)" s)
                         rest
                         (mapv #(Integer/parseInt %)))))))

(defn dist [[x1 y1] [x2 y2]]
  (+
    (Math/abs (- x1 x2))
    (Math/abs (- y1 y2))))

(defn closest [coord]
  (->> input
       (group-by (partial dist coord))
       (sort-by first)
       first
       second))

(defn get-bounds [coords]
  {:max-x (apply max (map first input))
   :min-x (apply min (map first input))
   :max-y (apply max (map second input))
   :min-y (apply min (map second input)) })

(def infinite-area-coords
  (let [{:keys [max-x min-x max-y min-y]} (get-bounds input)]
    (->> (distinct
           (concat
             (map #(vector % (dec min-y)) (range (dec min-x) (inc max-x)))
             (map #(vector % (inc max-y)) (range (dec min-x) (inc max-x)))
             (map #(vector (dec min-x) %) (range (dec min-y) (inc max-y)))
             (map #(vector (inc max-x) %) (range (dec min-y) (inc max-y)))))
         (map closest)
         (filter (comp zero? dec count))
         (mapcat identity)
         distinct
         (into #{}))))

(def part-1
  (let [{:keys [max-x min-x max-y min-y]} (get-bounds input)]
    [min-x min-y max-x max-y]
    (->> (for [x (range min-x (inc max-x))
               y (range min-y (inc max-y))]
           [x y])
         (map closest)
         (filter (comp zero? dec count))
         (mapcat identity)
         (remove infinite-area-coords)
         (group-by identity)
         (map (fn [[coord copies]] [coord (count copies)]))
         (sort-by second)
         last
         second)))

(def part-2
  (let [{:keys [max-x min-x max-y min-y]} (get-bounds input)
        x-dists (map (fn [x] (reduce #(+ %1 (Math/abs (- x %2))) 0 (map first  input))) (range min-x max-x))
        y-dists (map (fn [y] (reduce #(+ %1 (Math/abs (- y %2))) 0 (map second input))) (range min-y max-y))]
    (count (for [xd x-dists
                 yd y-dists
                 :when (> 10000 (+ xd yd))]
             true))))