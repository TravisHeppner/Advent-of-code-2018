(ns advent-of-code.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

;givens
(def input
  (->> (io/resource "day03-input.txt")
       slurp
       clojure.string/split-lines
       (map #(->> (re-find #"#(\d+) @ (\d+),(\d+)\: (\d+)x(\d+)" %)
                  rest
                  (map read-string)
                  (zipmap [:id :x :y :w :h])))))

(def min-x 0)
(def min-y 0)
(def max-x 1000)
(def max-y 1000)
(def cloth (into [] (repeat max-y (into [] (repeat max-x [])))))

;transforms

(defn marking [sq-inch mark]
  (conj sq-inch mark))

(defn mark-point [cloth {:keys [x y id]}]
  (update-in cloth [y x] marking id))

(defn bounds->points [{:keys [x y w h id] :as claim}]
  (for [i (range x (+ x w))
        j (range y (+ y h))]
    {:x i :y j :id id}))

(defn mark-cloth [cloth claim]
  (reduce mark-point cloth (bounds->points claim)))

(defn mark-all-claims [cloth claims]
  (reduce mark-cloth cloth claims))

(def part-1
  (->> (mark-all-claims
         cloth
         input)
       (mapcat identity)
       (remove empty?)
       (remove (comp zero? dec count))
       count))

(def part-2
  (->> (mark-all-claims
         cloth
         input)
       (mapcat identity)
       distinct
       (reduce (fn [m cell]
                 (reduce #(update % %2 (comp (partial into #{}) (partial concat cell))) m cell)) {})
       (filter (comp zero? dec count second))
       ffirst
       ))