(ns advent-of-code.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))
(def input
  (->> (io/resource "day15-input.txt")
       slurp
       (cs/split-lines)
       (mapv #(into [] %))))

(defn display-map [grid items]
  (->> (reduce (fn [m {:keys [x y val]}] (assoc-in m [y x] val)) grid items)
       (keep (fn [line] (println (cs/join line))))
       seq))

(defn extract-chars [grid c]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= c (get-in grid [y x]))]
    {:x x :y y :val c}))

(defn order-units [units]
  (sort-by (juxt :y :x) units))

(defn possible-liberties [{:keys [x y val] :as unit}]
  (let [liberty (update unit :val #(char (+ 32 (int %))))]
    [(update liberty :x inc)
     (update liberty :x dec)
     (update liberty :y inc)
     (update liberty :y dec)]))

(defn determine-liberties [grid {:keys [x y val] :as unit}]
  (let [liberties possible-liberties unit]
    (filter (fn [{:keys [x y]}] (#{\.} (get-in grid [y x]))) liberties )))

(defn intialize-state [input]
  (let [goblins (extract-chars input \G)
        elves (extract-chars input \E)
        all-units (concat goblins elves)
        blank-grid (reduce (fn [g {:keys [x y val]}] (assoc-in g [y x] \.)) input all-units)]
    {:goblins   goblins
     :elves     elves
     :all-units (map (fn [unit] (assoc unit :liberties (determine-liberties blank-grid unit))) all-units)
     :grid      (reduce (fn [g {:keys [x y val]}] (assoc-in g [y x] \.)) input all-units)}))

;todo change to running a turn
(defn turn [unit {:keys [all-units goblins elves grid] :as state}])

;todo reduce state through turns
(defn round [{:keys [all-units goblins elves grid] :as state}]
  (if (or (empty? goblins)
          (empty? elves))
    (assoc state :done? true)
    (let [all-units (map (fn [unit] (assoc unit :liberties (determine-liberties grid grid))) (order-units all-units))]
      (assoc state :all-units all-units)
      )))

(comment
  (display-map input [])
  (extract-chars input \E)
  (extract-chars input \G)


  (->> (intialize-state input)
       step

       ((fn [{:keys [liberties all-units grid]}]
          (display-map grid (concat liberties all-units))))
       )

  )