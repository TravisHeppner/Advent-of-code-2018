(ns advent-of-code.day2
  (:require
    [clojure.java.io :as io]))

(def input
  (->> (io/resource "day2-input.txt")
       slurp
       clojure.string/split-lines))

(def part-1
  (->> input
       (reduce
         (fn [counts s]
           (let [f (->> s
                        frequencies
                        (map second)
                        (into #{}))]
             (cond-> counts
                     (f 2) (update :twos inc)
                     (f 3) (update :threes inc))))
         {:threes 0
          :twos 0})
       ((fn [{:keys [threes twos]}]
         (* threes twos)))))

(def part-2
  (->> (for [id1 input
             id2 input
             :when (not= id1 id2)]
         (let [out (->> (map (fn [a b] (when (= a b) a)) id1 id2)
                        (filter identity))]
           (when (#{24} (count out))
             (apply str out))))
       (some identity)))

