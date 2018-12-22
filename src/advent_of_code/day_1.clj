(ns advent-of-code.day-1
  (:require
    [clojure.java.io :as io]))

(def input
  (->> (io/resource "day1-input.edn")
       slurp
       read-string))

(def first-part
  (reduce + input))

(def second-part
  (->> (cycle input)
       (hash-map :visited #{0} :freq 0 :s)
       (iterate (fn [{visited :visited freq :freq [a & r] :s :as state}]
                  (let [new-freq (+ freq a)]
                    (if (visited new-freq)
                      (assoc state :done new-freq)
                      {:visited (conj visited new-freq)}
                      :s r
                      :freq new-freq))))
       (drop-while (comp not :done))
       first
       :done))