(ns advent-of-code.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def input
  (->> (io/resource "day05-input.txt") slurp))

(defn toggle-case [c]
  (first
    (if (re-find #"[A-Z]" (str c))
      (cs/lower-case (str c))
      (cs/upper-case (str c)))))

(defn crush [s]
  (->> (reduce
         (fn [ret c]
           (let [p (last ret)]
             (if (= (toggle-case p) c)
               (subs ret 0 (dec (count ret)))
               (str ret c))))
         "" s)))

(defn completely-crush [s]
  (->> (iterate crush s)
       (partition 2 1)
       (drop-while (fn [[a b]] (not= (count a) (count b))))
       ffirst
       count))

;(def part-1
;  (completely-crush input))
;
;(def part-2
;  (->> (map (comp completely-crush
;                  #(cs/replace input % "")
;                  re-pattern
;                  #(str % "|" (cs/upper-case (str %)))
;                  char) (range (int \a) (int \z)))
;       (apply min)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn efficient-crush [input]
  (->> (reduce (fn [stack letter]
                 (if-let [top (and (seq stack) (peek stack))]
                   (if (= top (toggle-case letter))
                     (pop stack)
                     (conj stack letter))
                   (conj stack letter))) () input)
       reverse
       cs/join))

(def part-1
  (count
    (efficient-crush input)))

(def part-2
  (->> (map (comp count
                  efficient-crush
                  #(cs/replace input % "")
                  re-pattern
                  #(str % "|" (cs/upper-case (str %)))
                  char) (range (int \a) (int \z)))
       (apply min)))