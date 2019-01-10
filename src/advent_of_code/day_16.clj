(ns advent-of-code.day-16
  (:require [clojure.java.io :as io]
            [medley.core :as medley]))

;Addition:
;
;addr (add register) stores into register C the result of adding register A and register B.
;addi (add immediate) stores into register C the result of adding register A and value B.
;Multiplication:
;
;mulr (multiply register) stores into register C the result of multiplying register A and register B.
;muli (multiply immediate) stores into register C the result of multiplying register A and value B.
;Bitwise AND:
;
;banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
;bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
;Bitwise OR:
;
;borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
;bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
;Assignment:
;
;setr (set register) copies the contents of register A into register C. (Input B is ignored.)
;seti (set immediate) stores value A into register C. (Input B is ignored.)
;Greater-than testing:
;
;gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
;gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
;gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
;Equality testing:
;
;eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
;eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
;eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.

(def ops
  {:addr (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (+ (get registers a) (get registers b))))
   :addi (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (+ (get registers a) b)))

   :mulr (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (* (get registers a) (get registers b))))
   :muli (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (* (get registers a) b)))

   :banr (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (bit-or (get registers a) (get registers b))))
   :bani (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (bit-or (get registers a) b)))

   :borr (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (bit-and (get registers a) (get registers b))))
   :bori (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (bit-and (get registers a) b)))

   :setr (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (get registers a)))
   :seti (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c a))

   :gtir (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (if (> a (get registers b)) 1 0)))
   :gtri (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (if (> (get registers a) b) 1 0)))
   :gtrr (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (if (> (get registers a) (get registers b)) 1 0)))


   :eqir (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (if (= a (get registers b)) 1 0)))
   :eqri (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (if (= (get registers a) b) 1 0)))
   :eqrr (fn [[_ a b c] [r0 r1 r2 r3 :as registers]]
           (assoc registers c (if (= (get registers a) (get registers b)) 1 0)))})

(def input (slurp (io/resource "day16-input.txt")))

(defn parse-observation [s]
  (->> (re-find #"Before\: \[(\d+), (\d+), (\d+), (\d+)\]\r\n(\d+) (\d+) (\d+) (\d+)\r\nAfter\:  \[(\d+), (\d+), (\d+), (\d+)\]" s)
       rest
       (map read-string )
       (partition 4)
       (mapv (partial apply vector))))

(def observations
  (-> (clojure.string/split input #"\r\n\r\n\r\n")
      first
      (clojure.string/split #"\r\n\r\n")
      (as-> x (map parse-observation x))))

(defn parse-instructions [s] (read-string(str "[" s "]")))
(def instructions
  (->  (clojure.string/split input #"\r\n\r\n\r\n")
       last
       (clojure.string/split #"\r\n")
       rest
       (as-> xs (map parse-instructions xs))))

(defn observations->matches [ms]
  (map (fn [[in cmd out]]
         {(first cmd)
          (into #{} (keep (fn [[op f]] (when (= (f cmd in) out) op)) ops))})ms))

(defn part-1 []
  (->> observations
       observations->matches
       (filter (comp #(>= % 3) count second first))
       count))

;;;;;;;;;;;part 2

(defn observations->possible-op-maps [observations]
  (->> observations
       observations->matches
       (reduce (fn [m e] (merge-with clojure.set/intersection m e)) (sorted-map))))

(defn eliminate [m eliminated]
  (reduce #(if (zero? (dec (count (% %2))))
             %
             (update % %2 clojure.set/difference eliminated)) m (range 16)))

(defn eliminate-trivial [m]
  (let [accounted (into #{} (keep (fn [[k v]] (when (zero? (dec (count v))) (first v))) m))
        simplified (eliminate m accounted)]
    (if (= m simplified)
      (assoc m :done true)
      simplified)))

(def op-code->fn
  (->> observations
       observations->possible-op-maps
       (iterate eliminate-trivial)
       (drop-while (fn [m] (->> (vals m)
                                (some (comp pos? dec count)))))
       first
       (medley/map-vals (fn [k] (ops (first k))))))

(defn part-2 []
  (reduce (fn [reg instruction]
            ((op-code->fn (first instruction))
              instruction reg))
          [0 0 0 0]
          instructions))