(ns advent-of-code.day-14
  (:require [clojure.string :as cs]))

(defmacro debug
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn "-------------Start Debug-------------")
     (clojure.pprint/pprint ret#)
     (prn (str "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     (prn "--------------End Debug--------------")
     ret#))

(defn add-recipes [elf1 elf2]
  (let [x (+ elf1 elf2)
        d1 (quot x 10)
        d2 (mod x 10)]
    (if (zero? d1)
      [d2]
      [d1 d2])))

(def starting-recipes (zipmap [0 1] [3 7]))

(defn update-recipes [{:keys [elves recipes] :as state}]
  (assoc
    state :recipes
    (->> (map #(get recipes %) elves)
         (apply add-recipes )
         (zipmap (range (count recipes) (+ 3 (count recipes))))
         (merge recipes))))

(defn update-elves [{:keys [elves recipes] :as state}]
  (update
    state :elves
    #(map (fn [elf]
            (-> (recipes elf)
                (+ elf 1)
                (mod (count recipes)))) %)))

(defn update-state [{:keys [elves recipes] :as state}]
  (-> state
      update-recipes
      update-elves))

(defn scores-after [after num-after]
  (->> (iterate update-state {:elves [0 1] :recipes starting-recipes})
       (drop-while #(->> (:recipes %)
                         (count)
                         (>= (+ num-after after))))
       first
       :recipes
       (drop after)
       (take num-after)
       (map str)
       cs/join))

(defn scores-before-seq [before]
  (let [pattern (->> before str (mapv (comp read-string str))) ]
    (->> (iterate update-state {:elves [0 1] :recipes starting-recipes})
         (drop 100000000)
         first
         :recipes
         (#(map (fn [i] (get % i)) (range (count %))))
         (partition-all (count pattern) 1)
         (take-while #(not= pattern (into [] %)))
         count
         ;(drop-while (fn [{:keys [recipes]}]
         ;              (let [thing1 (take-last (count pattern) recipes)
         ;                    thing2 (drop-last (take-last (inc (count pattern)) recipes))]
         ;                (not
         ;                  (or
         ;                    (= thing1 pattern)
         ;                    (= thing2 pattern))))))
         ;first
         ;:recipes
         ;((juxt count #(take-last (+ 3 (count pattern)) %)))
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;debugs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn print-state [{:keys [elves recipes] :as state}]
  (println
    (cs/join
      (map-indexed #(let [wrap (cond
                                 (= %1 (first elves)) "(%d)"
                                 (= %1 (second elves)) "[%d]"
                                 :default " %d ")]
                      (format wrap %2)) recipes))))

(defn print-example []
  (->> (iterate update-state {:elves [0 1] :recipes [3 7]})
       (take-while #(->> (:recipes %)
                         (count)
                         (>= (+ 20))))
       (map print-state)))
;(print-example)


;part-1
;(for [after [[9    "5158916779"]
;             [5    "0124515891"]
;             [18   "9251071085"]
;             [2018 "5941429882"]]]
;  (let [result (scores-after (first after) 10)]
;    (concat
;      [(= result (second after))]
;      after
;      [result])))

;part-2
;(for [before [["51589" 9]
;              ["01245" 5]
;              ["92510" 18]
;              ;["59414" 2018]
;              ]]
;  (let [result (scores-before-seq (first before))]
;    (concat
;      [(= result (second before))]
;      before
;      [result])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;answers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def part-1 (scores-after 77201 10))

;(def part-2 (scores-before-seq "077201"))