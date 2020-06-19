(ns advent-of-code.day-14
  (:require [clojure.string :as cs]))

(defn print-state [{:keys [elves sorted-recipes] :as state}]
  (println
    (cs/join
      (map-indexed #(let [wrap (cond
                                 (= %1 (first elves)) "(%d)"
                                 (= %1 (second elves)) "[%d]"
                                 :default " %d ")]
                      (format wrap %2)) (reverse sorted-recipes)))))

(def initial-state
  {:elves [0 1]
   :recipes {0 3 1 7}
   :sorted-recipes '(7 3)
   :num-recipes 2})

(defn add-recipes [elf1 elf2]
  (let [x (+ elf1 elf2)
        d1 (quot x 10)
        d2 (mod x 10)]
    (if (zero? d1)
      [d2]
      [d1 d2])))

(defn step [{:keys [recipes num-recipes] [elf1 elf2] :elves :as state}]
  (let [nums (add-recipes (get recipes elf1) (get recipes elf2))]
    (-> (reduce (fn [state [i recipe]] (-> (assoc-in state [:recipes i] recipe)
                                           (update-in [:sorted-recipes] conj recipe))) state (map-indexed (fn [i n] [(+ i num-recipes) n]) nums))
        (update :num-recipes #(+ % (count nums)))
        (update-in [:elves 0] (fn [r] (mod (+ r (get recipes r) 1) (+ (count nums) num-recipes))))
        (update-in [:elves 1] (fn [r] (mod (+ r (get recipes r) 1) (+ (count nums) num-recipes)))))))

(defn print-example []
  (->> (iterate step initial-state)
       (take-while #(<= (:num-recipes %) (+ 20)))
       (map print-state)))

(comment
  ;part-1
  (->> (iterate step initial-state)
       (take 100000)
       (drop-while #(> (+ 77201 10) (:num-recipes %)))
       first
       :recipes
       ((fn [recipes] (map (fn [i] (get recipes i)) (range 77201 77211)))))

  ;part-2
  (let [pattern [0 7 7 2 0 1]
        digits (count pattern)
        flipped (reverse pattern)]
    (->> (iterate step initial-state)
         (take 1000000000)
         (drop 100000000)
         (map (fn [{:keys [sorted-recipes num-recipes]}]
                (let [first-possibility (take digits sorted-recipes)
                      second-possibility (take digits (rest sorted-recipes))]
                  (cond
                    (= flipped first-possibility) (- num-recipes digits)
                    (= flipped second-possibility) (- num-recipes digits 1)
                    :default nil))))
         (drop-while nil?)
         first))
  )