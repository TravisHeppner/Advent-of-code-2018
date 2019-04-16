(ns advent-of-code.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def input
  (->> (io/resource "day12-input.txt")
       slurp
       (cs/split-lines)))

(def padding (cs/join (repeat 0 \.)))

(def initial-state
  (->> input
       first
       (re-find #"initial state\: (.*)")
       second
       (#(str padding % padding))))

(def observations
  (->> input
       (drop 2)
       (map #(rest (re-find #"([#\.]{5}) => ([#\.])" %)))
       (#(zipmap (map first %) (map second %)))))

(defn fix-offset [ offset state]
  (let [offset (+ offset -1
                  (count
                    (take-while #{\.} state)))]
    {:offset offset
     :state  (->> (drop-while #{\.} state)
                  reverse
                  (drop-while #{\.})
                  reverse
                  cs/join)}))

(defn apply-rules
  [observations {:keys [state offset]}]
  (->> state
       (#(str "..." % "..."))
       (partition 5 1)
       (map cs/join)
       (map observations)
       cs/join
       (fix-offset offset)))

(defn score-pots [{:keys [state offset]}]
  (->> state
       (map-indexed (fn [i c]
                      (let [i (+ i offset)]
                        (if (#{\#} c)
                          i 0))))
       (reduce +)))

(def part-1
  (->> (iterate (partial apply-rules observations) {:state initial-state :offset 0})
       (drop 20)
       first
       score-pots))

;note that past 100 the difference is always 57
(def differences-between-scores
  (->> (iterate (partial apply-rules observations) {:state initial-state :offset 0})
       (drop 100)
       (take 10000)
       (map score-pots)
       (partition 2 1)
       (map (partial apply -))
       frequencies))

(def part-2
  (->> (iterate (partial apply-rules observations) {:state initial-state :offset 0})
       (drop 100)
       first
       score-pots
       (+ (* (- 50000000000 100) 57) )))