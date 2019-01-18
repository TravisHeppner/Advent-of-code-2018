(ns advent-of-code.day-04
  (:require [clojure.string :as cs]
            [clojure.java.io :as io])
  (:import [Integer]))

(def input
  (->> (io/resource "day04-input.txt")
       slurp
       (cs/split-lines )
       (map #(rest (re-find #"\[(\d+)-(\d+)-(\d+) (\d+)\:(\d+)\] (?:Guard #(\d+) begins shift|(wakes up)|(falls asleep))" %)))
       (sort-by #(cs/join (drop-last 3 %)))))

(defn partition-shifts [events]
  (reverse
    (loop [[guard & in] events shifts []]
      (if guard
        (recur
          (drop-while #(not (:guard %)) in)
          (cons
            (cons guard (take-while #(not (:guard %)) in))
            shifts))
        shifts))))

(defn normalize [[yyyy MM dd hh mm guard wakes sleeps :as event]]
  (let [event {:minute (Integer/parseInt mm)
               :hour   (Integer/parseInt hh)
               :tuple event}]
    (cond
      guard (assoc event :guard guard :awake true)
      wakes  (assoc event :awake true)
      sleeps (assoc event :awake false))))

(defn normalize-shifts [shifts]
  (map (fn [[{:keys [minute hour guard] :as start} & r :as times]]
         (let [periods
               (partition 2 1
                          (cond
                            (= 23 hour) (concat [(assoc start :minute 0)] r [{:minute 60}])
                            (= 00 hour minute) (concat times [{:minute 60}])
                            :default (concat [{:minute 0 :awake false}] times [{:minute 60}])))]
           {:guard   guard
            :from    times
            :minutes (mapcat (fn [[{:keys [minute awake]} {minute2 :minute}]]
                               (repeat (- minute2 minute) (not awake))) periods)})) shifts))

(defn normalize-guards [guards]
  (map (fn [[guard v]]
         (let [minutes (->> v
                            (map :minutes)
                            (apply map (comp count #(filter true? %) vector))
                            (map-indexed #(hash-map :minute % :shifts-asleep %2)))]
           {:guard            guard
            :minutes-asleep   (->> v (mapcat :minutes) (filter identity) count)
            :minutes-awake    (->> v (mapcat :minutes) (remove identity) count)
            :shifts           (count v)
            :asleep-on-minute minutes})) guards))

(def guards
  (->> input
       (map normalize)
       partition-shifts
       normalize-shifts
       (group-by :guard)
       normalize-guards))

(defn part-1-fn [{:keys [guard asleep-on-minute]}]
  (->> (apply max-key :shifts-asleep asleep-on-minute)
       :minute
       (* (Integer/parseInt guard))))

;;3212
(def part-1
  (->> guards
       (apply max-key :minutes-asleep)
       part-1-fn))

(defn most-minutes-slept [{:keys [guard asleep-on-minute]}]
  (-> (apply max-key :shifts-asleep asleep-on-minute)
      (assoc :guard guard)))

;;4966
(def part-2
  (->> guards
       (map most-minutes-slept)
       (apply max-key :shifts-asleep)
       ((fn [{:keys [guard minute]}] (* (Integer/parseInt guard) minute)))))