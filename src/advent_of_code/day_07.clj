(ns advent-of-code.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [medley.core :as med]))


(def input
  (->> (io/resource "day07-input.txt")
       slurp
       cs/split-lines
       (map (fn [s] (rest (re-find #"Step (.) must be finished before step (.) can begin." s))))))

(def all-steps
  (->> input
       flatten
       distinct))

;(def blocked-steps
;  (->> input
;       (map first)
;       (into #{})))

(def step-dependencies
  (->> input
       (reduce (fn [m [blocked req]]
                 (update m req #(into #{} (conj % blocked)))) {})
       (merge (zipmap all-steps (repeat #{})))
       (into (sorted-map))))

;(defn update-dependencies [{:keys [step-dependencies visited]}]
;  (let [unblocked (into #{} (take 1 (keep (fn [[req blocked]] (when (empty? blocked) req)) step-dependencies)))
;        visited (concat visited (sort unblocked))]
;    (->> step-dependencies
;         (remove (fn [[k v]] (unblocked k)))
;         (into (sorted-map))
;         (med/map-vals (fn [blocked] (into #{} (remove unblocked blocked))))
;         (hash-map :visited visited :step-dependencies))))

(def one-sec-deps
  (->> step-dependencies
       (map (fn [[k v]] [k {:dep v :t 1}]))
       (into (sorted-map))))

(def timed-deps
  (->> step-dependencies
       (map-indexed (fn [i [k v]] [k {:dep v :t (+ 61 i)}]))
       (into (sorted-map))))


(defn async-update-dependencies [{:keys [step-dependencies visited i workers]}]
  (let [working-on (->> step-dependencies
                        (filter (comp empty? :dep second))
                        (take workers)
                        (map (fn [[k v]] [k (update v :t dec)])))
        finished (->> (filter (comp zero? :t second) working-on)
                      (map first)
                      (concat visited))]
    (->> step-dependencies
         (med/map-vals (fn [{:keys [dep t]}] {:t t :dep (into #{} (remove (into (sorted-set) finished) dep))}))
         (#(merge % (into {} working-on)))
         (remove (fn [[k v]] ((into (sorted-set) finished) k)))
         (into (sorted-map))
         (hash-map :visited finished :i (inc i) :workers workers :step-dependencies))))

;(def part-1
;  (->> (iterate update-dependencies {:step-dependencies step-dependencies
;                                     :visited           []})
;       (drop-while #(-> % :step-dependencies seq))
;       first
;       :visited
;       cs/join))

(def reusable-part-1
  (-> (drop-while #(-> % :step-dependencies seq)
                  (iterate async-update-dependencies
                           {:step-dependencies one-sec-deps
                            :visited           []
                            :i 0
                            :workers 1}))
      first
      :visited
      cs/join))

(def part-2
  (->> (iterate async-update-dependencies {:step-dependencies timed-deps :visited [] :i 0 :workers 5})
       (drop-while #(-> (:step-dependencies %) seq))
       first
       :i))