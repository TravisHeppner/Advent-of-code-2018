(ns advent-of-code.day-09
  (:require [clojure.java.io :as io]))


(def input
  (->> "426 players; last marble is worth 72058 points"
       (re-seq #"(\d+)")
       (mapv second)
       (mapv #(Integer/parseInt %))))

(def num-players  (first input))
(def final-marble (second input) )

(defn rotate [i circle]
  (let [size (count circle)
        rotate (mod i size)]
    (->> (cycle circle)
         (drop rotate)
         (take size))))

(defn play [{:keys [current-marble player board scores] :as state}]
  (let [current-marble (inc current-marble)]
    (if (zero? (mod current-marble 23))
      (let [rboard (rotate -7 board)]
        {:current-marble current-marble
         :player         (mod (inc player) num-players)
         :board          (rest rboard)
         :scores         (update scores player #(+ %
                                                   current-marble
                                                   (first rboard)))})
      {:current-marble current-marble
       :player         (mod (inc player) num-players)
       :board          (cons current-marble (rotate 2 board))
       :scores         scores}

      )))

(def last-two
  (->> {:current-marble       0
        :player               0
        :board                '(0)
        :scores               (zipmap (range num-players) (repeat 0))}
       (iterate play)
       (drop final-marble)
       ;(drop 22)
       (take 2)
       ;(map #(update % :board reverse))
       (map #(dissoc % :board))
       ))

; this is too slow
(def last-one
  (->> {:current-marble 0
        :player         0
        :board          '(0)
        :scores         (zipmap (range num-players) (repeat 0))}
       (iterate play)
       (drop (* 100 final-marble))
       ;(drop 22)
       (take 1)
       ;(map #(update % :board reverse))
       (map #(dissoc % :board))
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;circular linked list-ish???
(defn init-state [num-players]
  {:current-node 0
   :marble       0
   :player       0
   :scores      (zipmap (range num-players) (repeat 0))
   :nodes       {0 {:next 0
                    :val 0
                    :prev 0}}})

(defn insert [state n]
  (let [prev (-> state :current-node)
        next (-> state :nodes (get prev) :next)]
    (-> state
        (assoc :current-node n)
        (assoc-in [:nodes prev :next] n)
        (assoc-in [:nodes next :prev] n)
        (assoc-in [:nodes n] {:prev prev
                              :next next
                              :val  n}))))

(defn cw
  [{:keys [current-node] :as state}]
  (assoc state :current-node (-> state :nodes (get current-node) :next)))

(defn cww
  [{:keys [current-node] :as state}]
  (assoc state :current-node (-> state :nodes (get current-node) :prev)))

(defn remove-node
  [{:keys [current-node] :as state}]
  (let [prev (-> state :nodes (get current-node) :prev)
        next (-> state :nodes (get current-node) :next)]
    (-> (assoc state :current-node next)
        (assoc-in [:nodes prev :next] next)
        (assoc-in [:nodes next :prev] prev)
        (update :nodes #(dissoc % current-node)))))

(defn play-game [{:keys [current-node marble player scores nodes] :as state}]
  (let [marble (inc marble)]
    (if (zero? (mod marble 23))
      (let [new-state (->> (iterate cww state) (drop 7) first)
            new-head (:current-node new-state)]
        (-> new-state
            (assoc  :marble marble)
            (update :player #(mod (inc %) num-players))
            (update-in [:scores player] #(+ %
                                       marble
                                       (-> state :nodes (get new-head) :val)))
            remove-node))
      (-> state
          (assoc  :marble marble)
          (update :player #(mod (inc %) num-players))
          cw
          (insert marble)))))

(def part-2
  (->> (init-state num-players)
       (iterate play-game)
       (drop (* 100 final-marble))
       first
       :scores
       (sort-by second)
       last
       second
       ))