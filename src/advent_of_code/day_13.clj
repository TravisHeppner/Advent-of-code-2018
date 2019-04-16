(ns advent-of-code.day-13
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))


(def input
  (->>
    "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "
    ;"/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"
    ;(io/resource "day13-input.txt")
    ;slurp

    (cs/split-lines)
    (mapv #(into [] %))))

(def cart->path {\> \-
                 \< \-
                 \^ \|
                 \v \|})

(def dirs #{\> \^ \< \v})
(def turn-ccw {\> \^ \< \v \^ \< \v \>})
(def turn-cw {\^ \>, \v \<, \< \^, \> \v})
(def consevative-turn {\> \^ \< \v \^ \> \v \<})
(def liberal-turn {\> \v \< \^ \^ \< \v \>})

;(def intersection
;  {0 turn-ccw
;   1 identity
;   2 turn-cw})

(defn grid->cells [grid]
  (remove (comp #{\space} :val)
    (for [y (range 0 (inc (count grid)))
          x (range 0 (inc (apply max (map count grid))))]
      (hash-map :x x :y y :val (get-in grid [y x])))))

(def starting-carts
  (->> (grid->cells input)
       (filter (comp dirs :val))
       (mapv #(assoc % :choice 0 :tick 0))))

(def starting-map
  (reduce (fn [grid {:keys [x y val]}]
            (update-in grid [y x] cart->path)) input starting-carts))

(defn advance-cart [{:keys [x y val choice] :as cart}]
  (update
    (cond
      (#{\<} val) (update cart :x dec)
      (#{\>} val) (update cart :x inc)
      (#{\^} val) (update cart :y dec)
      (#{\v} val) (update cart :y inc))
    :tick inc))

(defn turn-cart
  [grid {:keys [x y val choice] :as cart}]
  (let [cell (get-in grid [y x])]
    (cond
      (#{\\} cell) (update cart :val liberal-turn)
      (#{\/} cell) (update cart :val consevative-turn)
      (#{\+} cell) (-> cart
                       (update :choice #(mod (inc %) 3))
                       (update :val #(nth (drop 3 (iterate turn-cw %)) choice)))
      :default cart)))

(defn move-cart [cart]
  (turn-cart starting-map
    (advance-cart cart)))

(defn move-each-cart [carts]
  (reduce (fn [carts i]
            (if (not (get-in carts [i :crashed]))
              (let [new-carts (update carts i move-cart)
                    {:keys [x y] :as cart} (get new-carts i)
                    newly-crashed (->> new-carts
                                       (remove :crashed)
                                       (filter #(and (= x (:x %)) (= y (:y %)))))]
                (if (zero? (dec (count newly-crashed)))
                  new-carts
                  (mapv (fn [cart] (if (and (= x (:x cart)) (= y (:y cart)))
                                     (assoc cart :crashed true :val \X)
                                     cart)) new-carts)))
              carts))
          carts (range (count carts))))

(defn clr [];clear a screen
  (do
    (print (str (char 27) "[2J")) ; clear screen
    (print (str (char 27) "[;H")))) ; move cursor to the top left corner of the screen

(defn mark-grid [grid carts]
  (let [frame (->> (reduce (fn [grid {:keys [x y val]}] (assoc-in grid [y x] val)) grid carts)
                   (map cs/join)
                   (cs/join "\n"))]
    (Thread/sleep 500)
    (clr)
    (println frame)))
;
(def part-1
  (->> (iterate move-each-cart starting-carts)
       (drop-while #(not (some :crashed %)))
       first
       (filter :crashed)
       first
       ((juxt :x :y))
       ))
;
;(def part-2
;  (->> (iterate move-each-cart starting-carts)
;       (drop-while (comp pos? dec count #(remove :crashed %)))
;       first
;       (remove :crashed)
;       first
;       ((juxt :x :y))))