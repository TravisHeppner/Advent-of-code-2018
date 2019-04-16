(ns advent-of-code.day-10)

(def grid-serial-number 9995)

(defn nth-digit [x n]
  (int
    (/
      (- x
         (mod x (Math/pow 10 (dec n)))
         (* (Math/floor (/ x (Math/pow 10 n))) (Math/pow 10 n)))
      (Math/pow 10 (dec n)))))

(defn power-level [x y sn]
  (let [rack-id (+ x 10)]
    (-> (* rack-id y)
        (+ sn)
        (* rack-id)
        (nth-digit 3)
        (- 5))))

(defn grid [sn]
  (mapv (fn [y]
          (mapv (fn [x] (power-level x y sn)) (range 1 301)))
        (range 1 301)))

(defn get-power
  ([grid size x y]
   (let [size (dec size)]
     (->>
       (for [x (range (dec x) (+ size x))
             y (range (dec y) (+ size y))]
         (get-in grid [y x]))
       (reduce +)))))

(defn optimize-power [grid min-size max-size]
  (->>
    (for [size (range min-size (inc max-size))
          x (range 1 (- 301 size))
          y (range 1 (- 301 size))]
      {:x x :y y :size size :v (get-power grid size x y)})
    (apply max-key :v)))

(def part-1
  (-> (grid grid-serial-number)
      (optimize-power 3 3)))

(def part-2
  (-> (grid grid-serial-number)
      (optimize-power 3 16)))