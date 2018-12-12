(ns aoc2018.aoc11 (:require [aoc2018.common :refer :all]
    [clojure.string :as str]))

(def grid-serial-number 7689)
(def N 300)
; (def grid-serial-number 18)

(defn get-power-level
    [[x y]]
    (let [tmp (->> x
        (+ 10)
        (* y)
        (+ grid-serial-number)
        (* (+ x 10)))]
        (- (int (mod (Math/floor (/ tmp 100)) 10)) 5)))

(def field (apply concat (get-rectangle-points-partitioned [1 1 N N])))

(def single-power-map 
    (reduce (fn [acc point] (assoc acc point (get-power-level point))) {} field))

(def power-array
  (->> (get-rectangle-points-partitioned [1 1 N N])
    (map (fn [row] (map #(get-power-level %) row)))
    (to-array-2d)))

(defn get-power
  "Indexed from 1 to N"
  [x y]
  (aget power-array (dec y) (dec x)))

(defn get-square-power 
  [x1 y1 inner-square-power h]
  (let [x2 (+ x1 h) y2 (+ y1 h)]
    (+ inner-square-power (get-power x2 y2)
      (->> (range 0 h)
        (map (fn [i] (+ (get-power (+ x1 i) y2) (get-power x2 (+ y1 i)))))
        (reduce +)
      )
    )
  )
)

(defn square-power-by-size
    "return 2 element list first - extension, second power"
    [[x1 y1]]
    (let [maxRectX (- N x1)
          maxRectY (- N y1)
          rect-height-limit (min maxRectX maxRectY)]
          ; rect-extenstion-range (range 0 (inc maxRectExtension))]
      (loop [h 0 inner-square-power (get-power x1 y1) max-h 1 max-power (get-power x1 y1)]
        (let [new-h (inc h)]
            (if (> new-h rect-height-limit)
              (list max-h max-power)
              (let [square-power (get-square-power x1 y1 inner-square-power new-h)
                    new-max-h (if (> square-power max-power) new-h max-h)
                    new-max-power (max square-power max-power)]
                (recur new-h square-power new-max-h new-max-power)
              )
            )
          )
        )
    )
)

(defn solve2
    []
  ;(power height x y)
    (apply max-key first (map #(into % (square-power-by-size %)) field))
)

; (defn solve1
;     []
;     (apply max-key first (map #(conj % (rect-power %)) rect-corners))
; )

; (defn rect-points-power
;   [rect-points]
;   (->> rect-points
;     (map (fn [p] (single-power-map p)))
;     (reduce +)
;   ))

; (defn rect-power
;   [[x1 y1]]
;   ; (println (str "!" x1 " " y1 "?"))
;   (let [x2 (+ x1 2) y2 (+ y1 2)
;           rect-points (get-rectangle-points [x1 y1 x2 y2])]
;       (rect-points-power rect-points)
;   )
; )
; ; grid ({:point p :power val})
; (defn get-point-from-grid
;     [x y grid]
;     (some (fn [cell] (when 
;                         (and (= x (first (cell :point)))
;                             (= y (second (cell :point))))
;                       cell)) 
;     grid))



; ; grid ({:point p :power val})
; ; should return ({:point p :power total-val})
; (defn get-square-total-power
;     "Get total power by top left corner of square"
;     [point grid]
;     (let [x (first point)
;           y (second point)
;           total-power-value (if (or (<= 299 x 300)
;                                     (<= 299 y 300))
;                                 Long/MIN_VALUE
;                                 (let [points (get-rectangle-points [x y (+ x 2) (+ y 2)])]
;                                     ; (println points)
;                                     (->> points
;                                         (map (fn [[x y]] (get-point-from-grid x y grid)))
;                                         (map :power)
;                                         (reduce +))))]
;         {:point point :power total-power-value}
;         ))
