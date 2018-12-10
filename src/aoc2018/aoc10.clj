(ns aoc2018.aoc10 (:require [aoc2018.common :refer :all]
    [clojure.string :as str]))
; {:p (x,y) :v (x,y)}

(defn parse-input-row1
    [s]
    (let [[x y vx vy]
        (->> s
        (re-matches #"position=<\s*([-]?\d+),\s*([-]?\d+)>\s*velocity=<\s*([-]?\d+),\s*([-]?\d+)>")
        (drop 1)
        (map str/trim)
        (map #(Integer/parseInt %)))]
    {:p (list x y) :v (list vx vy) }))

; (def p0 {:p '(3,9) :v '(1,-2)})

(defn apply-velocity
    [point]
    (let [[x,y] (point :p)
            [vx,vy] (point :v)
        new-coords (list (+ x vx) (+ y vy))]
        (assoc point :p new-coords)))

(defn figure-height
    [points]
    (let [ycoords (map (fn [p] (second (p :p))) points)
          ymax (apply max ycoords)
          ymin (apply min ycoords)]
          (- ymax ymin)))

(defn drawing-area
    [points]
    (let [ycoords (map (fn [p] (second (p :p))) points)
          ymax (apply max ycoords)
          ymin (apply min ycoords)
          xcoords (map (fn [p] (first (p :p))) points)
          xmax (apply max xcoords)
          xmin (apply min xcoords)]
          [xmin ymin xmax ymax]))

(defn solve1
    [s]
    (let [starting-points (parse-input s parse-input-row1)]
        ; starting-points))
        (loop [points starting-points height (figure-height starting-points)]
            (let [new-points (map apply-velocity points)
                  new-height (figure-height new-points)]
                (if (< new-height height)
                    (recur new-points new-height)
                    points)))))


(defn solve2
    [s]
    (let [starting-points (parse-input s parse-input-row1)]
        ; starting-points))
        (loop [points starting-points height (figure-height starting-points) i 0]
            (let [new-points (map apply-velocity points)
                    new-height (figure-height new-points)]
                (if (< new-height height)
                    (recur new-points new-height (inc i))
                    i)))))

(defn get-line
    [line-coords points]
    (clojure.string/join (map (fn [[x y]] 
            (if (some (
                fn [{p :p}] 
                (and 
                    (= x (first p)) 
                    (= y (second p)))) points)
            "#"
            "."))
          line-coords)))

(defn draw 
    [points]
    (let [rect (drawing-area points)
          rect-points (get-rectangle-points-partitioned rect)]
          (doseq [line rect-points]
            ; (println line)
            (println (get-line line points)))))

(defn solve-and-draw
    [s]
    (draw (solve1 s)))