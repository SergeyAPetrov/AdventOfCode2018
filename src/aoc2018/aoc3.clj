(ns aoc2018.aoc3
    (:require [aoc2018.common :refer :all]))

(defn parse-input-row
    [s]
    (let [[id x1 y1 w h]
        (->> s
        (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
        (drop 1)
        (map #(Integer/parseInt %)))]
    {:id id :x1 x1 :y1 y1 :x2 (dec (+ x1 w)) :y2 (dec (+ y1 h))}))

(defn point-inside-rectangle
    [point rectangle]
    (and (<= (rectangle :x1) (point :x) (rectangle :x2))
            (<= (rectangle :y1) (point :y) (rectangle :y2))))

(defn point-inside-rectangles-count 
    [point rectangles]
    (->> rectangles
        (filter #(point-inside-rectangle point %))
        (count)))

(defn point-inside-rectangles-tmp 
    [point rectangles]
        (filter #(point-inside-rectangle point %) rectangles)
        )

(defn rectangles-collide
    [rect1 rect2]
    (let [xs (range (rect1 :x1) (inc (rect1 :x2)))
            xy (range (rect1 :y1) (inc (rect1 :y2)))
            rect1points (cart [xs xy])]
            (if (some (fn [[x y]] (point-inside-rectangle {:x x :y y} rect2)) rect1points)
            true
            false)))
            
(defn rectangle-does-not-collide-rectangles
    [rect rectangles]
        (if (some #(rectangles-collide rect %) rectangles)
        nil
        rect))

(defn solve1
    [s]
    (let [rectangles (parse-input s parse-input-row)
          coord-pairs (cart [(range 1001) (range 1001)])
          points (map (fn [[a b]] {:x a :y b}) coord-pairs)]
         (->> points
            (map #(point-inside-rectangles-count % rectangles))
            (filter #(> % 1))
            (count))))

(defn solve2
    [s]
    (let [rectangles (parse-input s parse-input-row)]
        (some (fn [r] (rectangle-does-not-collide-rectangles r (remove #(= r %) rectangles))) rectangles)))        
