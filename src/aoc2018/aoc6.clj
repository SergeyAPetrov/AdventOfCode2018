(ns aoc2018.aoc6
    (:require [aoc2018.common :refer :all]))

(defn distance 
    [p1 p2]
    (+ (Math/abs (- (first (p1 :point)) (first (p2 :point))))
        (Math/abs (- (second (p1 :point)) (second (p2 :point))))))

(defn parse-input-row
    [s]
    (->> s
        (re-matches #"(\d+), (\d+)")
        (drop 1)
        (map #(Integer/parseInt %))))

(defn get-working-area
    [coords]
    (let [xmax (apply max (map first coords))
           ymax (apply max (map second coords))
           xmin (apply min (map first coords))
           ymin (apply min (map second coords))]
    [xmin ymin xmax ymax]))

(defn get-area-points
    [[xmin ymin xmax ymax]]
    (cart [(range xmin (inc xmax)) (range ymin (inc ymax))]))

(defn distance-reducer
    [area point]
    (map (fn [area-point] 
        (let [dist (distance point area-point)]
            (if (< dist (area-point :d))
                (assoc area-point :id (point :id) :d dist)
                (if (= dist (area-point :d))
                    (assoc area-point :id \x :d dist)
                    area-point))))
        area))

(defn distance-reducer2
    [area point]
    (map (fn [area-point] 
        (let [dist (distance point area-point)]
                (assoc area-point :d (+ dist (area-point :d)))))
    area))
        

(defn detect-border-ids
    [xmin ymin xmax ymax field]
    (reduce (fn [acc cell] 
                (let [x (first (cell :point))
                      y (second (cell :point))]
                    (if (or (= x xmin) (= x xmax) (= y ymin) (= y ymin))
                        (conj acc (cell :id))
                        acc)))
            (set nil)
            field))

(defn solve1 
    [s]
    (let [coords (parse-input s parse-input-row)
            main-points (map-indexed (fn [idx p] {:point p :id (char (+ 65 idx))}) coords)
            working-area (get-working-area coords)
            [xmin ymin xmax ymax] working-area
            area-points (map (fn [p] {:point p :id -1 :d Long/MAX_VALUE}) (get-area-points working-area))
            solved-field (reduce distance-reducer area-points main-points)
            border-ids (detect-border-ids xmin ymin xmax ymax solved-field)]
        (->> solved-field
            (filter (fn [cell] (not (contains? border-ids (cell :id)))))
            (group-by :id)
            (map (fn [[item coll]] (count coll)))
            (apply max))))

(defn solve2 
    [s]
    (let [coords (parse-input s parse-input-row)
            main-points (map-indexed (fn [idx p] {:point p :id (char (+ 65 idx))}) coords)
            working-area (get-working-area coords)
            [xmin ymin xmax ymax] working-area
            area-points (map (fn [p] {:point p :id -1 :d 0}) (get-area-points working-area))
            solved-field (reduce distance-reducer2 area-points main-points)]
            (->> solved-field
                (filter (fn [cell] (> 10000 (cell :d))))
                (count))))