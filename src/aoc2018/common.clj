(ns aoc2018.common)

(defn parse-input
    [s parse-row]
    (map parse-row (clojure.string/split-lines s)))

(defn cart [colls]
    (if (empty? colls)
        '(())
        (for [x (first colls)
            more (cart (rest colls))]
        (cons x more))))

(defn transpose [m]
    (apply mapv vector m))

(defn get-rectangle-points
    [[x1 y1 x2 y2]]
    (let [xs (range x1 (inc x2))
          xy (range y1 (inc y2))
          rect-points (cart [xs xy])]
        rect-points))
    
(defn get-rectangle-points-partitioned
    [[x1 y1 x2 y2]]
    (let [xs (range x1 (inc x2))
          xy (range y1 (inc y2))
          rect-points (cart [xs xy])]
        (transpose (partition (inc (- y2 y1)) rect-points))))