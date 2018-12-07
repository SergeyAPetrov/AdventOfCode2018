(ns aoc2018.aoc5)

(defn units-react
    [a b]
        (= 32 (Math/abs (- (int a) (int b)))))

(defn validate-string
    [s]
    (reduce (fn [acc char]
        (when (units-react acc char) (println (str "AXHTUNG:" acc char)))
        char) s))

(defn polymer-reducer
    [stack unit]
    (if (and 
            (not (empty? stack))
            (units-react (peek stack) unit))
        (pop stack)
        (conj stack unit)))

(defn solve
    [s]
    (reduce polymer-reducer '() s))

(defn solve1
    [s]
    (count (solve s)))

(defn strip [coll chars]
    (apply str (remove #((set chars) %) coll)))
;polymer (clojure.string/join (solve s)))
(defn solve2
    [s]
    (let [
        chars (map (fn [c] [(char c) (char (+ 32 c))]) (range 65 91))
        no-char-polymer (map #(strip s %) chars)
        reacted-polymer (map solve no-char-polymer)
        reacted-polymer-length (map count reacted-polymer)
    ]
        (apply min reacted-polymer-length)
))