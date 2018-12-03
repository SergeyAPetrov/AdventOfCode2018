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
