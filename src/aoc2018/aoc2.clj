(ns aoc2018.aoc2
    (:require [aoc2018.common :refer :all]))
 
(defn has-letter-n-times 
    [s n]
    (some (fn [[k v]] (= n (count v))) (group-by identity s)))

(defn solve1
    [s]
    (let [parsed-input (parse-input s identity)]
        (let [twoTimes (filter #(has-letter-n-times % 2) parsed-input)
              threeTimes (filter #(has-letter-n-times % 3) parsed-input)
              twoTimesCout (count twoTimes)
              threeTimesCout (count threeTimes)]
            (* twoTimesCout threeTimesCout))))

(defn get-common-letters
    [s1 s2]
    (filter #(not(nil? %)) (map (fn [c1 c2] (if (= c1 c2) c1 nil))
     s1 s2)))

(defn solve2
    [s]
    (let [parsed-input (parse-input s identity)
          word-length (dec (count (first parsed-input)))
          cartesian-product (cart [parsed-input parsed-input])
          common-letters-map (map (fn [[s1 s2]] (get-common-letters s1 s2)) cartesian-product)]
          (some (fn [letters] (and (= word-length (count letters)) letters)) common-letters-map)))
            