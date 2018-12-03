(ns aoc2018.aoc1
    (:require [aoc2018.common :refer :all]))

(defn parse-input-row
    [s]
    {:operation (if (= \+ (first s))
                    +
                    -)
     :argument (Integer/parseInt (subs s 1))})

(defn calculate-next-freq [prev-freq instruction] 
    ((:operation instruction) prev-freq (:argument instruction)))

(defn solve 
    [s]
    (let [parsed-freq (parse-input s parse-input-row)]
        (reduce calculate-next-freq 0 parsed-freq)))

(defn solve2
    [s]
    (let [parsed-freq (parse-input s parse-input-row)]
        (loop [index 0 freq 0 seenFreq #{}]
            (let [nextFreq (calculate-next-freq freq (nth parsed-freq index))]
                (if (contains? seenFreq nextFreq)
                    nextFreq
                    (recur 
                        (mod (inc index) (count parsed-freq))
                        nextFreq 
                        (conj seenFreq nextFreq)))))))
