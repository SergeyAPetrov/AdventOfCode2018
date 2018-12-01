(ns aoc2018.aoc1)

(defn parse-input-row
    [s]
    {:operation (if (= \+ (first s))
                    +
                    -)
     :argument (Integer/parseInt (subs s 1))})

(defn parse-input
    [s]
    (map parse-input-row (clojure.string/split-lines s)))

(defn calculate-next-freq [prev-freq instruction] 
    ((:operation instruction) prev-freq (:argument instruction)))

(defn solve 
    [s]
    (let [parsed-freq (parse-input s)]
        (reduce calculate-next-freq 0 parsed-freq)))

(defn solve2
    [s]
    (let [parsed-freq (parse-input s)]
        (loop [index 0 freq 0 seenFreq #{}]
            (let [nextFreq (calculate-next-freq freq (nth parsed-freq index))]
                (if (contains? seenFreq nextFreq)
                    nextFreq
                    (recur 
                        (mod (inc index) (count parsed-freq))
                        nextFreq 
                        (conj seenFreq nextFreq)))))))
