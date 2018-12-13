(ns aoc2018.aoc12 (:require [aoc2018.common :refer :all]
    [clojure.string :as str]))

(defn parse-state
  [s]
  (map #(if (= \# %) true false) s)
  )

  ;...## => #

(defn parse-rule
  [s]
  (let [[pattern result]
        (->> s
        (re-matches #"([.#]+) => ([.#])")
        (drop 1))
        parsed-pattern (parse-state pattern)
        parsed-result (first (parse-state result))]
      {parsed-pattern parsed-result}
      ))

(defn parse-rules
  [lines]
  (reduce (fn [acc line] (into acc (parse-rule line)) ) {} lines))
 
(defn transform-state-left
    "state - list of bools"
    [state]
    (if (not (= (take 3 state) (list false false false)))
      (list (into state (list false false false)) 3)
      (list state 0)
    )
)

(defn transform-state-right
  "state - list of bools"
  [state]
  (if (not (= (drop (- (count state) 3) state) (list false false false)))
    (concat state (list false false false))
    state
  )
)

(defn transform-state
  [state]
  (transform-state-left (transform-state-right state)))

(defn get-next-state
  [state rules]
    (let [groups (partition 5 1 state)
          next-state (map (fn [group] (let [t (rules group)] (if (nil? t) false t))) groups)]
        (concat (into next-state (list false false)) (list false false))
    )
  )

(defn to-string
  [state]
  (let [s1 (clojure.string/join (map #(if (true? %) "#" ".") state))
        s2 (clojure.string/replace s1 #"[.]+$" "")
        s3 (clojure.string/replace s2 #"^[.]+" "")]
      s3
    )
)

(defn test
  [s]
  (to-string (parse-state (first (clojure.string/split-lines s)))))

(defn print-state 
  [state]
  (println (clojure.string/join (map #(if (true? %) "#" ".") state)))
  )
; true - has plant
(defn process-states
  [s]
  (let [lines (clojure.string/split-lines s)
        initial-state (parse-state (first lines))
        rules (parse-rules (drop 1 lines))]
          (loop [i 0 state initial-state increment 0]
            ; (print-state state)
            (if (= i 20)
              (do (list increment state))
              ; (list increment state)
              (let [[transformed-state add] (transform-state state)]
                ; (print-state transformed-state)
                (recur (inc i) (get-next-state transformed-state rules) (+ add increment)))
            )
          )
  )
)

(defn solve1
  [s]
  (let [[increment final-state] (process-states s)
        pot-with-plant-fake-indexes (keep-indexed (fn [idx v] (if (true? v) idx)) final-state)
        fake-sum (reduce + pot-with-plant-fake-indexes)
        no-of-pots (count pot-with-plant-fake-indexes)
        decrement (* increment no-of-pots)]
      (println pot-with-plant-fake-indexes)
    (- fake-sum decrement)
  )
)

(defn process-states-loop-detection
  [s]
  (let [lines (clojure.string/split-lines s)
        initial-state (parse-state (first lines))
        rules (parse-rules (drop 1 lines))]
          (loop [i 0 state initial-state increment 0 seen-states-map {}]
            ; (println (to-string state))
            (let [[transformed-state add] (transform-state state)
                  next-state (get-next-state transformed-state rules)
                  next-state-str (to-string next-state)]
              (println next-state-str)
              (if (contains? seen-states-map next-state-str)
                (do (print-state state) (seen-states-map next-state-str))
                (recur 
                  (inc i)
                  next-state
                  (+ add increment) 
                  (assoc seen-states-map next-state-str i)
                )
              )
            )
          )
  )
)


(defn solve2
  [s]
  (process-states-loop-detection s)
)