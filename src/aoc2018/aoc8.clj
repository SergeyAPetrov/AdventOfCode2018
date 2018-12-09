(ns aoc2018.aoc8
    (:require [clojure.string :as str]
                [clojure.walk :refer [postwalk]]))

(defn parse-input
    [s] 
        (map #(Integer/parseInt %) (str/split s #" "))
)

(defn parse-tree
    "return node {:nodes [] :data [] :offset int}"
    [input-seq]
    (let [[no-of-childs no-of-data] (take 2 input-seq)
            [child-nodes offset] (loop [i 0 offset 2 nodes '()]
                                    (if (= i no-of-childs)
                                        [nodes offset]
                                        (let [new-node (parse-tree (drop offset input-seq))]
                                            (recur (inc i) (+ offset (new-node :offset)) (conj nodes new-node)))))]
        {:nodes child-nodes :offset (+ offset no-of-data) :data (take no-of-data (drop offset input-seq))}))

(defn calc-sum
    [node]
    (+ 
        (reduce + (node :data))
        (reduce + (map calc-sum (node :nodes)))))

(defn calc-sum2
    [node]
    (if (empty? (node :nodes)) 
        (reduce + (node :data))
        (let [nodes-data (reverse (map calc-sum2 (node :nodes)))
                mapped-nodes-data (map (fn [midx] (nth nodes-data (dec midx) 0)) (node :data))]
                (reduce + mapped-nodes-data)
            )))

(defn solve1
    [s]
    (let [tree (parse-tree (parse-input s))]
        (calc-sum tree)
    ))

(defn solve2
    [s]
    (let [tree (parse-tree (parse-input s))]
        (calc-sum2 tree)
    ))