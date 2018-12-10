(ns aoc2018.core
  (:gen-class)
  (:require [aoc2018.aoc10 :refer :all]))

(defn -main
  "Run solutions code"
  [& args]
  (println (solve2 (slurp "resources\\10.txt"))))
