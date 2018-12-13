(ns aoc2018.core
  (:gen-class)
  (:require [aoc2018.aoc12 :refer :all]))

(defn -main
  "Run solutions code"
  [& args]
  (println (solve1 (slurp "resources\\12.txt"))))
