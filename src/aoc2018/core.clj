(ns aoc2018.core
  (:gen-class)
  (:require [aoc2018.aoc2 :refer :all]))

(defn -main
  "Run solutions code"
  [& args]
  (println (clojure.string/join "" (solve2 (slurp "resources\\2.txt")))))
