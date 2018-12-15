(ns aoc2018.core
  (:gen-class)
  (:require [aoc2018.aoc13 :refer :all]))

(defn -main
  "Run solutions code"
  [& args]
  ; (let  [[the-map carts] (read-map (slurp "resources\\13.txt"))]
  ;   (println (map (fn [cart] (move-cart the-map cart)) carts))
  ;   ))
  (println (solve2 (slurp "resources\\13.txt"))))
