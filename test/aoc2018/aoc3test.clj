(ns aoc2018.aoc3test
    (:require [clojure.test :refer :all]
              [aoc2018.aoc3 :refer :all]))

(deftest day3
    (testing "input parse check"
        (is (= (parse-input-row "#1 @ 1,3: 4x4") {:id 1 :y1 3 :x1 1 :y2 6 :x2 4}))
    )
    (testing "rectangle contains point"
        (is (true? (point-inside-rectangle {:x 3 :y 4} {:id 1 :y1 3 :x1 1 :y2 7 :x2 5})))
        (is (false? (point-inside-rectangle {:x 1 :y 2} {:id 1 :y1 3 :x1 1 :y2 7 :x2 5})))
        (is (true? (point-inside-rectangle {:x 2 :y 2} {:id 1 :y1 1 :x1 1 :y2 2 :x2 2})))
        (is (true? (point-inside-rectangle {:x 2 :y 3} {:x1 2 :y1 3 :y2 4 :x2 4})))
        (is (true? (point-inside-rectangle {:x 2 :y 3} {:x1 2 :y1 3 :x2 2 :y2 4})))
    )
    (testing "rectangles contains point"
        (is (= 
            (point-inside-rectangles-count 
                {:x 3 :y 4} 
                [{:id 1 :y1 3 :x1 1 :y2 7 :x2 5} 
                 {:id 2 :y1 1 :x1 1 :y2 1 :x2 1}]) 
            1))
        (is (= 
            (point-inside-rectangles-count {:x 1 :y 2} [{:id 1 :y1 3 :x1 1 :y2 7 :x2 5}]) 
            0))
    )
    (testing "solve1"
        (is (= (solve1 "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2") 4))
    )
    (testing "rectangles collide"
        (is (true? (rectangles-collide {:x1 0 :y1 0 :y2 2 :x2 2} {:y1 1 :x1 1 :y2 3 :x2 4})))
        (is (true? (rectangles-collide {:x1 0 :y1 0 :y2 2 :x2 2} {:y1 2 :x1 2 :y2 3 :x2 4})))
        (is (true? (rectangles-collide {:x1 2 :y1 3 :y2 4 :x2 4} {:x1 2 :y1 3 :x2 2 :y2 4})))
        (is (true? (rectangles-collide {:id 42, :x1 318, :y1 469, :x2 340, :y2 484} {:id 105, :x1 322, :y1 455, :x2 332, :y2 476})))
        (is (false? (rectangles-collide {:x1 0 :y1 0 :y2 2 :x2 2} {:y1 3 :x1 2 :y2 4 :x2 4})))
    )
    (testing "solve2"
        (is (= ((solve2 "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2") :id) 3))
    )
)

            