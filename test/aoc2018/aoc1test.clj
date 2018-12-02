(ns aoc2018.aoc1test
    (:require [clojure.test :refer :all]
              [aoc2018.aoc1 :refer :all]))

(deftest day1
    (testing "one part input"
        (let [result (parse-input-row "+1")]
            (is (= (:operation result) +))
            (is (= (:argument result) 1))
        )
        (let [result (parse-input-row "-2")]
            (is (= (:operation result) -))
            (is (= (:argument result) 2))
        ))
    (testing "full input"
        (let [result (parse-input "+1\n-2")]
            (let [first (nth result 0) second (nth result 1)]
                (is (= (:operation first) +))
                (is (= (:argument first) 1))
                (is (= (:operation second) -))
                (is (= (:argument second) 2)))))
    (testing "part1"
        (is (solve "+1\n+1\n+1") 3)
        (is (solve "+1\n+1\n-2") 0)
        (is (solve "-1\n-2\n-3") -6))
    (testing "part2"
        (is (solve "+1\n-1") 0)
        (is (solve "+3\n+3\n+4\n-2\n-4") 10)
        (is (solve "-6\n+3\n+8\n+5\n-6") 5)
        (is (solve "+7\n+7\n-2\n-7\n-4") 14)))
  