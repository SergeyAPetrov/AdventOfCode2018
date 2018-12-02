(ns aoc2018.aoc2test
    (:require [clojure.test :refer :all]
              [aoc2018.aoc2 :refer :all]))

(deftest day2
    (testing "letter freq check"
        (is (true? (has-letter-n-times "abc" 1)))
        (is (true? (has-letter-n-times "aabc" 2)))
        (is (nil? (has-letter-n-times "aabc" 3)))
        (is (true? (has-letter-n-times "aabbc" 2)))
        (is (nil? (has-letter-n-times "aabbcc" 1)))
        (is (nil? (has-letter-n-times "aaabc" 2))))
    (testing "part1"
        (is (= (solve1 "bababc") 1))
        (is (= (solve1 "bababc\nabbcde") 2))
        (is (= (solve1 "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab\n") 12)))
    (testing "get common letters"
        (is (= (get-common-letters "aa" "ab") '(\a)))
        (is (= (get-common-letters "abc" "aba") '(\a \b)))
        (is (= (get-common-letters "abc" "def") '()))
        (is (= (get-common-letters "abc" "abc") '(\a \b \c)))
        (is (= (get-common-letters "abcde" "axcye") '(\a \c \e)))
        (is (= (get-common-letters "fghij" "fguij") '(\f \g \i \j))))
    (testing "part2"
        (is (= (solve2 "aa\nab") '(\a)))
        (is (= (solve2 "abcdef\nabcdeg") '(\a \b \c \d \e)))
        (is (= (solve2 "abcdef\nabcdeg\naxcduf") '(\a \b \c \d \e)))
        ))