(ns aoc2018.aoc4test
    (:require [clojure.test :refer :all]
              [aoc2018.aoc4 :refer :all]
              [aoc2018.common :refer :all]))

(defn prepare-day
    [s]
    (parse-input s parse-input-row))

(defn convert-sleep
    [sleeps]
    (clojure.string/join (map #(if % "#" ".") sleeps)))

(deftest day4
    (testing "input parse check"
        (is (= 
            (parse-input-row "[1518-11-01 00:00] Guard #10 begins shift")
            {:month 11 :day 1 :hour 0 :minute 0 :text "Guard #10 begins shift"}))
        (is (= 
            (parse-input-row "[1518-11-01 23:58] Guard #99 begins shift")
            {:month 11 :day 2 :hour 0 :minute 0 :text "Guard #99 begins shift"}))
    )
    (testing "process sleep"
        (is (=
            (convert-sleep (process-sleep [{:minute 5} {:minute 25} {:minute 30} {:minute 55}]))
            ".....####################.....#########################....."))
        (is (=
            (convert-sleep (process-sleep [{:minute 40} {:minute 50}]))
            "........................................##########.........."))
        (is (=
            (convert-sleep (process-sleep [{:minute 1} {:minute 59}]))
            ".##########################################################.")))
    (testing "process day"
        (is (= 
            (update 
                (process-day (prepare-day "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up"))
                :sleep-bits
                convert-sleep
            )
            {:month 11 :day 1 :guard-id 10 :sleep-bits ".....####################.....#########################....."}))
        (is (= 
            (update 
                (process-day (prepare-day "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:00] falls asleep\n[1518-11-01 00:59] wakes up"))
                :sleep-bits
                convert-sleep
            )
            {:month 11 :day 1 :guard-id 10 :sleep-bits "###########################################################."}))
        ; (is (= 
        ;     (parse-input-row "[1518-11-01 23:58] Guard #99 begins shift")
        ;     {:month 11 :day 2 :hour 0 :minute 0 :text "Guard #99 begins shift"}))
    )
)