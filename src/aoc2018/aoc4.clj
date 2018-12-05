(ns aoc2018.aoc4
    (:require [aoc2018.common :refer :all]))

; true - sleep
(defn parse-input-row
    [s]
    (let [[monthStr dayStr hourStr minuteStr text]
        (->> s
        (re-matches #"\[1518-(\d+)-(\d+) (\d+):(\d+)\] (.*)")
        (drop 1))]
    (let [minute (Integer/parseInt minuteStr)
          hour (Integer/parseInt hourStr)
          day (Integer/parseInt dayStr)
          month (Integer/parseInt monthStr)]
        (if (= hour 23)
            (let [
                new-day (.plusDays (java.time.LocalDate/of 1518 month day) 1)]
                {:month (.getMonthValue new-day) 
                :day (.getDayOfMonth new-day)
                :hour 0  
                :minute 0  
                :text text})
            {:month month 
            :day day 
            :hour hour 
            :minute minute  
            :text text}
        ))))

(defn process-sleep
    [sleeps]
    (let [result 
        ((reduce (fn 
        [accum entry]
        (let [interval (repeat (- (entry :minute) (count (accum :result))) (accum :sleep))]
            ; (println (conj (accum :result) interval))
            {:sleep (not (accum :sleep)) :result (concat (accum :result) interval)}
            )) 
        {:sleep false :result []} sleeps) :result)]
    (concat result (repeat (- 60 (count result)) false)))
)

(defn process-day
    [day-entries]
    ; (println day-entries)
    (let [entries (sort-by (juxt :minute :text) day-entries)
          first-entry (first entries)
          guardid (Integer/parseInt (re-find #"\d+" (first-entry :text)))
          sleeps (drop 1 entries)
          sleep-bits (process-sleep sleeps)]
    {:month (first-entry :month)
     :day (first-entry :day)
     :guard-id guardid
     :sleep-bits sleep-bits
     :sleep-minutes (count (filter true? sleep-bits))}))

(defn total-sleep
    [days]
    (->> days
        (map :sleep-minutes)
        (reduce +)))

(defn most-sleepy-minute
    [days]
    (println (str "DAYS: " days))
    (let [x (->> days
        (map :sleep-bits)
        (map (fn [sleepy-bits] (filter #(true? (:item %)) (map-indexed (fn [idx itm] {:index idx :item itm}) sleepy-bits))))
        (reduce concat)        
    )]
    (if (empty? x) 
        ()
        (->> x
            (map :index)
            (group-by identity)
            (apply max-key (fn [[item coll]] (count coll) ))
        ))
    ))

(defn solve1
    [s]
    (let [parsed-rows (parse-input s parse-input-row)
            day-rows (group-by (juxt :month :day) parsed-rows)
            guard-info (->> day-rows
                (map second)
                (map process-day)
                (group-by :guard-id))
            sleepy-guard (->> guard-info
                (map (fn [[id days]] {:guard-id id :sleep-minutes (total-sleep days) :days days} ))
                (apply max-key :sleep-minutes))
            sleepy-minute (first (most-sleepy-minute (sleepy-guard :days)))]
        (* sleepy-minute (sleepy-guard :guard-id))
    ))

(defn solve2
    [s]
    (let [parsed-rows (parse-input s parse-input-row)
            day-rows (group-by (juxt :month :day) parsed-rows)
            guard-info (->> day-rows
                (map second)
                (map process-day)
                (group-by :guard-id))
            sleepy-minutes (map (fn [[guard-id data]] {:guard-id guard-id :sleepy-minute (most-sleepy-minute data)}) guard-info)
            result (apply max-key (fn [info] (count (second (info :sleepy-minute))) ) sleepy-minutes)]
       (* (result :guard-id) (first (result :sleepy-minute)))
    ))