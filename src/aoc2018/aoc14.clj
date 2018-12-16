(ns aoc2018.aoc14
  (:gen-class))

(defn digits [n]
  (if (= n 0)
    (list 0)
    (->> n
    (iterate #(quot % 10))
    (take-while pos?)
    (mapv #(mod % 10))
    rseq)))

(defn solve1
  [n]
  (let [n10 (+ n 10)]
    (loop [a 0 b 1 recipies [3 7]]
      (let [ra (nth recipies a)
            rb (nth recipies b)
            recipy-digits (digits (+ ra rb))
            new-recipies (into recipies recipy-digits)
            new-recipies-length (count new-recipies)
            new-a (mod (+ a ra 1) new-recipies-length)
            new-b (mod (+ b rb 1) new-recipies-length)]
        (if (< new-recipies-length n10)
          (recur new-a new-b new-recipies)
          (take 10 (drop n new-recipies))
          ; new-recipies
        )
      )
    )
  )
)

(defn vector-comparer
  [input-digits queue new-digits]
  (let [extended-queue (into (apply vector (drop 1 queue)) new-digits)
        candidate1 (apply vector (take (count input-digits) extended-queue))
        candidate2 (apply vector (drop 1 extended-queue))]
    (if (= input-digits candidate1)
      (list true (dec (count new-digits)))
      (if (= input-digits candidate2)
        (list true 0)
        (list false (apply vector (drop (dec (count new-digits)) extended-queue)))
      )
    )
  )
)


(defn solve2
  [n]
  (let [input-digits (digits n)
        input-digits-vector (apply vector input-digits)
        input-lenght (count input-digits)]
    (loop [a 4 b 3 recipies [3 7 1 0 1 0] queue [3 7 1 0 1 0]]
      (let [ra (nth recipies a)
            rb (nth recipies b)
            recipy-digits (digits (+ ra rb))
            new-recipies (into recipies recipy-digits)
            new-recipies-length (count new-recipies)
            new-a (mod (+ a ra 1) new-recipies-length)
            new-b (mod (+ b rb 1) new-recipies-length)
            [result x] (vector-comparer input-digits-vector queue recipy-digits)]
        (if (true? result)
          (- new-recipies-length input-lenght x) 
          (recur new-a new-b new-recipies x)
        )
      )
    )
  )
)