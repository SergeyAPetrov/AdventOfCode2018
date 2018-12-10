(ns aoc2018.aoc9)

(def largest-marble 7201901) ; 25+1
(def players 459) ; 9+1

(def game-seq (map vector (cycle (range 1 players)) (range 1 largest-marble)))

(def circle {:idx 0 :data '(0)})
(def circle {:idx 1 :data '(0 4 2 1 3)})
(def circle {:idx 3 :data '(0 4 2 5 1 3)})
(def circle {:idx 5 :data '(0 4 2 5 1 6 3)})
(def circle {:idx 7 :data '(0 4 2 5 1 6 3 7)})
(def circle {:idx 13 :data '(0 16 8 17 4 18 9 19 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15)})

(defn add-to-circle
    "adds int to circle {:idx currentIndex :data current state}"
    [circle marble]
    (let [new-idx (mod (+ 2 (circle :idx)) (count (circle :data)))
            [head tail] (split-at new-idx (circle :data))]
                {:idx new-idx :data (concat head (conj tail marble))})
    )

(defn remove-from-circle
    "remove int from circle {:idx currentIndex :data current state}"
    [circle]
    (let [new-idx (mod (- (circle :idx) 7) (count (circle :data)))
            [head tail] (split-at (inc new-idx) (circle :data))]
                [(last head) {:idx new-idx :data (concat (take (dec (count head)) head) tail)}])
    )

(defn game-reducer
    "accum is {:circle current state :score map of playerNo to score"
    [accum next-game-seq]
        (let [circle (accum :circle)
                score (accum :score)
                [player marble] next-game-seq]
            (if (= 0 (rem marble 23))
                (let [new-score1 (update score player + marble)
                        [removed-marble new-circle] (remove-from-circle circle)
                        new-score2 (update new-score1 player + removed-marble)]
                        {:circle new-circle :score new-score2})
                {:score score :circle (add-to-circle circle marble)}
            ))
    )

(defn solveX
    []
    (let [score (zipmap (range 1 players) (repeat players 0))]
        (->> (reduce game-reducer {:score score :circle {:idx 0 :data '(0)}} game-seq)
            (:score)
            (map val)
            (apply max))))
    ; new-circle (add-to-circle circle next-marble)