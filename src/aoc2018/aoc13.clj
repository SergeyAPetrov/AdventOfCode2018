(ns aoc2018.aoc13
  (:gen-class))

; (def map (read-map (slurp "resources\\13.txt")))

(def map-width 13)
(def normalize-track-map {\^ \| \v \| \< \- \> \- \/ \/ \- \- \\ \\ \| \| \+ \+ \space \space})
(def track-symbols-set #{\^ \> \v \<})
(def track-symbols [\^ \> \v \<])

(defn read-map-raw
    "return list of vectors [(x y) character]"
    [lines]
    (apply concat (map-indexed 
        (fn [i row] 
            (map-indexed (fn [j ch]
                (vector (list j i) ch))
            row)) 
        lines)))

(defn read-carts
    "return carts data {:point :direction :iteration} from raw-map - list of vectors [(x y) char]"
    [raw-map]
    (->> raw-map
        (filter (fn [[point char]] (contains? track-symbols-set char)))
        (map (fn [[point char]] {:point point :direction char :iteration 0}))))    

(defn read-map
  "return (hashmap {(x y) char)} carts list {:point :direction :iteration})"
  [s]
  (let [lines (clojure.string/split-lines s)
        raw-map (read-map-raw lines)
        hashed-map (into {} (map (fn [v] [(first v) (normalize-track-map (second v))]) raw-map))
        carts (read-carts raw-map)]
        (list hashed-map carts)))

(defn get-next-direction-crossroad
  "convert iteration and direction to new one"
  [current-direction iteration]
  (let [op (case iteration
        0 dec
        1 identity
        2 inc)]
    (nth track-symbols (mod (op (.indexOf track-symbols current-direction)) 4))))

(defn get-next-direction-sideturn
  [current-direction sideturn]
  (let [op (case (list current-direction sideturn)
    ((\^ \/)) inc
    ((\v \/)) inc
    ((\^ \\)) dec
    ((\v \\)) dec
    ((\> \/)) dec
    ((\< \/)) dec
    ((\> \\)) inc
    ((\< \\)) inc)]
  (nth track-symbols (mod (op (.indexOf track-symbols current-direction)) 4))    
  ))

(defn get-next-position
  "return new (x,y)"
  [current-position current-direction]
  (let [new-position (case current-direction
                      \^ (list (first current-position) (dec (second current-position)))
                      \v (list (first current-position) (inc (second current-position)))
                      \> (list (inc (first current-position)) (second current-position))
                      \< (list (dec (first current-position)) (second current-position))
                      )]
    new-position
    )
  )

(defn move-cart
    "Return updated cart. The-map - map of point->chars cart {:point :direction :iteration}"
    [the-map cart]
    (let [{point :point current-direction :direction iteration :iteration} cart
          new-position (get-next-position point current-direction)
          new-map-point (the-map new-position)
          new-direction (case new-map-point
                          (\| \-) current-direction
                          \+ (get-next-direction-crossroad current-direction iteration)
                          (\\ \/) (get-next-direction-sideturn current-direction new-map-point)
                          )
          new-iteration (if (= new-map-point \+) (mod (inc iteration) 3) iteration)]
        {:point new-position :direction new-direction :iteration new-iteration}
      )
    )

(defn collision-detection-reducer
  "acc has #{points} where cars are at the moment and list of new carts"
  [acc cart]
  (let [[the-map positions-set moved-carts firstCollisionDetected] acc
          positions-without-current (disj positions-set (cart :point))
          new-cart (move-cart the-map cart)
          ]
    (if (not (nil? firstCollisionDetected))
      (list the-map positions-set moved-carts firstCollisionDetected)
      (let [new-position-set (conj positions-without-current (new-cart :point))
            new-moved-carts (conj moved-carts new-cart)
            new-collision-candidate (when (contains? positions-without-current (new-cart :point)) (new-cart :point))]
        (list the-map new-position-set new-moved-carts new-collision-candidate)      
      )
    )
  )
)

(defn collision-resolution-reducer
  [acc cart]
  (let [[the-map active-carts] acc
        is-car-still-active (contains? active-carts cart)]
    (if (not is-car-still-active)
      (list the-map (disj active-carts cart))
      (let [active-carts-without-current (disj active-carts cart)
            new-cart (move-cart the-map cart)
            collision-cart (some (fn [c] (when (= (new-cart :point) (c :point)) c)) active-carts-without-current)]
        (if (not (nil? collision-cart))
          (do (println collision-cart) (list the-map (disj active-carts-without-current collision-cart)))
          (list the-map (conj active-carts-without-current new-cart)) 
        )
      )
    )
  )
)

(defn solve1
  [s]  
  (let [[the-map initial-carts] (read-map s)]
    (loop [carts initial-carts]
      (let [carts-positions (into #{} (map :point carts))
            [x y new-carts collision-point] (reduce collision-detection-reducer (list the-map carts-positions (list) nil) carts)]
        (if (not (nil? collision-point))
          collision-point
          (recur new-carts)
        )
      )
    )
  )
)

(defn solve2
  [s]  
  (let [[the-map initial-carts] (read-map s)
        initial-carts-set (into #{} initial-carts)]
    (loop [carts initial-carts-set]
      (let [ordered-carts (sort-by 
                            (juxt 
                                (comp second :point) 
                                (comp first :point)) carts)
            [x new-active-carts-set] (reduce collision-resolution-reducer (list the-map carts) ordered-carts)]
        ; (println new-active-carts-set)
        (if (= 1 (count new-active-carts-set))
          (first new-active-carts-set)
          (recur new-active-carts-set)
        )
      )
    )
  )
)


(defn print-map
    [m]
   (let [lines (map clojure.string/join 
                (partition map-width 
                    (map second 
                        (sort-by 
                            (juxt 
                                (comp second key) 
                                (comp first key)) m))))]
      (doseq [line lines]
        (println line))))