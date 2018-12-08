(ns aoc2018.aoc7
    (:require [aoc2018.common :refer :all]
        [clojure.set :refer :all]))

(defn parse-input-row
    [s]
    (->> s
        (re-matches #"Step ([A-Z]) must be finished before step ([A-Z]) can begin.")
        (drop 1)))

(defn is-not-blocked
    [instructions candidate]
    (not (some (fn [i] (= candidate (second i))) instructions)))

(defn solve1
    [s]
    (let [input (parse-input s parse-input-row)]
        (loop [result '() instructions input]
            (if (empty? instructions)
                (reverse result)
                (let [next-instruction (->> instructions
                        (map first)
                        (set)
                        (filter (fn [candidate] (is-not-blocked instructions candidate)))
                        (sort)
                        (first))
                      new-instructions (filter (fn [i] (not (= next-instruction (first i)))) instructions)
                      new-result (if (empty? new-instructions)
                                         (apply conj 
                                            (conj result next-instruction) 
                                            (sort (map second instructions)))
                                         (conj result next-instruction))]
                   (recur new-result new-instructions))))))

(defn get-in-links
    [node data]
    (->> data
        (filter #(= (node :id) (second %)))
        (map first)
        (set)))

(defn get-out-links
    [node data]
    (->> data
        (filter #(= (node :id) (first %)))
        (map second)
        (set)))

(defn parse-nodes 
    [input]
    (let [unique-labels (set (apply concat input))
            nodes (map (fn [id] {:id id}) unique-labels)]
            (map (fn [node] 
                    (assoc node 
                        :in (get-in-links node input)
                        :out (get-out-links node input))
                )
            nodes)))

(defn get-next-tasks
    [task-queue instructions amount]
    (let [ongoing-tasks (map #(:instruction %) task-queue)
        not-yet-started-tasks (filter (fn [i] (not (some #(= i %) ongoing-tasks))) instructions)]
        (->> not-yet-started-tasks  
            (filter #(empty? (% :in)))
            (sort-by :id)
            (take amount))))

(defn enqueue-tasks
    [task-queue instructions]
    (let [ongoing-tasks (map #(:instruction %) task-queue)
        not-yet-started-tasks (filter (fn [i] (not (some #(= i %) ongoing-tasks))) instructions)
        tasks (map (fn [i] {:instruction i :time-to-complete (- (int (first (i :id))) 64)}) not-yet-started-tasks)]  
        (into task-queue tasks)))

(defn process-tasks
    "return 2 groups: true for completed, false for ongoing tasks"
    [task-queue]
    (let [processed-tasks (map (fn [t] (assoc t :time-to-complete (dec (t :time-to-complete)))) task-queue)]
        (->> processed-tasks
            (group-by #(= 0 (% :time-to-complete))))))

(defn update-instructions
    [finished-tasks instructions]
    (let [completed-ids (set (map :id finished-tasks))
            filtered-instructions (filter #(not (contains? completed-ids (% :id))) instructions)]
          (map (fn [i] (assoc i :in (difference (i :in) completed-ids))) filtered-instructions)))
    
(defn solve2
    [s]
    (let [input (parse-input s parse-input-row)
            instruction-graph (parse-nodes input)]
        (loop [task-queue '() instructions instruction-graph second 0]
            (if (and (empty? task-queue) (empty? instructions))
                (dec second)
                (let [  processed-tasks-groups (process-tasks task-queue)
                        completed-tasks (processed-tasks-groups true)
                        ongoing-tasks (processed-tasks-groups false)        
                        new-instructions (update-instructions (map :instruction completed-tasks) instructions)
                        free-workers (- 2 (count ongoing-tasks))
                        next-tasks (get-next-tasks ongoing-tasks new-instructions free-workers)
                        new-queue (enqueue-tasks ongoing-tasks next-tasks)
                        new-second (inc second)
                        ]
                    (recur new-queue new-instructions new-second))))))