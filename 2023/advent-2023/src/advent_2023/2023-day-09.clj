(ns advent-2023.day-07
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-2023.utils :refer [parse-int parse-long read-input]]))

(def s (read-input "day09-sample.txt"))
;(def s2 (read-input "day09-sample2.txt"))
(def i (read-input "day09.txt"))

(defn parse-line [line]
  (map parse-int (str/split line #" +")))

(parse-line "0 3 6 9 12 15")

(defn reduce-part1 
  ([a] 
   (print a "\n") 
   (reduce-part1 (drop 1 a) (first (first a))))
  ([a d] (let [v (+ d (first (first a)))]
           (if (= 1 (count a)) v
               (reduce-part1 (drop 1 a) v)))))

(defn compute-part1 
  ([c] (compute-part1 (list c) c))
  ([a v] (let [d (map (fn [[a b]] (- a b)) (partition 2 1 v))]
           (if (every? zero? d) (reduce-part1 a)
               (compute-part1 (conj a d) d)))))

; REPL testing
(compute-part1 (reverse (parse-line "0 3 6 9 12 15")))
(compute-part1 (reverse (parse-line "1 3 6 10 15 21")))
(compute-part1 (reverse (parse-line "10  13  16  21  30  45 ")))

(defn part1 [s]
  (let [cs (map parse-line (str/split s #"\n"))
        vs (map (fn [c] (compute-part1 (reverse c))) cs)]
    (reduce + 0 vs)))

(defn part2 [s]
  (let [cs (map parse-line (str/split s #"\n"))
        vs (map (fn [c] (compute-part1 c)) cs)]
    (reduce + 0 vs)))

(part1 s)
(part1 i)

(part2 s)
(part2 i)
