(ns day_01
  (:require [util]))

(defn depth-increase? [pair]
  (if (nil? (first pair))
    false
    (< (first pair) (nth pair 1))))

(def input-filename "day_01.txt")

(def data (map util/parse-int (util/load-data input-filename)))

(defn part1 [data]
  (let [depths (partition 2 1 (concat (seq [nil]) data))]
    (count (filter depth-increase? depths))))

(defn part2 [data]
  (let [depth-trends (map #(reduce + %) (partition 3 1 data))
        depths (partition 2 1 depth-trends)]
    (count (filter depth-increase? depths))))

(defn -main [& _]
  (println "Part 1" (part1 data))
  (println "Part 2" (part2 data)))
