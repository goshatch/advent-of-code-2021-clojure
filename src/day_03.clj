(ns day-03
  (:require [util]
            [clojure.string]))

(def input-filename "day_03.txt")

(def data (util/load-data input-filename))

(defn value-for-bit-at-index
  "Returns the value of the bit at index n of binary number represented by
  string s"
  [n s]
  (util/parse-int (str (nth s n))))

;; Part 1
(defn value-for-col
  "Returns the value for the column at index `n` as determined by using `f` to
  compare counts of 1 and 0 in each line of the `data` set."
  [f n data]
  (let [all-values (map #(value-for-bit-at-index n %) data)
        one-count (count (filter #(= 1 %) all-values))
        zero-count (count (filter #(= 0 %) all-values))]
    (if (f one-count zero-count) 1 0)))

(defn part1 [data]
  (let [columns-range (range (count (first data)))
        gamma-rate (util/parse-binary (clojure.string/join (map #(value-for-col > % data) columns-range)))
        epsilon-rate (util/parse-binary (clojure.string/join (map #(value-for-col < % data) columns-range)))]
    (println "Part 1 answer:" (* gamma-rate epsilon-rate))))

;; Part 2
(defn filter-data
  "Recursively filters a data set"
  ([data fn]
   (filter-data data fn 0))
  ([data fn index]
   (loop [d data f fn i index]
     (let [col-value (value-for-col f i d)]
       (if (> (count d) 1)
         (recur (filter #(= (value-for-bit-at-index i %) col-value) d) f (inc i))
         (first d))))))

(defn compare-with-default-val-f
  "Wraps the supplied `compare-func` as to provide a default return value
  `default-value` when compared elements are equal."
  [default-value compare-func]
  (fn [a b]
    (if (= a b)
      default-value
      (compare-func a b))))

(defn part2 [data]
  (let [oxygen-generator-rating (util/parse-binary (filter-data data (compare-with-default-val-f true >)))
        co2-scrubber-rating (util/parse-binary (filter-data data (compare-with-default-val-f false <)))]
    (println "Part 2 answer:" (* oxygen-generator-rating co2-scrubber-rating))))

(defn -main [& _]
  (part1 data)
  (part2 data))
