(ns day-02
  (:require [util]))

(def input-filename "day_02.txt")

(def data (util/load-data input-filename))

;; Part 1
(defn total-for-keyword [keyword data]
  (let [exp (re-pattern (str keyword " \\d+"))
        items (->> data
                   (filter #(re-matches exp %))
                   (map #(util/parse-int (first (re-seq #"\d+" %)))))]
    (reduce + items)))

(defn part1 [data]
  (let [total-forward (total-for-keyword "forward" data)
        total-up (total-for-keyword "up" data)
        total-down (total-for-keyword "down" data)
        depth (- total-down total-up)]
    (println "Forward:" total-forward)
    (println "Up:" total-up)
    (println "Down:" total-down)
    (println "Horizontal position:" total-forward)
    (println "Depth:" depth)
    (println "Answer:" (* depth total-forward))))

;; Part 2
(def aim (atom 0))
(def horizontal-position (atom 0))
(def depth (atom 0))

(defn line-int-value [line]
  (util/parse-int (first (re-seq #"\d+" line))))

(defn dispatch-line-type [line]
  (cond
    (re-matches #"forward \d+" line) :forward
    (re-matches #"up \d+" line) :up
    (re-matches #"down \d+" line) :down))

(defmulti process-line dispatch-line-type)

(defmethod process-line :forward [line]
  (let [forward (line-int-value line)]
    (swap! horizontal-position #(+ forward %))
    (swap! depth #(+ % (* @aim forward)))))

(defmethod process-line :up [line]
  (swap! aim #(- % (line-int-value line))))

(defmethod process-line :down [line]
  (swap! aim #(+ % (line-int-value line))))

(defn part2 [data]
  (map process-line data)
  (println "Aim:" @aim)
  (println "Horizontal position:" @horizontal-position)
  (println "Depth:" @depth)
  (println "Answer:" (* @depth @horizontal-position)))

(defn -main [& _]
  (println "Part 1" (part1 data))
  (println "Part 2" (part2 data)))
