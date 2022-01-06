(ns day-04
  (:require [util]
            [clojure.string]))

(def input-filename "day_04.txt")

(def data (util/load-data input-filename))

(defn parse-bingo-numbers
  "Parses the first line of `data` into a sequence of numbers to be drawn over
  the course of the game."
  [data]
  (map #(util/parse-int %)
       (clojure.string/split (first data) #",")))

(defn make-board-cell
  "Build a newly initialised board cell with a value of `val`."
  [val]
  {:value val :marked false})

(defn make-board-row
  "Build a board row representation from raw data (`raw-board-row`)"
  [raw-board-row]
  (let [as-ints (map #(util/parse-int %)
                     (clojure.string/split
                      (clojure.string/trim raw-board-row)
                      #"\s+"))]
    (map make-board-cell as-ints)))

(defn make-board
  "Build a board representation from raw data (`raw-board`)."
  [raw-board]
  (map make-board-row raw-board))

(defn make-boards
  "Build a sequence of board representations from `data`."
  [data]
  (let [boards-data (rest (rest data))
        raw-boards  (partition 5 (filter #(not (= "" %)) boards-data))]
    (map make-board raw-boards)))

(defn row-after-marking
  "Returns an updated version of `row` after number `n` has been marked."
  [row n]
  (map #(if (= n (:value %))
          {:value n :marked true}
          %) row))

(defn board-after-marking
  "Returns an updated version of `board` after number `n` has been marked."
  [board n]
  (map #(row-after-marking % n) board))

(defn boards-after-marking
  "Returns an updated seq of `boards` after number `n` has been marked."
  [boards n]
  (map #(board-after-marking % n) boards))

(defn line-complete?
  "Checks whether `board-line` (which can be a row or a column) qualifies for
  'bingo', i.e. all numbers in the line are marked."
  [board-line]
  (= 5 (count (filter #(% :marked) board-line))))

(defn board-col-at-index
  "Returns the column at index `i` of `board` as a seq of cells."
  [board i]
  (map #(nth % i) board))

(defn board-as-cols
  "Returns a representation of `board` as sets representing columns."
  [board]
  (map #(board-col-at-index board %) (range 0 5)))

(defn board-won?
  "Has the board represented as rows (`board-as-rows`) been won?
  In order to win, at least one line in the board, be it a row or a column, must
  contain only marked numbers."
  [board-as-rows]
  (let [board-as-cols  (board-as-cols board-as-rows)
        full-board     (concat board-as-cols board-as-rows)
        complete-lines (filter line-complete? full-board)]
    (> (count complete-lines) 0)))

(defn any-board-won?
  "Have any of the `boards` been won?"
  [boards]
  (> (count (filter board-won? boards)) 0))

(defn last-board-won?
  "Has a board been won and is it the last remaining one of `boards`?"
  [boards]
  (= (count (filter board-won? boards)) (count boards)))

(defn last-unwon-board
  "Returns the last unwon board in `boards`, or nil if more than one board is
  unwon."
  [boards]
  (let [unwon-boards (filter #(not (board-won? %)) boards)
        unwon-count  (count unwon-boards)]
    (if (= 1 unwon-count)
      (first unwon-boards)
      nil)))

(defn board-unmarked-total
  "Calculate the sum of all unmarked cells of `board`."
  [board]
  (reduce + (map #(:value %) (filter #(= (:marked %) false) (flatten board)))))

(defn calculate-answer
  "Calculate the answer for a puzzle part.
  `n` is a seq of bingo numbers, `b` is a seq of bingo boards, `check-fn` is a
  function that takes a seq of boards and returns true if success conditions are
  true, and `board-select-fn` is a function that returns the relevant winning
  board to perform calculations on."
  [n b check-fn board-select-fn]
  (loop [bingo-numbers n
         boards        b]
    (let [last-unwon     (last-unwon-board boards)
          bingo-number   (first bingo-numbers)
          updated-boards (boards-after-marking boards bingo-number)]
      (if (not (check-fn updated-boards))
        (recur (rest bingo-numbers) updated-boards)
        (let [relevant-board       (if last-unwon
                                     (board-after-marking last-unwon bingo-number)
                                     (board-select-fn updated-boards))
              board-unmarked-total (board-unmarked-total relevant-board)]
          (* bingo-number board-unmarked-total))))))

(defn part1 [bingo-numbers boards]
  (let [answer (calculate-answer
                bingo-numbers
                boards
                any-board-won?
                #(first (filter board-won? %)))]
    (println "Part 1 Answer:" answer)))

(defn part2 [bingo-numbers boards]
  (let [answer (calculate-answer
                bingo-numbers
                boards
                last-board-won?
                nil)]
    (println "Part 2 Answer:" answer)))

(defn -main [& _]
  (let [bingo-numbers (parse-bingo-numbers data)
        boards        (make-boards data)]
    (part1 bingo-numbers boards)
    (part2 bingo-numbers boards)))
