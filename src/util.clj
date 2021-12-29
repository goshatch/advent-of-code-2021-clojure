(ns util
  (:require [clojure.string :refer [split-lines]]
            [clojure.java.io :as io]))

(defn load-data
  "Load data from filename into a sequence, split by newline."
  [filename]
  (split-lines (slurp (io/resource filename))))

(defn parse-int
  "Parses n, a string containing a number, to a base-10 integer."
  [n]
  (Integer/parseInt n 10))

(defn parse-binary
  "Parses n, a string containing a binary number, to a base-10 integer."
  [n]
  (Integer/parseInt n 2))
