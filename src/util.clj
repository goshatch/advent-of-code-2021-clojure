(ns util
  (:require [clojure.string :refer [split-lines]]
            [clojure.java.io :as io]))

(defn load-data [filename]
  (split-lines (slurp (io/resource filename))))

(defn parse-int [n]
  (Integer/parseInt n 10))
