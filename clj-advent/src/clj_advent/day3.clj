(ns clj-advent.day3
  (:require [clojure.string :as str]))

(def lines (str/split-lines (slurp "./day3.txt")))

(defn split [line]
  (split-at (/ (count line) 2) line))
