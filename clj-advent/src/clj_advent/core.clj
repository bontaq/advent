(ns clj-advent.core
  (:require [clojure.string :as str]))

(defn parse-elve [elve]
  (map #(Integer/parseInt %) (str/split-lines elve)))

(defn summarized-elves [input]
  (let [elves (str/split input #"\n\n")
        parsed-elves (map parse-elve elves)]
    (map #(reduce + %) parsed-elves)))

(defn part-one []
  (let [input (slurp "./file.txt")
        summary (summarized-elves input)]
    (println (reduce max summary))))

(part-one)

(defn part-two []
  (let [input (slurp "./file.txt")
        summary (summarized-elves input)]
    (println (reduce + (take 3 (reverse (sort summary)))))))

(part-two)
