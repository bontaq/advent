(ns clj-advent.day2
  (:require [clojure.string :as str]))

(def scores {"A" 1 "B" 2 "C" 3 "X" 1 "Y" 2 "Z" 3})
(def outcome-scores {"lose" 0 "tie" 3 "win" 6})

;; X means you need to lose,
;; Y means you need to end the round in a draw, and
;; Z means you need to win. Good luck!


(defn winner [args]
  (case args
    ["A" "X"] "tie"
    ["A" "Y"] "win"
    ["A" "Z"] "lose"

    ["B" "X"] "lose"
    ["B" "Y"] "tie"
    ["B" "Z"] "win"

    ["C" "X"] "win"
    ["C" "Y"] "lose"
    ["C" "Z"] "tie"))

(defn move [args]
  (case args
    ["A" "X"] "Z"
    ["A" "Y"] "X"
    ["A" "Z"] "Y"

    ["B" "X"] "X"
    ["B" "Y"] "Y"
    ["B" "Z"] "Z"

    ["C" "X"] "Y"
    ["C" "Y"] "Z"
    ["C" "Z"] "X"))

(defn score [[theirs yours]]
  (let [outcome (winner [theirs yours])
        out-score (get outcome-scores outcome)

        points-for-choice (get scores yours)]
    (+ out-score points-for-choice)))

(defn part-one []
  (let [plays (map #(str/split % #" ") (str/split-lines (slurp "./day2.txt")))]
    (println (reduce + (map score plays)))))

(part-one)

(defn part-two []
  (let [plays (map #(str/split % #" ") (str/split-lines (slurp "./day2.txt")))
        new-plays (map assign-new-move plays)]
    (println (reduce + (map score new-plays)))))
