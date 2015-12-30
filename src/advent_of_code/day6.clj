(ns advent-of-code.day6
  (:use clojure.test)
  (:use clojure.pprint)
  (:require [clojure.string :as str]
            [clojure.set :as s]))

;;turn on the lights


(defn turn-on  [bulb-state] true)
(defn turn-off [bulb-state] false)
(defn toggle [bulb-state] (not (or bulb-state false)))

(defn cmd->str1
  [cmd-type]
  (case cmd-type
    "toggle" toggle
    "turn on" turn-on
    "turn off" turn-off
    identity))

(defn cmd->str2
    [cmd-type]
    (case cmd-type
      "toggle" (fn [state] (+ 2 (or state 0)))
      "turn on" (fn [state] (inc (or state 0)))
      "turn off" (fn [state] (max 0 (dec (or state 0))))
      identity))

;; command parsing
(defn str->cmd
  [cmd-string]
  (let [[blah main-cmd from-x from-y to-x to-y]
        (first (re-seq #"(.+)\s(\d{1,3}),(\d{1,3}) through (\d{1,3}),(\d{1,3})" cmd-string))]
    {:main-cmd (cmd->str2 main-cmd)
     :from [(Integer/parseInt from-x) (Integer/parseInt from-y)]
     :to [(inc (Integer/parseInt to-x)) (inc (Integer/parseInt to-y))]})
  )

;; bulb map updator. f is applied to bulb-state returns new state
(defn update-bulb
  [bulb-map x y f]
  (update-in bulb-map [x y] f))
;; applies the f val and update the map with the results.
(defn for-each-bulb
  [bulb-map [from-x from-y] [to-x to-y] f]
  (reduce
   #(update-bulb %1 (first %2) (second %2) f) bulb-map
   (for [x (range from-x to-x) y (range from-y to-y)] [x y]))
  )

(defn num-lit-lights
  [bulb-map]
  (reduce (fn [count coords]
            (if (get-in bulb-map coords) (inc count) count)) 0
          (for [x (range 0 1000) y (range 0 1000)] [x y]))
  )

(defn total-brightness
  [bulb-map]
  (reduce (fn [count coords]
            (if-let [brightness (get-in bulb-map coords)]
              (+ count brightness) count)) 0
          (for [x (range 0 1000) y (range 0 1000)] [x y])
          ))

(defn solve1
  []
  (num-lit-lights (with-open [rdr (clojure.java.io/reader "resources/day6-input.txt")]
                    (->> (line-seq rdr)
                         (map str->cmd)
                         (reduce (fn [bulb-map {:keys [main-cmd to from]}]
                                   (for-each-bulb bulb-map from to main-cmd)) {}))
                    )))
(defn solve2
  []
  (total-brightness
   (with-open [rdr (clojure.java.io/reader "resources/day6-input.txt")]
     (->> (line-seq rdr)
          (map str->cmd)
          (reduce (fn [bulb-map {:keys [main-cmd to from]}]
                    (for-each-bulb bulb-map from to main-cmd)) {})
          )))
  )
