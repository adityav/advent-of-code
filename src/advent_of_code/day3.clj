(ns advent-of-code.day3
  (:use clojure.test)
  (:use clojure.pprint)
  (:require [clojure.string :as str])
  (:require [clojure.set :as s]))

;;present delivered to grid of houses

;; Post comments

(def day-data (slurp "resources/day3-input.txt"))

(defn move
  [[x y] direction]
  (case direction
    \^ [x (inc y)]
    \v [x (dec y)]
    \> [(inc x) y]
    \< [(dec x) y]
    [x y]))

(defn data->dir-stream
  [data]
  (reductions move [0 0] (seq data)))

(defn visit-houses
  [data]
  (->> data
   data->dir-stream
   (into #{})
   count))

(defn visit-with-robo
  [data]
  (count 
   (reduce s/union
           (map #(->> % data->dir-stream (into #{}))
                (list
                 (take-nth 2 data)
                 (take-nth 2 (rest data)))
                ))))
