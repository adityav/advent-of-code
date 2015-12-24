(ns advent-of-code.day2
  (:use clojure.test)
  (:use clojure.pprint)
  (:require [clojure.string :as str]))

;; wrapping paper

(defn take-while-and-one
  "like take-while but contains the last elem"
  [pred coll]
  (when-let [s (seq coll)]
    (if (pred (first s))
      (cons (first s) (take-while-and-one pred (rest s)))
      (list (first s)))))

(defn pprint-ret
  "pprints and returns value"
  [res]
  (let [r res]
    (pprint r)
    r))

(def box-dim-filedata (slurp "resources/day2-input.txt"))

(defn wrapping-paper-per-present
  [[l w h]]
  (let [sides (list (* l w) (* w h) (* l h))
        area (reduce + 0 (map #(* 2 %) sides))]
    (+ (apply min sides) area)))

(defn dimstring->dimlist
  [line-item]
  (map #(Integer. %)
       (-> line-item str/trim (str/split #"x"))))

(defn read-dimdata-from-file
  [rawdata]
  (->> rawdata
       (str/split-lines)
       (map dimstring->dimlist)))

(defn wrapping-paper-reqd
  "calculate the amount of paper required"
  [filedata]
  (->> filedata
       read-dimdata-from-file
       (map  wrapping-paper-per-present)
       (reduce +)))
;;; day2 part 1 answer
;; (wrapping-paper-reqd box-dim-filedata)

(defn ribbon-needed-per-present
  [[l w h]]
  (let [perimeters (map #(* 2 %) (list (+ l w) (+ w h) (+ l h)))
        volume (* l w h)]
    (+ (apply min perimeters) volume)))

(defn total-ribbon-req
  [filedata]
  (->> filedata
       read-dimdata-from-file
       (map ribbon-needed-per-present)
       (reduce +)))

;;; day 2 part 2 answer
(total-ribbon-req box-dim-filedata)

;;; TESTS
(deftest test1
  (is (= 58 (wrapping-paper-per-present '(2 3 4))))
  (is (= 43 (wrapping-paper-per-present '(1 1 10)))))
(deftest test1
  (is (= 34 (ribbon-needed-per-present '(2 3 4))))
  (is (= 14 (ribbon-needed-per-present '(1 1 10)))))
