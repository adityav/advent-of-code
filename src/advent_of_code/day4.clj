(ns advent-of-code.day4
  (:use clojure.test)
  (:use clojure.pprint)
  (:require [clojure.string :as str]
            [clojure.set :as s]
            [pandect.algo.md5 :refer :all]))

;; ideal stocking buffer
(defn min-int-suffix
  [prefix]
  (->> (repeat prefix)
       (map-indexed #(str %2 %1))
       (map (juxt identity md5))
       (filter #(.startsWith (second %) "00000"))
       (first)
       (first)
       (#(subs % (count prefix)))
       (Integer/parseInt)
       ))

(deftest ans1
  (is (= 609043 (min-int-suffix "abcdef"))))
