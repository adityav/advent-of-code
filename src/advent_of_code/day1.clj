(ns advent-of-code.day1
  (:use clojure.test)
  (:use clojure.pprint))

;; santa trying to deliver presents.
;; `(` -> means go up one floor
;; `)` -> go down one floor

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

;; (println "Take-While-and-one Test" (take-while-and-one #(not (= 1 %)) '(0 2 1 3 5 6)))

(defn move-santa 
  "find which floor to move santa to. Only moves on ( / ). Otherwise rejects"
  [current-floor direction-char]
  ((if (= direction-char \()
     inc
     (if (= direction-char \) )
       dec
       identity))
   current-floor))

(defn find-floor
  "move santa to correct floor"
  [directions]
  (reduce move-santa 0 (seq directions)))



(defn basement-char-idx
  "returns which character index causes santa to reach basement"
  [directions]
  (let [get-idx (fn [floors]
                  (let [final-floor (last floors)]
                    (if (= final-floor -1) (count floors) -1)))]
    (->> (seq directions)
         (reductions move-santa 0)
         (take-while-and-one #(not (= % -1)))
         ;; ((fn [r] (println "Last: " r) r))
         (get-idx)
         (dec))))

(def input-directions (slurp "resources/day1-input.txt"))
;; find the result of day1, part1
(println "Correct floor to move to is: " (find-floor input-directions))
(println "Reaches basement at: " (basement-char-idx input-directions))
;; TESTS
;; (run-all-tests)
(deftest test1
  (is (= (find-floor "(())") (find-floor "() ()") 0)))
(deftest test2
  (is (= (find-floor "(((") (find-floor "(()(()(") (find-floor "))(((((") 3)))

(deftest test3
  (is (= (basement-char-idx "())(") 3)))
