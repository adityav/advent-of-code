(ns advent-of-code.day5
  (:use clojure.test)
  (:use clojure.pprint)
  (:require [clojure.string :as str]
            [clojure.set :as s]))

;; this code can do with serious optimizations.

;; the idea is to have a list of validations, which is run over the test-string
;; each validation is a map, containing
;; :state-info -> current state-info. pass / fail / continue. begins with continue
;; :state -> current state data
;; :next -> which takes the char and current state data => and returns
;;          a tuple containing new stateInfo and state data
;;

(def vowel? (let [vowel-set (set "aeiou")] (partial contains? vowel-set)))
(defn -vowel-validator-fn
  [test-char current-sum]
  (if (vowel? test-char)
    (if (>= (inc current-sum) 3)
      (vector "pass" (inc current-sum))
      (vector "continue" (inc current-sum)))
    (vector "continue" current-sum)))

(defn -twice-validator-fn
  [test-char last-seen]
  (vector  (if (nil? last-seen) "continue"
             (if (= test-char last-seen) "pass" "continue")) test-char))

(defn fail-vtor-on-true [cond] (if cond "fail" "continue"))
(defn -bad-seq-validator-fn
  [test-char last-seen]
  (vector (case last-seen
          nil "continue"
          \a (fail-vtor-on-true  (= test-char \b))
          \c (fail-vtor-on-true  (= test-char \d))
          \p (fail-vtor-on-true  (= test-char \q))
          \x (fail-vtor-on-true  (= test-char \y))
          "continue") test-char))

(def vowel-validator { :state-info "continue"
                       :state 0 ;;current sum
                       :next -vowel-validator-fn})

(def twice-validator {:state-info "continue"
                      :state nil ;; last seen char
                      :next -twice-validator-fn
                      })
(def bad-seq-validator {:state-info "continue"
                        :state nil ;; last seen char
                        :next -bad-seq-validator-fn})

(def empty-vowel-dic (->> (seq "aeiou") (map #(vector % 0)) (into {})))

(defn vtor-failed? [vtor] (= (:state-info vtor) "fail"))
(defn vtor-passed? [vtor] (= (:state-info vtor) "pass"))
(defn vtor-continue? [vtor] (= (:state-info vtor) "continue"))

(defn run-vtor
  "only runs vtor:next if not in pass state. Returns updated vtor"
  [test-char vtor]
  (if (not (vtor-continue? vtor)) vtor
      (let [result ((:next vtor) test-char (:state vtor))]
        (assoc vtor :state-info (first result) :state (second result))
        )))

(defn run-validators
  [validators test-char]
  (let [new-vtors (map #(run-vtor test-char %) validators)]
    (if (some vtor-failed? new-vtors) (reduced new-vtors) new-vtors)))

(def validators (list vowel-validator twice-validator bad-seq-validator))

(defn nice-string?
  ""
  [test-string]
  (let [final-state (reduce run-validators validators (seq test-string))
        [v1 v2 v3] final-state]
    (and (vtor-passed? v1) (vtor-passed? v2) (vtor-continue? v3))
    ;; final-state
  ))

;; finally read file and test all strings

(defn count-nice-strings
  []
  (with-open [rdr (clojure.java.io/reader "resources/day5-input.txt")]
    (reduce #(if (nice-string? %2) (inc %1) %1) 0 (line-seq rdr)))
         ;; (count (filter nice-string? (line-seq rdr))))
  )


(deftest test1
  (is (nice-string? "ugknbfddgicrmopn"))
  (is (nice-string? "aaa"))
  (is (not (nice-string? "jchzalrnumimnmhp")))
  (is (not (nice-string? "haegwjzuvuyypxyu")))
  (is (not (nice-string? "dvszwmarrgswjxmb")))
  )

;; part 2 has no structure. so solving it via regex
(defn new-nice-string?
  [test-string]
  (and
   (re-find #"(\w{2}).*\1" test-string)
   (re-find #"(\w).\1" test-string))
  )

(defn count-new-nice-strings
  []
  (with-open [rdr (clojure.java.io/reader "resources/day5-input.txt")]
    (reduce #(if (new-nice-string? %2) (inc %1) %1) 0 (line-seq rdr)))
  )

(deftest test2
  (is (new-nice-string? "qjhvhtzxzqqjkmpb"))
  (is (new-nice-string? "xxyxx"))
  (is (not (new-nice-string? "uurcxstgmygtbstg")))
  (is (not (new-nice-string? "ieodomkazucvgmuy")))
  )
