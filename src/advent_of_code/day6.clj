(ns advent-of-code.day6
  (:use clojure.test)
  (:use clojure.pprint)
  (:require [clojure.string :as str]
            [clojure.set :as s]))

;;turn on the lights
;; for part 1 soln
(defn cmd->action1
  [cmd-type]
  (case cmd-type
    "toggle" #(not (or % false))
    "turn on" (constantly true)
    "turn off" (constantly false)
    identity))

;; part 2 soln
(defn cmd->action2
    [cmd-type]
    (case cmd-type
      "toggle" #(+ 2 (or % 0))
      "turn on" #(+ 1 (or % 0))
      "turn off" #(max 0 (dec (or % 0)))
      identity))

;; command parsing
(defn str->cmd
  [cmd-string]
  (let [[blah main-cmd from-x from-y to-x to-y]
        (first (re-seq #"(.+)\s(\d{1,3}),(\d{1,3}) through (\d{1,3}),(\d{1,3})" cmd-string))]
    {:main-cmd main-cmd
     :from [(Integer/parseInt from-x) (Integer/parseInt from-y)]
     :to [(inc (Integer/parseInt to-x)) (inc (Integer/parseInt to-y))]})
  )

(defn coord-stream
  "generate a stream of [x y] coords for the given ranges"
  [[from-x from-y] [to-x to-y]]
  (for [x (range from-x to-x) y (range from-y to-y)] [x y]))

;; bulb map updator. f is applied to bulb-state returns new state
(defn update-bulb
  [bulb-map coord f]
  (update-in bulb-map coord f))

(defn update-bulb-trans
  [bulb-map [x y] f]
  (let [y-vec (get bulb-map x)
        xy-val (get y-vec y)
        new-val (f xy-val)]
    (assoc! bulb-map x (assoc! y-vec y new-val)))
  )

;; applies the f val and update the map with the results.
(defn for-each-bulb
  [bulb-map from to f]
  (reduce #(update-bulb %1 %2 f) bulb-map (coord-stream from to))
  )

(defn num-lit-lights
  [bulb-map]
  (reduce (fn [count coords]
            (if (get-in bulb-map coords) (inc count) count))
          0
          (coord-stream [0 0] [1000 1000])))

(defn total-brightness
  [bulb-map]
  (reduce (fn [count coords]
            (if-let [brightness (get-in bulb-map coords)]
              (+ count brightness) count))
          0
          (coord-stream [0 0] [1000 1000])))


(defn resolve-actions
  "returns a function which updates a command with the correct action to take on bulb map"
  [f]
  (fn [a-cmd] (update a-cmd :main-cmd f)))

;; apparently clojure can't generate transient collections for unboxed vectors
(defn gen-transient-grid
  [cell-type]
  (vec (repeat 1000 (reduce conj (vector-of cell-type) (repeat 1000 nil))))
  ;; (transient (vec (repeat 1000 (reduce conj! (transient []) (repeat 1000 nil)))))
  )

;; using a dic takes 18500 msec
;; using the above transient grid takes 3000 msec
;; using the boolean vector of vectors takes 14000 msec
(defn solve1
  []
  (num-lit-lights (with-open [rdr (clojure.java.io/reader "resources/day6-input.txt")]
                    (->> (line-seq rdr)
                         (map str->cmd)
                         (map (resolve-actions cmd->action1))
                         (reduce (fn [bulb-map {:keys [main-cmd to from]}]
                                   ;; (for-each-bulb bulb-map from to main-cmd)) {}))
                                   (for-each-bulb bulb-map from to main-cmd)) (gen-transient-grid :boolean)))
                    )))
(defn solve2
  []
  (total-brightness
   (with-open [rdr (clojure.java.io/reader "resources/day6-input.txt")]
     (->> (line-seq rdr)
          (map str->cmd)
          (map (resolve-actions cmd->action2))
          (reduce (fn [bulb-map {:keys [main-cmd to from]}]
                    (for-each-bulb bulb-map from to main-cmd)) (gen-transient-grid :int))
          )))
  )
