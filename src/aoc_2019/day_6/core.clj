(ns aoc-2019.day-6.core
  (:require [instaparse.core :as insta]))




(def input (clojure.string/split-lines (slurp (clojure.java.io/resource "day6.txt"))))

(def formatted (map #(clojure.string/split % #"\)") input))

; first val will be planet, second will be orbit count 
(def memo (atom {}))

(defn count-orbits
  [input planet]
  (let [orbits (get @memo planet)]
    (if (some? orbits)
      orbits
      (let
       [next-record (filter #(= planet (second %)) input)
        count (if (<= 1 (count next-record))
                (+ 1 (count-orbits input (first (first next-record))))
                0)]
        (swap! memo assoc planet count)
        count))))

(def planets (map second formatted))
(reduce + (map #(count-orbits formatted %) planets))



(def inputtst (clojure.string/split-lines (slurp (clojure.java.io/resource "day6.test.txt"))))

(def formattedtst (map #(clojure.string/split % #"\)") inputtst))
(defn list-orbits
  [input planet]
  (let
   [next-record (filter #(= planet (second %)) input)
    planets (if (<= 1 (count next-record))
              (conj (list-orbits input (first (first next-record))) planet)
              [planet])]
    planets))

(def you-orbits (drop-last 1 (list-orbits formatted "YOU")))
(def san-orbits (drop-last 1 (list-orbits formatted "SAN")))

(def bigger-size (apply max (map count [you-orbits san-orbits])))
(def you-orbits (concat you-orbits (repeat (- bigger-size (count you-orbits)) 0)))
(def san-orbits (concat san-orbits (repeat (- bigger-size (count san-orbits)) 0)))
you-orbits
san-orbits
(reduce + (map #(cond
                  (= %1 %2) 0
                  (= 0 %1) 1
                  (= 0 %2) 1
                  (not= %1 %2) 2
                  :else 0)
               you-orbits
               san-orbits))

