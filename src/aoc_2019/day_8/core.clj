(ns aoc-2019.day-8.core)

(def input (slurp (clojure.java.io/resource "day8.txt")))
(def width 25)
(def height 6)
(def least-zero-frame (first (sort-by (fn [x] (count (filter #(= % \0) x))) < (partition (* width height) (seq input)))))
(def freqs (frequencies least-zero-frame))
(* (get  freqs \1) (get freqs \2))

(doseq [line (map #(apply str %) (partition width (reduce 
                                                   (fn [top-layer bottom-layer]
                                                     (map #(if (= \2 %1)
                                                             %2
                                                             %1) top-layer bottom-layer)) 
                                                   (partition (* width height) (seq input)))))]
  (println line))

