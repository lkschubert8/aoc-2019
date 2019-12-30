(ns aoc-2019.day-4.core)

(defn increasing-digits?
  [num]
  (apply <=  (map #(Integer/parseInt (str %)) (-> num str seq))))

(defn has-duplicate?
  [num]
  (some? (some (comp #(< 1 %) second) (frequencies (map #(Integer/parseInt (str %)) (-> num str seq))))))


(def my-range (range 158126 (+ 1 624574)))


(count (filter #(and (increasing-digits? %) (has-duplicate? %)) my-range))


;;Part 2 
(defn has-double?
  [num]
  (some? (some (comp #(= 2 %) second) (frequencies (map #(Integer/parseInt (str %)) (-> num str seq))))))

(def my-range (range 158126 (+ 1 624574)))


(count (filter #(and (increasing-digits? %) (has-double? %)) my-range))

