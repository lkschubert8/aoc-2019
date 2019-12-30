(ns aoc-2019.day-3.core)

(def input (slurp "./inputs/day3.txt"))
(def wires (map #(clojure.string/split % #",") (clojure.string/split-lines input)))


;;This can clearly be cleaned up 
(defn get-line-points
  [start-x start-y line steps]
  (let [dir (subs line 0 1)
        length (Integer/parseInt (subs line 1))]
    (cond
      (= dir "R") {:points (map-indexed #(hash-map :x %1 :y start-y :steps (+ steps %2)) (range start-x (+ start-x length) 1)) 
                   :end {:x (+ start-x length) :y start-y}
                   :steps length}
      (= dir "L") {:points (map-indexed #(hash-map :x %1 :y start-y :steps (+ steps %2)) (range start-x (- start-x length) -1)) 
                   :end {:x (- start-x length) :y start-y}
                   :steps length}
      (= dir "U") {:points (map-indexed #(hash-map :x start-x :y %1 :steps (+ steps %2)) (range start-y (+ start-y length) 1)) 
                   :end {:x start-x :y (+ start-y length)}
                   :steps length}
      (= dir "D") {:points (map-indexed #(hash-map :x start-x :y %1 :steps (+ steps %2)) (range start-y (- start-y length) -1)) 
                   :end {:x start-x :y (- start-y length)}
                   :steps length})))

(defn line-point-reducer
  [{:keys [x y points steps]} line]
  (let [{new-points :points
         end :end
         steps :steps} (get-line-points x y line steps)]
    {:x (:x end) :y (:y end) :points (concat points new-points) :steps steps}))

(defn get-wire-point-set
  [wire]
  (set (:points (reduce  line-point-reducer {:x 0 :y 0 :points [] :steps 0} wire))))

(def first-wire-points (->
                        wires
                        first
                        get-wire-point-set))
(def second-wire-points (->
                        wires
                        second
                        get-wire-point-set))

second-wire-points
(def intersections (clojure.set/intersection first-wire-points second-wire-points))
(def sorted-distances (sort (map #(+ (Math/abs (:x %)) (Math/abs (:y %))) intersections)))
(second sorted-distances)

