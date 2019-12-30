(ns aoc-2019.day-5.core
  (:require
   [clojure.tools.logging :as log]))


(def op-table
  {1 {:name "add"
      :arity 2
      :output 1
      :op (fn [a b]
            {:val (+ a b)})}
   2 {:name "multiply"
      :arity 2
      :output 1
      :op (fn [a b]
            {:val (* a b)})}
   3 {:name "input"
      :arity 0
      :output 1
      :op (fn []
            (println "Please enter an integer:")
            {:val (Integer/parseInt (read-line))})}
   4 {:name "output"
      :arity 1
      :output 0
      :op (fn [a] (println "Output: " a))}
   5 {:name "jump if true"
      :arity 2
      :output 0
      :op (fn [a b]
            (if (= 0 a)
              ()
              {:pc b}))}
   6 {:name "jump if false"
      :arity 2
      :output 0
      :op (fn [a b]
            (if (= 0 a)
              {:pc b}
              ()))}
   7 {:name "less than"
      :arity 2
      :output 1
      :op (fn [a b]
            {:val (if (< a b) 1 0)})}
   8 {:name "equals"
      :arity 2
      :output 1
      :op (fn [a b]
            {:val (if (= a b) 1 0)})}})


(defn get-digit [val index]
  (let [val-str (reverse (str val))]
    (log/info "get digit index " index)
    (if (< (count val-str) (+ 1 index))
      0
      (Integer/parseInt (str (nth val-str index))))))

(defn process
  [initial-state]
  (let [internal-state (atom initial-state)]
    (loop [index 0]
      (if (= 99 (nth @internal-state index))
        @internal-state ;; we've hit the halt code 
        (let [fullcode (nth @internal-state index)
              parameter-modes (quot fullcode 100)
              op-code (- fullcode (* parameter-modes 100))
              op-entry (get op-table op-code)
              arity (:arity op-entry)
              has-output (= 1 (:output op-entry))
              inputs (map #(let [param-val (nth @internal-state (+ index 1 %))]
                             (if (= 1 (get-digit parameter-modes %))
                               param-val ;; immediate mode
                               (nth @internal-state param-val) ;; position mode
                               ))
                          (range 0 arity))]
          (let [{outval :val new-pc :pc} (apply (:op op-entry) inputs)]
            (if has-output
              (swap! internal-state
                     assoc
                     (nth @internal-state (+ 1 index arity))
                     outval)
              ())
            (recur (if (some? new-pc)
                     new-pc
                     (+ index 1 arity (if has-output 1 0))))))))))

(process [3,0,4,0,99])

(map #() (range 0 1))

(def input (slurp (clojure.java.io/resource "day5.txt")))
(def values (into [] (map #(Integer/parseInt %) (clojure.string/split input #","))))
(process values)