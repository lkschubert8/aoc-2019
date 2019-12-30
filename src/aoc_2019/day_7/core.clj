(ns aoc-2019.day-7.core
  (:require
   [clojure.tools.logging :as log]
   [clojure.math.combinatorics :as combo]))

(defn read-stream [stream] (if (empty? @stream)
                             (do
                               (println (str stream " empty. Please provide value"))
                               (Integer/parseInt (read-line)))
                             (ffirst (swap-vals! stream pop))))
(defn write-stream [stream val] (swap! stream conj val))
(defn get-digit )

(defn intcode-computer
  [program input-epochs]
  (let [std-in (atom [])
        std-out (atom [])
        op-table {1 {:name "add"
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
                           {:val (read-stream std-in)})}
                  4 {:name "output"
                     :arity 1
                     :output 0
                     :op (fn [a]
                           (write-stream std-out a)
                           ())}
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
                           {:val (if (= a b) 1 0)})}}]
    (for [input input-epochs]
      (do (swap! std-in (fn [x] concat input @std-out))
          (log/info "std-in " @std-in)
          (swap! std-out (fn [x] []))
          (let [internal-state (atom program)]
            (loop [index 0]
              (if (= 99 (nth @internal-state index))
                @std-out ;; we've hit the halt code 
                (let [fullcode (nth @internal-state index)
                      _ (log/info "Fullcode : " fullcode)
                      parameter-modes (quot fullcode 100)
                      op-code (- fullcode (* parameter-modes 100))
                      op-entry (get op-table op-code)
                      arity (:arity op-entry)
                      has-output (= 1 (:output op-entry))
                      _ (log/info "Arity : " arity " Index " index " Parameter modes " parameter-modes)
                      inputs (map #(let [param-val (nth @internal-state (+ index 1 %))]
                                     (if (= 1 (get-digit parameter-modes %))
                                       param-val ;; immediate mode
                                       (nth @internal-state param-val) ;; position mode
                                       ))
                                  (range 0 arity))
                      _ (log/info inputs)]
                  (let [{outval :val new-pc :pc} (apply (:op op-entry) inputs)]
                    (if (some? outval)
                      (swap! internal-state
                             assoc
                             (nth @internal-state (+ 1 index arity))
                             outval)
                      ())
                    (recur (if (some? new-pc)
                             new-pc
                             (+ index 1 arity (if has-output 1 0)))))))))))))







(def input-permuations (combo/permutations [1 2 3 4 0]))

(def input (slurp (clojure.java.io/resource "day7.txt")))
(def values (into [] (map #(Integer/parseInt %) (clojure.string/split input #","))))

(intcode-computer values [[4][3][2][1][0]])