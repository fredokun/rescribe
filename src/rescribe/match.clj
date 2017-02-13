
(ns rescribe.match
  "This namespace is for the matching algorithm.
  We use core.match as a backend."
  (:require [clojure.core.match :as m]
            [rescribe.term :refer [variable?
                                   vec-term?
                                   seq-term?
                                   assoc-term?]]))

(defn pattern-compile
  "Compilation of `pattern`."
  [pattern vars]
  ;; (println "[pattern-compile] pattern=" pattern "vars=" vars)
  (cond
    ;; symbol or variable
    (symbol? pattern)
    (if (or (contains? vars pattern)
            (= pattern '&))
      pattern
      (list 'quote pattern))
    ;; vector
    (vec-term? pattern)
    (mapv #(pattern-compile % vars) pattern)
    ;; sequence
    (seq-term? pattern)
    (list (mapv #(pattern-compile % vars) pattern) :seq)
    ;; assoc
    (assoc-term? pattern)
    (into {} (map (fn [[k p]] [k (pattern-compile p vars)]) pattern))
    ;; things considered constants
    :else pattern))

;; (pattern-compile '(0 + x) '#{x})
;; => ([0 (quote +) x] :seq)
;; (pattern-compile '[0 + (0 + x)] '#{x})
;; => [0 (quote +) ([0 (quote +) x] :seq)]
;; (pattern-compile '(0 + x & y) '#{x, y})
;; => ([0 (quote +) x & y] :seq)
;; (pattern-compile '{:a x :b 1 :c y} '#{x, y})
;; => {:a x, :b 1, :c y}
;; (pattern-compile '{:a [x + 0] :b 1 :c y} '#{x, y})
;; => {:a [x (quote +) 0], :b 1, :c y}
;; (pattern-compile '{:a (x + 0) :b 1 :c y} '#{x, y})
;; => {:a ([x (quote +) 0] :seq), :b 1, :c y}

(defn pattern-vars
  "Compute the variables used in `pattern` as a sequence
  (with possible duplicates)."
  [pattern vars]
  (cond
    ;; symbol or variable
    (symbol? pattern)
    (if (contains? vars pattern)
      (list pattern)
      ())
    ;; vector and sequences
    (or (vec-term? pattern)
        (seq-term? pattern))
    (reduce (fn [res p] (concat res (pattern-vars p vars))) () pattern)
    ;; assoc
    (assoc-term? pattern)
    (reduce (fn [res [_ p]] (concat res (pattern-vars p vars))) () pattern)
    ;; things considered constants
    :else ()))

;; (pattern-vars '(0 + x) '#{x})
;; => (x)
;; (pattern-vars '[0 + (0 + x)] '#{x})
;; => (x)
;; (pattern-vars '(0 + x & y) '#{x, y})
;; => (x y)
;; (pattern-vars '{:a x :b 1 :c y} '#{x, y})
;; => (x y)
;; (pattern-vars '{:a [x + 0] :b 1 :c y} '#{x, y})
;; => (x y)
;; (pattern-vars '{:a (x + 0) :b 1 :c y} '#{x, y})
;; => (x y)

(defn pattern-rhs
  "Compute the rhs part of `pattern`."
  [pattern vars]
  (into {} (map (fn [x] [(list 'quote x) x]) (distinct (pattern-vars pattern vars)))))

;; (pattern-rhs '(0 + x) '#{x})
;; => {(quote x) x}
;; (pattern-rhs '[0 + (0 + x)] '#{x})
;; => {(quote x) x}
;; (pattern-rhs '(0 + x & y) '#{x, y})
;; => {(quote x) x, (quote y) y}
;; (pattern-rhs '{:a x :b 1 :c y} '#{x, y})
;; => {(quote x) x, (quote y) y}
;; (pattern-rhs '{:a [x + 0] :b 1 :c y} '#{x, y})
;; => {(quote x) x, (quote y) y}
;; (pattern-rhs '{:a (x + 0) :b 1 :c y} '#{x, y})
;; => {(quote x) x, (quote y) y}

(defn match-compile-clauses
  ([patterns vars] (match-compile-clauses patterns vars 0))
  ([patterns vars id]
   (if (seq patterns)
     (lazy-seq (cons [(pattern-compile (first patterns) vars)]
                     (cons [id (pattern-rhs (first patterns) vars)]
                           (match-compile-clauses (rest patterns) vars (inc id)))))
    ())))

;; (match-compile-clauses '((0 + x)
;;                          [0 + (0 + x)]
;;                          (0 + x & y)
;;                          {:a x :b 1 :c y}
;;                          {:a [x + 0] :b 1 :c y}
;;                          {:a (x + 0) :b 1 :c y}) '#{x, y})
;; =>
;; ([([0 (quote +) x] :seq)]
;;  [0 {(quote x) x}]
;;  [[0 (quote +) ([0 (quote +) x] :seq)]]
;;  [1 {(quote x) x}]
;;  [([0 (quote +) x & y] :seq)]
;;  [2 {(quote x) x, (quote y) y}]
;;  [{:a x, :b 1, :c y}]
;;  [3 {(quote x) x, (quote y) y}]
;;  [{:a [x (quote +) 0], :b 1, :c y}]
;;  [4 {(quote x) x, (quote y) y}]
;;  [{:a ([x (quote +) 0] :seq), :b 1, :c y}]
;;  [5 {(quote x) x, (quote y) y}])

(defn match-compile
  [patterns vars]
  (let [vars vars]
    `(fn [t#] (m/match [t#]
                       ~@(match-compile-clauses patterns vars)
                       :else nil))))

;; (match-compile '([0 + x]
;;                  {:a x, :b 1, :c y}) '#{x, y})
;; =>
;; (clojure.core/fn
;;  [t__33332__auto__]
;;  (clojure.core.match/match
;;   [t__33332__auto__]
;;   [[0 (quote +) x]]
;;   [0 {(quote x) x}]
;;   [{:a x, :b 1, :c y}]
;;   [1 {(quote x) x, (quote y) y}]
;;   :else
;;   nil))

(defmacro match
  "Matches pattern `)p` against term `t`.
  Returns a substitution, a map from
  variables to matched subterms, or `nil` if
  the matching fails."
  [vars patt term]
  `(let [res# (~(match-compile [patt] (into #{} vars)) ~term)]
     (second res#)))



