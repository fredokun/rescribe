
(ns rescribe.strategy
  "This namespace defines the rescribe *strategy* language,
  and provides implementation for important high-level
  strategies such as top-down or bottom-up rewriting.

  A strategy is basically a function from a term to
  a rewritten term or nil.
  "
  (:require [rescribe.term :refer [mk-var vars subst
                                   vec-term?
                                   seq-term?
                                   assoc-term?
                                   rule-wff?]]
            [rescribe.match :refer [match-compile]]))


(defn success
  "The `success` strategy never fails, and
  always yield the identity term."
  [t] t)


(defn fail
  "A failing strategy, always yielding `nil`."
  [_] nil)


(defn rule-expander
  [lhs rhs vars]
  (let [matcher (match-compile [lhs] vars)]
    `(fn [t#] (when-let [s# (~matcher t#)]
                ;; (println "[rule] matches: subst=" s#)
                (subst (quote ~rhs) (second s#))))))

(rule-expander '(0 + x) 'x '#{x})

(defmacro rule
  "This generates a strategy that tries to apply
  the `[lhs rhs]` rule to the provided term `t`."
  [lhs rhs vars]
  `~(rule-expander lhs rhs vars))

(macroexpand '(rule (0 + x) x #{x}))

((rule (0 + x) x #{x}) '(0 + 2))
((rule (0 + x) x #{x}) '(0 * 2))

(defn rules-expander
  [rules vars]
  (let [matcher (match-compile (map first rules) vars)]
    `(let [rhs# (quote ~(mapv (fn [x] (second x)) rules))]
       (fn [t#] (when-let [s# (~matcher t#)]
                  ;; (println "[rules] matches: subst=" s#)
                  (subst (nth rhs# (first s#)) (second s#)))))))

(rules-expander '([(0 + x) x]
                  [(x + 0) x]
                  [(1 * x) x]
                  [(x * 1) x]) '#{x})

(defmacro rules
  [rules vars]
  `~(rules-expander rules vars))

(macroexpand-1 '(rules ([(0 + x) x]
                        [(x + 0) x]
                        [(1 * x) x]
                        [(x * 1) x]) #{x}))

(let [f (rules ([(0 + x) x]
                [(x + 0) x]
                [(1 * x) x]
                [(x * 1) x]) #{x})]
  [(f '(0 + 2))
   (f '(0 + (1 * 2)))
   (f '(1 + (1 * 2)))])

(defn and-then
  "A strategy that is a success iff the sub-strategies
  can be all applied sequentially. If any of the argument
  strategies fails, then the whole strategy also fails.
  It is a success if there is no sub-strategy that applies."
  ([] success)
  ([s & more]
   (fn [t] (loop [strats (cons s more), res t]
             (if (seq strats)
               (let [res' ((first strats) res)]
                 (if (nil? res')
                   nil
                   (recur (rest strats) res')))
               res)))))

(def >> and-then)

(defn or-else
  "A strategy that is a success iff any of the sub-strategies
  can be all applied sequentially. If any of the argument
  strategy is successful, then the whole strategy also is a success.
  It is a failure if there is no sub-strategy that applies."
  ([] fail)
  ([s & more]
   (fn [t] (loop [strats (cons s more)]
             (if (seq strats)
               (let [res ((first strats) t)]
                 (if (nil? res)
                   (recur (rest strats))
                   res))
               nil)))))

(def +> or-else)

(defn- apply-all
  "Applies strategy `s` to the sequence of terms `ts`,
  yielding a vector of rewritten terms in case of success,
  or `nil` if `s` fails for any element of `ts`."
  [s ts]
  (reduce (fn [ts' t]
            (let [t' (s t)]
              (if (nil? t')
                (reduced nil)
                (conj ts' t')))) [] ts))

(defn all-sub
  "A strategy that applies `s` to all the subterms of a given
  term, and is a success iff `s` is a success for all the subterms.
  If the term is not compound, the strategy is a success and yields
  the identity."
  [s]
  (fn [t] (cond
            (vec-term? t) (apply-all s t)
            (seq-term? t) (let [ts (apply-all s t)]
                            (if (nil? ts)
                              nil
                              (seq ts)))
            (assoc-term? t) (let [vals' (apply-all s (vals t))]
                              (if (nil? vals')
                                nil
                                (zipmap (keys t) vals')))
            :else t)))

(defn- apply-some
  "Applies strategy `s` to the sequence of terms `ts`,
  yielding a vector of potentially rewritten terms. 
  The strategy fails if `s` fails for all the elements of `ts`."
  [s ts] (loop [ts ts, fail? true, res []]
           ;; (println "[apply-some] ts=" ts "fail?=" fail? "res=" res)
           (if (seq ts)
             (let [t' (s (first ts))]
               (if (nil? t')
                 (recur (rest ts) fail? (conj res (first ts)))
                 (recur (rest ts) false (conj res t'))))
             (if fail?
               nil
               res))))

(defn some-sub
  "A strategy that applies `s` to all the subterms of a given
  term, and is a success iff `s` is a success for at least one of the subterms.
  It is thus a failure of `s` fails for all the subterms.
  If the term is not compound, the strategy is a success and yields
  the identity."
  [s]
  (fn [t] (cond
            (vec-term? t) (apply-some s t)
            (seq-term? t) (let [ts (apply-some s t)]
                            ;; (println "[some-sub]: ts=" ts)
                            (if (nil? ts)
                              nil
                              (seq ts)))
            (assoc-term? t) (let [vals' (apply-some s (vals t))]
                              (if (nil? vals')
                                nil
                                (zipmap (keys t) vals')))
            :else t)))


(defn bottom-up
  "Recursive bottom-up rewriting strategy."
  [s] (fn [t]
        (let [rec-s (>> (some-sub (bottom-up s))
                        s)]
          (rec-s t))))


(defn top-down
  "Recursive top-down rewriting strategy."
  [s] (fn [t]
        (let [rec-s (>> s
                        (some-sub (top-down s)))]
          (rec-s t))))


