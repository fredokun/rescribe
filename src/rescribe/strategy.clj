
(ns rescribe.strategy
  "This namespace defines the rescribe *strategy* language,
  and provides implementation for important high-level
  strategies such as top-down or bottom-up rewriting.

  A strategy is basically a function from a term to
  a rewritten term or nil.
  "
  (:require [rescribe.term :refer [vars subst
                                   vec-term?
                                   seq-term?
                                   assoc-term?
                                   rule-wff?]]
            [rescribe.match :refer [match]]))


(defn success
  "The `success` strategy never fails, and
  always yield the identity term."
  [t] t)


(defn fail
  "A failing strategy, always yielding `nil`."
  [_] nil)


(defn rule
  "This generates a strategy that tries to apply
  the `[lhs rhs]` rule to the provided term `t`."
  [lhs rhs]
  {:pre [(rule-wff? lhs rhs)]}
  (fn [t] (when-let [s (match lhs t)]
            (subst rhs s))))

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
           (if (seq ts)
             (let [t' (s (first ts))]
               (if (nil? t')
                 (recur (rest ts) fail? (conj res (first ts)))
                 (recur (rest ts) false (conj res t'))))
             (if fail?
               nil?
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


