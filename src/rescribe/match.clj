
(ns rescribe.match
  "This namespace provides the implementation
  of the pattern matching algorithm."
  (:require [rescribe.term :refer [variable?
                                   vec-term?
                                   seq-term?
                                   assoc-term?]]))



(defn match
  "Matches pattern `p` against term `t`.
  Returns a substitution, a map from 
  variables to matched subterms, or `nil` if
  the matching fails."
  {:todo "Write a tail-recursive version (use a zipper ?)."}
  ([p t] (match p t {}))
  ([p t s]
   (if (nil? s)
     s
     (cond
       (variable? p) (if-let [u (s p)]
                       (when (= t u)
                         s)
                       (assoc s p t))
       (vec-term? p) (if (vec-term? t)
                       (if-let [p' (seq p)]
                         (when-let [t' (seq t)]
                           (recur p' t' s))
                         (if (seq t)
                           nil
                           s)))
       (seq-term? p) (when (seq-term? t)
                       (if (seq p)
                         (when (seq t)
                           (recur (rest p) (rest t) (match (first p) (first t) s)))
                         (if (seq t)
                           nil
                           s)))
       (assoc-term? p) (when (assoc-term? t)
                         (if (seq p)
                           (when (seq t)
                             (let [[kp tp] (first p)]
                               (when-let [tt (get t kp)]
                                 (recur (dissoc p kp) (dissoc t kp) (match tp tt s)))))
                           (if (seq t)
                             nil
                             s)))
       :else (when (= p t)
               s)))))


