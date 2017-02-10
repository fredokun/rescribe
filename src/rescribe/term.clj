
(ns rescribe.term
  "This namespace is for the representation of the
  rewriting **terms**. 

  Almost any *finite* Clojure data, with the notable exception of `nil` 
  can be  considered as a valid term.

  The term categories are as follows:

    - **term variables** are symbols whose name
  begins with a question mark, e.g. `?X`, `?myvar`, etc.

    - **vector terms** are of the form `[t1 t2 ... tn]` with subterms `t1`, `t2`, ... `tn`.

    - **sequential terms** are of the form `(t1 t2 ... tn)` with subterms `t1`, `t2`, ... `tn`
      (infinite sequences cannot be terms).

    - **associative terms** are of the form `{k1:t1 ... k2:tn}` with keys (constants) `ki`'s associated to subterms `ti`'s.
      This is for maps, etc.

    - **constants** are everything else.

  "
  (:require [clojure.set :as s]
            [rescribe.utils :as u]))

(defn variable?
  "Check if the argument `t` is a term variable."
  [t]
  (symbol? t))

(defn vec-term?
  "The argument `t` is a *vector term* if
  it is a Clojure vector."
  [t] (vector? t))

(defn seq-term?
  "The argument `t` is a *sequential term* if
  it is `sequential?` for Clojure (and not a vector)."
  [t] (sequential? t))

(defn assoc-term?
  "The argument `t` is an *associative term* if
  it is `associative?`for Clojure."
  [t] (associative? t))

(defn constant?
  "The argument `t` is a constant if it is non-`nil`,
 and it is not a variable, a sequential or an associative term."
  [t]
  (and (not (nil? t))
       (not (variable? t))
       (not (vec-term? t))
       (not (assoc-term? t))
       (not (seq-term? t))))

(defn vars
  "Returns the set of variables occurring in term `t`."
  ([t] (vars t '() #{}))
  ([t cont vs]
   (cond
     (variable? t) (if (seq cont)
                     (recur (first cont) (rest cont) (conj vs t))
                     (conj vs t))
     (seq-term? t) (if (seq t)
                     (recur (first t) (cons (rest t) cont) vs)
                     (if (seq cont)
                       (recur (first cont) (rest cont) vs)
                       vs))
     (assoc-term? t) (if (seq t)
                       (recur (second (first t))
                              (cons (dissoc t (first (first t))) cont)
                              vs)
                       (if (seq cont)
                         (recur (first cont) (rest cont) vs)
                         vs))
     :else (if (seq cont)
             (recur (first cont) (rest cont) vs)
             vs))))


(defn ground-term?
  "A term is *ground* if it contains no variable."
  [t] (empty? (vars t)))

(defn pattern?
  "A *pattern* has variable, it is thus *not* a ground term."
  [t] (not (ground-term? t)))

(defn subst
  "Applies substitution `s` (a map from variables to terms)
  to the term `t`."
  {:todo "Write a tail-recursive ?"}
  [t s]
  (u/postwalk
   (fn [v]
     (if (variable? v)
       (if-let [u (s v)]
         u
         v)
       v))
   t))

;; (defn subst
;;   "Applies substitution `s` (a map from variables to terms)
;;   to the term `t`."
;;   {:todo "Write a tail-recursive ?"}
;;   [t s]
;;   (clojure.walk/postwalk
;;    (fn [v]
;;      (if (variable? v)
;;        (if-let [u (s v)]
;;          u
;;          v)
;;        v))
;;    t))

(defn rule-wff?
  "Checks if a rule `[lhs rhs]` is well-formed."
  [lhs rhs] (clojure.set/subset? (vars rhs) (vars lhs)))


