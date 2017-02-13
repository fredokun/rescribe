(ns rescribe.examples.proplog
  (:use midje.sweet)
  (:require [rescribe.rewrite :as sys :refer [rewrite]]
            [rescribe.strategy :refer [bottom-up top-down or-else success]]))


;;{

;; # Rewrite system example: normal forms in propositional logic

;; This document (and associated Clojure source code) illustrates the
;; definition of rewrite systems in the *rescribe* framework.

;; We manipulate propositional logic formulas to produce :

;; - simplified formulas
;; - negation normal forms (NNF)
;; - disjunctive normal forms (DNF)
;; - conjunctive normal forms (CNF)

;; All these transformations can be explained, thus encoded,
;; as simple rewrite systems.

;;}


;;{

;; ## Propositional logic formulas

;; A formula in (full) propositional logic, is either:

;;   - a constant `true` or  `false`
;;   - an atom or variable `x`, `y`, etc.
;;   - a conjunction of two sub-formulas `(and <prop> <prop>)`
;;   - a disjunction of two sub-formulas `(or <prop> <prop>)`
;;   - an implication `(==> <prop> <prop>)`
;;   - an equivalence `(<=> <prop> <prop>)`

;; Here is a simple recognizer rewrite system for propositional logic formulas
;; without variables.

(def constant-formula?
  (rewrite [x y]
   true-const? true -> true
   false-const? false -> true
   conjunction? (and x y) -> true
   disjunction? (or x y) -> true
   implication? (==> x y) -> true
   equivalence? (<=> x y) -> true))

(fact "Checking some constant formulas."

      (constant-formula? 'true) => true
      (constant-formula? '(and p q)) => true
      (constant-formula? '(==> p q)) => true

      (constant-formula? 'x) => nil ;; variable are not (yet) recognizable

      )


;; And this is the way we recognize full formulas.

(defn variable? [v]
  (and (symbol? v)
       (not (contains? '#{true false and or not ==> <=>} v))))

(defn formula? [v]
  (or (symbol? v)
      (constant-formula? v)))

(fact "Checking some formulas."

      (formula? 'true) => true
      (formula? '(and p q)) => true
      (formula? '(==> p q)) => true

      (formula? 'X) => true

      (formula? '(> p q)) => nil
      )

;; **Remark**: the recognizer does not check the subformulas

;;}

;;{

;; ## Simplified formulas

;; Here are some simplifications for propositional formulas.
;; This is a directed encoding of well-known simplifying tautologies.

(def simplify
  (rewrite [x]
   ;; negation
   simpl-not-true (not true) -> false
   simpl-not-false (not false) -> true
   simpl-not-not (not (not x)) -> x
   ;; conjunction
   simpl-and-absurd-l (and x (not x)) -> false
   simpl-and-absurd-r (and (not x) x) -> false
   simpl-and-true-l (and true x) -> x
   simpl-and-true-r (and x true) -> x
   simpl-and-false-l (and false x) -> false
   simpl-and-false-r (and x false) -> false
   ;; disjunction
   simpl-or-exclude-l (or x (not x)) -> true
   simpl-or-exclude-r (or (not x) x) -> true
   simpl-or-true-l (or true x) -> true
   simpl-or-true-r (or x true) -> true
   simpl-or-false-l (or false x) -> x
   simpl-or-false-r (or x false) -> x
   ;; implication
   simpl-impl-refl (==> x x) -> true
   simpl-impl-true-l (==> true x) -> x
   simpl-impl-true-r (==> x true) -> true
   simpl-impl-false-l (==> false x) -> true
   simpl-impl-false-r (==> x false) -> (not x)
   ;; equivalence
   simpl-equiv-refl (<=> x x) -> true
   simpl-equiv-true-l (<=> true x) -> x
   simpl-equiv-true-r (<=> x true) -> x
   simpl-equiv-false-l (<=> false x) -> (not x)
   simpl-equiv-false-r (<=> x false) -> (not x)
   :strategy (bottom-up (or-else simpl-not-true
                                 simpl-not-false
                                 simpl-not-not
                                 simpl-and-absurd-l
                                 simpl-and-absurd-r
                                 simpl-and-true-l
                                 simpl-and-true-r
                                 simpl-and-false-l
                                 simpl-and-false-r
                                 simpl-or-exclude-l
                                 simpl-or-exclude-r
                                 simpl-or-true-l
                                 simpl-or-true-r
                                 simpl-or-false-l
                                 simpl-or-false-r
                                 simpl-impl-refl
                                 simpl-impl-true-l
                                 simpl-impl-true-r
                                 simpl-impl-false-l
                                 simpl-impl-false-r
                                 simpl-equiv-refl
                                 simpl-equiv-true-l
                                 simpl-equiv-true-r
                                 simpl-equiv-false-l
                                 simpl-equiv-false-r
                                 success))))


(fact "Some simplifications."

      (simplify '(not true)) => false

      (simplify '(or X Y)) => '(or X Y)
      (simplify '(and true (or X Y))) => '(or X Y)

      (simplify '(and false (or (not Z) (not true)))) => false
      )

;;{

;; ## Negation normal forms (NNF)

;; The following rewrite system put a formula in negation normal form (NNF)

;; The formula is then guaranteed to contains only:

;; - positive literals: `X`, `Y`, etc.
;; - negative literals: `(not X)`, `(not Y)`, etc.
;; - conjunctions: `(and <nnf> <nnf>)`
;; - disjunctions: `(or <nnf> <nnf>)`


(def and-or-form
  (rewrite
   and-or-impl (==> x y) -> (or (not x) y)
   and-or-equiv (<=> x y) -> (and (==> x y) (==> y x))
  :strategy (bottom-up (or-else and-or-impl
                                and-or-equiv
                                success))))


(def negation-normal-form
  (rewrite
   nnf-not-not (not (not x)) -> x
   nnf-morgan-and (not (and x y)) -> (or (not x) (not y))
   nnf-morgan-or (not (or x y)) -> (and (not x) (not y))
   :strategy (top-down (or-else nnf-not-not
                                nnf-morgan-and
                                nnf-morgan-or
                                success))))

(defn nnf [prop]
  (-> prop
      (and-or-form)
      (negation-normal-form)))

(fact "Some formulas put in NNF."

      (nnf '(==> x (==> (==> x y) y)))
      => '(or (not x) (or (and x (not y)) y)))

;;{

;; ## Conjunctive normal forms

;;}

(def conjunctive-normal-form
  (rewrite
   cnf-not-not (not (not x)) -> x
   cnf-morgan-and (not (and x y)) -> (or (not x) (not y))
   cnf-morgan-or (not (or x y)) -> (and (not x) (not y))
   cnf-distrib-l (or (and x y) ?Z) -> (and (or x ?Z) (or y ?Z))
   cnf-distrib-r (or x (and y ?Z)) -> (and (or x y) (or x y))
   :strategy (top-down (or-else cnf-not-not
                                cnf-morgan-and
                                cnf-morgan-or
                                cnf-distrib-l
                                cnf-distrib-r
                                success))))

