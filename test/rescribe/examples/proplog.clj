(ns rescribe.examples.proplog
  (:use midje.sweet)
  (:require [rescribe.system :as sys :refer [defsystem]]
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
;;   - an atom or variable `?X`, `?Y`, etc.
;;   - a conjunction of two sub-formulas `(and <prop> <prop>)`
;;   - a disjunction of two sub-formulas `(or <prop> <prop>)`
;;   - an implication `(==> <prop> <prop>)`
;;   - an equivalence `(<=> <prop> <prop>)`

;; Here is a simple recognizer rewrite system for propositional logic formulas
;; without variables.

(defsystem constant-formula?
  [true-const? true -> true
   false-const? false -> true
   conjunction? (and ?X ?Y) -> true
   disjunction? (or ?X ?Y) -> true
   implication? (==> ?X ?Y) -> true
   equivalence? (<=> ?X ?Y) -> true]
  with (or-else true-const?
                false-const?
                conjunction?
                disjunction?
                implication?
                equivalence?))

(fact "Checking some constant formulas."

      (true-const? 'true) => true
      (true-const? 'false) => nil

      (false-const? 'true) => nil
      (false-const? 'false) => true

      (conjunction? '(and p q)) => true
      (conjunction? '(or p q)) => nil
      (conjunction? 'true) => nil

      (disjunction? '(and p q)) => nil
      (disjunction? '(or p q)) => true
      (disjunction? 'true) => nil

      (implication? '(==> p q)) => true
      (implication? '(<=> p q)) => nil

      (equivalence? '(==> p q)) => nil
      (equivalence? '(<=> p q)) => true

      (constant-formula? 'true) => true
      (constant-formula? '(and p q)) => true
      (constant-formula? '(==> p q)) => true

      (constant-formula? '?X) => nil ;; variable are not (yet) recognizable

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

;; **Remark**: the recognizer does not *yet* check the subformulas

;;}

;;{

;; ## Simplified formulas

;; Here are some simplifications for propositional formulas.
;; This is a directed encoding of well-known simplifying tautologies.

(defsystem simplify
  [;; negation
   simpl-not-true (not true) -> false
   simpl-not-false (not false) -> true
   simpl-not-not (not (not ?X)) -> ?X
   ;; conjunction
   simpl-and-absurd-l (and ?X (not ?X)) -> false
   simpl-and-absurd-r (and (not ?X) ?X) -> false
   simpl-and-true-l (and true ?X) -> ?X
   simpl-and-true-r (and ?X true) -> ?X
   simpl-and-false-l (and false ?X) -> false
   simpl-and-false-r (and ?X false) -> false
   ;; disjunction
   simpl-or-exclude-l (or ?X (not ?X)) -> true
   simpl-or-exclude-r (or (not ?X) ?X) -> true
   simpl-or-true-l (or true ?X) -> true
   simpl-or-true-r (or ?X true) -> true
   simpl-or-false-l (or false ?X) -> ?X
   simpl-or-false-r (or ?X false) -> ?X
   ;; implication
   simpl-impl-refl (==> ?X ?X) -> true
   simpl-impl-true-l (==> true ?X) -> ?X
   simpl-impl-true-r (==> ?X true) -> true
   simpl-impl-false-l (==> false ?X) -> true
   simpl-impl-false-r (==> ?X false) -> ?X
   ;; equivalence
   simpl-equiv-refl (<=> ?X ?X) -> true
   simpl-equiv-true-l (<=> true ?X) -> ?X
   simpl-equiv-true-r (<=> ?X true) -> ?X
   simpl-equiv-false-l (<=> false ?X) -> (not ?X)
   simpl-equiv-false-r (<=> ?X false) -> (not ?X)
   ] with (bottom-up (or-else simpl-not-true
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
                           success)))


(fact "Some simplifications."

      (simpl-not-true '(not true)) => false
      (simplify '(not true)) => false

      (simplify '(or X Y)) => '(or X Y)
      (simpl-and-true-l '(and true (or X Y))) => '(or X Y)
      (simplify '(and true (or X Y))) => '(or X Y)

      (simpl-or-false-r '(or (not Z) false)) => '(not Z)
      (simpl-and-false-l '(and false (not Z))) => false

      (simpl-and-absurd-l '(and false (not Z))) => false

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


(defsystem and-or-form
  [and-or-impl (==> ?X ?Y) -> (or (not ?X) ?Y)
   and-or-equiv (<=> ?X ?Y) -> (and (==> ?X ?Y) (==> ?Y ?X))]
  with (bottom-up (or-else and-or-impl
                           and-or-equiv
                           success)))



(defsystem negation-normal-form
  [nnf-not-not (not (not ?X)) -> ?X
   nnf-morgan-and (not (and ?X ?Y)) -> (or (not ?X) (not ?Y))
   nnf-morgan-or (not (or ?X ?Y)) -> (and (not ?X) (not ?Y))
  ] with (top-down (or-else nnf-not-not
                            nnf-morgan-and
                            nnf-morgan-or
                            success)))


(defn nnf [prop]
  (-> prop
      (and-or-form)
      (negation-normal-form)))

(fact "Some formulas put in NNF."

      (nnf '(==> x (==> (==> x y) y)))
      => '(or (not x) (or (and x (not y)) y)))
