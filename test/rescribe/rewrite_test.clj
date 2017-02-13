
(ns rescribe.rewrite-test
  (:use midje.sweet)
  (use [midje.util :only [testable-privates]])
  (:require [rescribe.rewrite :as sys :refer [rewrite]]
            [rescribe.strategy :refer [bottom-up or-else success]]))

(def check-rule #'sys/check-rule)

(fact "`check-rule` returns `true` if a rule is well-formed."
      (check-rule 'correct-rule '(?X + 0) '-> '?X) => true)

(fact "`check-rule` raises an error if a rule is not well-formed."
      (check-rule 'incorrect-rule '(?X + 0) '-> '?Y) => (throws Exception))

(fact "`check-rule` raises an error if tre arrow is not present."
      (check-rule 'missing-arrow '(?X + 0) '=> '?X) => (throws Exception))

(def arith-simpl
  (rewrite [x]
   lplus-zero (0 + x) -> x
   rplus-zero (x + 0) -> x
   ltimes-one (1 * x) -> x
   rtimes-one (x * 1) -> x
   :strategy (bottom-up (or-else lplus-zero
                                 rplus-zero
                                 ltimes-one
                                 rtimes-one
                                 success))))

(facts "about `arith-simpl`."
       (arith-simpl '(0 + 2)) => 2
       (arith-simpl '(2 + 0)) => 2
       (arith-simpl '(1 * (2 + (0 * 1)))) => 2
       (arith-simpl '(2 * 2)) => '(2 * 2))




