
(ns rescribe.system-test
  (:use midje.sweet)
  (use [midje.util :only [testable-privates]])
  (:require [rescribe.system :as sys :refer [defsystem]]
            [rescribe.strategy :refer [bottom-up or-else success]]))

(def rule-to-map #'sys/rule-to-map)

(facts "about `rule-to-map`."
       (rule-to-map 'lplus-zero '(0 + ?X) '-> '?X)
       => [:lplus-zero '['(0 + ?X) '?X]])


(def check-rule #'sys/check-rule)

(fact "`check-rule` returns `true` if a rule is well-formed."
      (check-rule 'correct-rule '(?X + 0) '-> '?X) => true)

(fact "`check-rule` raises an error if a rule is not well-formed."
      (check-rule 'incorrect-rule '(?X + 0) '-> '?Y) => (throws Exception))

(fact "`check-rule` raises an error if tre arrow is not present."
      (check-rule 'missing-arrow '(?X + 0) '=> '?X) => (throws Exception))

(def mk-rule-fun #'sys/mk-rule-fun)

(fact "`mk-rule-fun` generates a rule strategy."
      (first (mk-rule-fun 'lplus-zero '(0 + ?X) '-> '?X))
      => 'clojure.core/defn)


(defsystem arith-simpl
  [lplus-zero (0 + ?X) -> ?X
   rplus-zero (?X + 0) -> ?X
   ltimes-one (1 * ?X) -> ?X
   rtimes-one (?X * 1) -> ?X]
  with (bottom-up (or-else lplus-zero
                           rplus-zero
                           ltimes-one
                           rtimes-one
                           success)))

(facts "about `arith-simpl`."
       (arith-simpl '(0 + 2)) => 2
       (arith-simpl '(2 + 0)) => 2
       (arith-simpl '(1 * (2 + (0 * 1)))) => 2
       (arith-simpl '(2 * 2) => '(2 * 2)))


