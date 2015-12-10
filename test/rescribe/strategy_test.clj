
(ns rescribe.strategy-test
  (:use midje.sweet)
  (:require [rescribe.strategy :refer [success
                                       fail
                                       rule
                                       and-then >>
                                       or-else +>]]))

(fact "The `success` strategy is ... successful, and yields identity."
      (success 12) => 12
      (success [2 3 :hello] => [2 3 :hello]))

(fact "The `fail` strategy fails ... no matter what."
      (fail 12) => nil
      (fail [2 3 :hello]) => nil)

(fact "`rule` yields a strategy that applies the rule, or fails."
      (background (around :checks (let [strat (rule '(?X + 0) '?X)] ?form)))
      (strat '(12 + 0)) => 12
      (strat '(12 * 0)) => nil)

(fact "The `and-then` (or `>>`) combinator applies the strategies sequentially."
      ((and-then (rule '(?X + 0) '?X)
                 (rule '(1 * ?X) '?X))
       '((1 * (2 + 0)) + 0)) => '(2 + 0)

      (let [raddzero (rule '(?X + 0) '?X)
            lmulone (rule '(1 * ?Y) '?Y)
            strat (>> raddzero lmulone raddzero)]
        (strat '((1 * (2 + 0)) + 0))) => 2)

(fact "`and-then` fails as soon as one of the sub-strategies fails."
      (let [raddzero (rule '(?X + 0) '?X)
            rmulone (rule '(?Y * 1) '?Y)
            strat (>> raddzero rmulone raddzero)]
        (strat '((1 * (2 + 0)) + 0))) => nil)

(fact "The `or-else` (or `+>`) combinator tries to find a successful substrategy."
      ((or-else (rule '(?X + 0) '?X)
                (rule '(1 * ?Y) '?Y))
       '((1 * (2 + 0)) + 0)) => '(1 * (2 + 0))

      ((+> (rule '(1 * ?Y) '?Y)
           (rule '(?X + 0) '?X))
       '((1 * (2 + 0)) + 0)) => '(1 * (2 + 0)))

(fact "The `or-else` strategies are tried sequentially."
      (background (around :checks 
                          (let [rule1 (rule '((?X + 0) + ?Y) '(?X + ?Y))
                                rule2 (rule '(?X + ?X) '(2 * ?X ))] ?form)))

      ((+> rule1 rule2) '((2 + 0) + (2 + 0))) => '(2 + (2 + 0))

      ((+> rule2 rule1) '((2 + 0) + (2 + 0))) => '(2 * (2 + 0)))

(fact "`or-else` fails iff all the sub-stragies fail."

      ((+> (rule '(?X + 0) '?X)
           (rule '(?Y * 1) '?Y)
           (rule '(1 * ?Z) '?Z))
       '(0 + (1 * (2 + 0)))) => nil)


