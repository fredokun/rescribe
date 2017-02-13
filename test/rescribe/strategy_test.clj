
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
      (background (around :checks
                          (let [strat (rule (x + 0) x #{x})] ?form)))
      (strat '(12 + 0)) => 12
      (strat '(12 * 0)) => nil)

(fact "The `and-then` (or `>>`) combinator applies the strategies sequentially."
      ((and-then (rule (x + 0) x #{x})
                 (rule (1 * x) x #{x}))
       '((1 * (2 + 0)) + 0)) => '(2 + 0)

      (let [raddzero (rule (x + 0) x #{x})
            lmulone (rule (1 * y) y #{y})
            strat (>> raddzero lmulone raddzero)]
        (strat '((1 * (2 + 0)) + 0))) => 2)

(fact "`and-then` fails as soon as one of the sub-strategies fails."
      (let [raddzero (rule (x + 0) x #{x})
            rmulone (rule (y * 1) y #{y})
            strat (>> raddzero rmulone raddzero)]
        (strat '((1 * (2 + 0)) + 0))) => nil)

(fact "The `or-else` (or `+>`) combinator tries to find a successful substrategy."
      ((or-else (rule (x + 0) x #{x})
                (rule (1 * y) y #{y}))
       '((1 * (2 + 0)) + 0)) => '(1 * (2 + 0))

      ((+> (rule (1 * y) y #{y})
           (rule (x + 0) x #{x}))
       '((1 * (2 + 0)) + 0)) => '(1 * (2 + 0)))

(let [rule1 (rule ((x + 0) + y) (x + y) #{x y})
      rule2 (rule (x + 0) x #{x})]
  ((+> rule1 rule2) '((2 + 0) + 0))
  ;;((+> rule2 rule1) '((2 + 0) + 0))  
  )


(fact "The `or-else` strategies are tried sequentially."
      (background (around :checks 
                          (let [rule1 (rule ((x + 0) + y) (x + y) #{x y})
                                rule2 (rule (x + 0) x #{x})] ?form)))

      ((+> rule1 rule2) '((2 + 0) + 0)) => '(2 + 0)

      ((+> rule2 rule1) '((2 + 0) + 0)) => '(2 + 0))

(fact "`or-else` fails iff all the sub-stragies fail."

      ((+> (rule (x + 0) x #{x})
           (rule (y * 1) y #{y})
           (rule (1 * z) z #{z}))
       '(0 + (1 * (2 + 0)))) => nil)


