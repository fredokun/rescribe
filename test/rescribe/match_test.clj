
(ns rescribe.match-test
  (:use midje.sweet)
  (:require [rescribe.match :refer [match]]))


(fact "A variable `match`es a value."
      (match [x] x 12) => '{x 12})

(facts "about sequence `match`."
       (match [x] (:red x 42) '(:red true 42)) => '{x true}
       (match [x] (:red x 42) '(:green true 42)) => nil
       ;; (match [x] (x 42 x) '(:green 42 :green)) => '{?X :green}
       )

(facts "sequence `match` must preserve cardinality."
       (match [x y z] (x y z) '(1 2 3)) => '{x 1 y 2 z 3}
       (match [x y z] (x y z) '(1 2)) => nil
       (match [x y z] (x y z) '(1 2 3 4)) => nil)

(fact "The `match` of constants is equality."
      (match [] 42 42) => {}
      (match [] 42 43) => nil)



