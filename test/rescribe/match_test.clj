
(ns rescribe.match-test
  (:use midje.sweet)
  (:require [rescribe.match :refer [match]]))


(fact "A variable `match`es a value."
      (match '?X 12) => '{?X 12})

(fact "Any new `match` is added to the input
substitution"
      (match '?X 12 '{?Y 42 ?Z 37}) => '{?X 12 ?Y 42 ?Z 37})


(fact "A `match` of an already bound variable must be equal."
      (match '?X 12 '{?Y 42 ?X 12}) => '{?Y 42 ?X 12}
      (match '?X 42 '{?Y 42 ?X 12}) => nil)

(facts "abount sequence `match`."
       (match '(:red ?X 42) '(:red true 42)) => '{?X true}
       (match '(:red ?X 42) '(:green true 42)) => nil
       (match '(?X 42 ?X) '(:green 42 :green)) => '{?X :green}
       (match '(?X 42 ?X) '(:green 42 :blue)) => nil)

(facts "sequence `match` must preserve cardinality."
       (match '(?X ?Y ?Z) '(1 2 3)) => '{?X 1 ?Y 2 ?Z 3}
       (match '(?X ?Y ?Z) '(1 2)) => nil
       (match '(?X ?Y ?Z) '(1 2 3 4)) => nil)

(fact "The `match` of constants is equality."
      (match 42 42) => {}
      (match 42 43) => nil)



