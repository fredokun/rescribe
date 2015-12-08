
(ns rescribe.term-test
  (:use midje.sweet)
  (:require [rescribe.term :refer [variable?
                                   vec-term?
                                   seq-term?
                                   assoc-term?
                                   constant?
                                   vars
                                   ground-term?
                                   pattern?
                                   subst]]))

(fact "A variable is a symbol starting with a question mark."
      (variable? '?X) => true
      (variable? '?myvar) => true
      (variable? 'X) => false
      (variable? 12) => false)

(fact "A vector term is ... a vector."
      (vec-term? '(?X ?Y ?Z)) => false
      (vec-term? '[?X ?Y ?Z]) => true
      (vec-term? '?X) => false
      (vec-term? '{:a ?X :b 12}) => false
      (vec-term? "not sequential") => false)

(fact "A sequential term is ... sequential."
      (seq-term? '(?X ?Y ?Z)) => true
      (seq-term? '[?X ?Y ?Z]) => true
      (seq-term? '?X) => false
      (seq-term? '{:a ?X :b 12}) => false
      (seq-term? "not sequential") => false)

(fact "An associative terms is ... associative."
      (assoc-term? {:k '?X :b '?Y :c 12}) => true
      (assoc-term? '(?X ?Y ?Z)) => false
      (assoc-term? '?X) => false
      (assoc-term? 12) => false)

(fact
 "A constant is anything non-nil... but a variable, 
a sequential or an associative term"
 (constant? 12) => true
 (constant? "hello") => true
 (constant? '?X) => false
 (constant? '(?X ?Y)) => false
 (constant? {:a '?X, :b 12}) => false)


(facts "about `vars`."
       (vars '?X) => #{'?X}
       (vars '(?X ?Y ?Z)) => #{'?X '?Y '?Z}
       (vars '(?X (12 ?Y ?Z) ?T)) => #{'?X '?Y '?Z '?T}
       (vars '[?X (12 ?Y ?Z) ?T]) => #{'?X '?Y '?Z '?T}
       (vars '{:a ?X, :b (?Y 12 ?Z) :c ?Z :d 14}) => '#{?X ?Y ?Z}
       (vars 12) => #{}
       (vars '(:a :?b 12 {:a :?X :c 42})) => #{})

(fact
 "A ground term contains no variable."
 (ground-term? 12) => true
 (ground-term? '(:a :b :c 42)) => true
 (ground-term? '?X) => false
 (ground-term? '(:a :b :c { :a ?X } 42)) => false
 (ground-term? '[:a :b :c { :a ?X } 42]) => false)

(fact
 "A pattern is *not* a ground term, it has variable."
 (pattern? 12) => false
 (pattern? '(:a :b :c 42)) => false
 (pattern? '?X) => true
 (pattern? '(:a :b :c { :a ?X } 42)) => true
 (pattern? '[:a :b :c { :a ?X } 42]) => true)

(facts
 "Abount `subst` for term substitutions."
 (background ..s.. =contains=> {'?X 1 '?Y [2 3]})
 (subst '?X ..s..) => 1
 (subst '?Z ..s..) => '?Z
 (subst '(?X ?X ?Y :red ?Z) ..s..) => '(1 1 [2 3] :red ?Z)
 (subst '[?X ?X ?Y :red ?Z] ..s..) => '[1 1 [2 3] :red ?Z])
