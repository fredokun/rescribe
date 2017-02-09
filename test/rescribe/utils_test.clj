(ns rescribe.utils-test
  (:require [rescribe.utils :as u :refer [postwalk]]
            [midje.sweet :as midje :refer [fact]]))


(defn debug-fun [x]
  [:walked x])

(fact "leaves"
      (=  (postwalk debug-fun 42)
          (clojure.walk/postwalk debug-fun 42))
      => true

      (= (postwalk debug-fun "string")
         (clojure.walk/postwalk debug-fun "string"))
      => true)

(fact "flat collections"
      (= (postwalk debug-fun [10 20 30])
         (clojure.walk/postwalk debug-fun [10 20 30]))
      => true
      
      (= (postwalk debug-fun (cons 10 (cons 20 (cons 30 ()))))
         (clojure.walk/postwalk debug-fun (cons 10 (cons 20 (cons 30 ())))))
      => true
      
      (= (postwalk debug-fun (list 10 20 30))
         (clojure.walk/postwalk debug-fun (list 10 20 30)))
      => true)


(defn debug-kv-fun [x]
  (if (vector? x)
    (let [[k v] x]
      (if (vector? k)
        [(second k) v]
        x))
    [:walked x]))

(fact "assoc"
      (= (postwalk debug-kv-fun  {:a 1 :b 2 :c 3})
         (clojure.walk/postwalk debug-kv-fun {:a 1 :b 2 :c 3}))
      => true)

(defrecord Test [a b c])

(fact "record"
      (= (postwalk debug-kv-fun (->Test 1 2 3))
         (clojure.walk/postwalk debug-kv-fun (->Test 1 2 3)))
      => true)

(fact "nested collections"

      (= (postwalk debug-fun [10 [20 30 [40 50] 60] [70 80] 90])
         (clojure.walk/postwalk debug-fun [10 [20 30 [40 50] 60] [70 80] 90]))
      => true)


