(ns rescribe.utils)

(declare continue-walk)
(defn postwalk
  "This is a tail-recursive version of [[clojure.walk/postwalk]]."
  [f form]
  (loop [form form, cont ()]
    (println "[postwalk] form=" form)
    (if (nil? cont)
      ;;; ***** finished *****
      form
      (cond
        ;; list
        (list? form)
        (if (seq form)
          (recur (first form)
                 (cons (list :list (rest form) [])
                       cont))
          (let [[form', cont'] (continue-walk f form cont)]
            (recur form' cont')))
        ;; map-entry (?)
        (instance? clojure.lang.IMapEntry form)
        (if (seq form)
          (recur (first form)
                 (cons (list :map-entry (rest form) [])
                       cont))
          (let [[form', cont'] (continue-walk f form cont)]
            (recur form' cont')))
        ;; seq
        (seq? form)
        (if (seq form)
          (recur (first form)
                 (cons (list :seq (rest form) [])
                       cont))
          (let [[form', cont'] (continue-walk f form cont)]
            (recur form' cont')))
        ;; records
        (instance? clojure.lang.IRecord form)
        (if (seq form)
          (recur (first form)
                 (cons (list :record (rest form) form)
                       cont))
          (let [[form', cont'] (continue-walk f form cont)]
            (recur form' cont')))
        ;; other collections
        (coll? form)
        (if (seq form)
          (recur (first form)
                 (cons (list :coll (rest form) (empty form) [])
                       cont))
          (let [[form', cont'] (continue-walk f form cont)]
            (recur form' cont')))
        ;; no inner
        :else (let [[form', cont'] (continue-walk f form cont)]
                (recur form' cont'))))))

(defn continue-walk [f form cont]
  (println "[continue-walk] form=" form)
  (let [form (f form)]
    (if (seq cont)
      (case (ffirst cont)
        :list
        (let [[_ rst res] (first cont)]
          (if (seq rst)
            [(first rst)
             (cons (list :list (rest rst) (conj res form))
                   (rest cont))]
            (recur f (apply list (conj res form)) (rest cont))))
        :map-entry
        (let [[_ rst res] (first cont)]
          (if (seq rst)
            [(first rst)
             (cons (list :map-entry (rest rst) (conj res form))
                   (rest cont))]
            (recur f (conj res form) (rest cont))))
        :seq
        (let [[_ rst res] (first cont)]
          (if (seq rst)
            [(first rst)
             (cons (list :seq (rest rst) (conj res form))
                   (rest cont))]
            (recur f (seq (conj res form)) (rest cont))))
        :record
        (let [[_ rst res] (first cont)]
          (if (seq rst)
            [(first rst)
             (cons (list :record (rest rst) (conj res form))
                   (rest cont))]
            (recur f (conj res form) (rest cont))))
        :coll
        (let [[_ rst coll res] (first cont)]
          (if (seq rst)
            [(first rst)
             (cons (list :coll (rest rst) coll (conj res form))
                   (rest cont))]
            (recur f (into coll (conj res form)) (rest cont))))
        (throw (ex-info "No such walk mode" {:walk-mode (ffirst cont)})))
      ;; no continuation
      [form nil])))

(defn debug-fun [x]
  [:walked x])

;; leaves
(postwalk debug-fun 42)
(clojure.walk/postwalk debug-fun 42)

(postwalk debug-fun "string")
(clojure.walk/postwalk debug-fun "string")


;; flat collections
(postwalk debug-fun [10 20 30])
(clojure.walk/postwalk debug-fun [10 20 30])

(postwalk debug-fun (cons 10 (cons 20 (cons 30 ()))))
(clojure.walk/postwalk debug-fun (cons 10 (cons 20 (cons 30 ()))))

(postwalk debug-fun (list 10 20 30))
(clojure.walk/walk debug-inner debug-outer (list 10 20 30))

(defn debug-kv-fun [x]
  (if (vector? x)
    (let [[k v] x]
      (if (vector? k)
        [(second k) v]
        x))
    [:walked x]))

(postwalk debug-kv-fun  {:a 1 :b 2 :c 3})
(clojure.walk/postwalk debug-kv-fun {:a 1 :b 2 :c 3})

(defrecord Test [a b c])

(postwalk debug-kv-fun (->Test 1 2 3))
(clojure.walk/postwalk debug-kv-fun (->Test 1 2 3))

;; nested collections

(postwalk debug-fun [10 [20 30 [40 50] 60] [70 80] 90])

(clojure.walk/postwalk debug-fun [10 [20 30 [40 50] 60] [70 80] 90])
