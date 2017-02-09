(ns rescribe.utils)

(declare continue-walk)
(defn postwalk
  "This is a tail-recursive version of [[clojure.walk/postwalk]]."
  [f form]
  (loop [form form, cont ()]
    ;;(println "[postwalk] form=" form)
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
  ;;(println "[continue-walk] form=" form)
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

