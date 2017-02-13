(ns rescribe.rewrite
  "This namespace defines the `rewrite` macro that
 is the principal mean of encoding rewrite systems
  in *rescribe*."
  (:require [rescribe.term :refer [variable?
                                   seq-term?
                                   vec-term?
                                   assoc-term?
                                   subst
                                   rule-wff?]]
            [rescribe.strategy :as s :refer [or-else]]
            [rescribe.match :refer [match]]))

(defn- check-rule
  [rname lhs mid rhs]
  ;;(when (not (rule-wff? lhs rhs))
  ;;(throw (ex-info "Malformed rule." {:rule-name rname :lhs lhs :rhs rhs})))
  (when (not (= (name mid) "->"))
    (throw (ex-info "Missing arrow -> in rule." {:rule-name rname})))
  true)

(defn parse-rule [rulemap name lhs mid rhs]
  (check-rule name lhs mid rhs)
  (when (get rulemap name)
    (throw (ex-info "Duplicate rule name" {:rule-name name})))
  (assoc rulemap name [lhs rhs]))

(defn gen-fresh [rulemap fresh]
  (loop [candidate "_"]
    (let [sym (symbol (str candidate fresh))]
      (if (contains? rulemap sym)
        (recur (str candidate "_"))
        sym))))

(defn parse-rules [args]
  (loop [args args, fresh 0, rulemap {}, rulenames []]
    (if (and (seq args) (not (keyword? (first args))))
      (let [[rulename, lhs, mid, rhs, fresh', args']
            (let [[arg1, arg2, arg3, arg4] args]
              (if (symbol? arg1)
                [arg1, arg2, arg3, arg4, fresh, (rest (rest (rest (rest args))))]
                [(gen-fresh rulemap fresh), arg1, arg2, arg3, (inc fresh),
                 (rest (rest (rest args)))]))]
        (recur args' fresh' (parse-rule rulemap rulename lhs mid rhs)
               (conj rulenames rulename)))
      [rulemap, rulenames, args])))

;; (parse-rules '[lplus-zero (0 + ?X) -> ?X
;;                rplus-zero (?X + 0) -> ?X
;;                ltimes-one (?X * 1) -> ?X
;;                rtimes-one (1 * ?X) -> ?X])

;; (parse-rules '[lplus-zero (0 + ?X) -> ?X
;;                rplus-zero (?X + 0) -> ?X
;;                ltimes-one (?X * 1) -> ?X
;;                rtimes-one (1 * ?X) -> ?X
;;                :strategy success])

(defn parse-rewrite
  [args]
  (let [[rulemap, rulenames, args'] (parse-rules args)
        [strat-kw strat] args']
    (when (and strat-kw (not (#{:strategy :strat} strat-kw)))
      (throw (ex-info "Malformed `defrewrite`, `:strategy` keyword expected"
                      {:args args'})))
    {:rules rulemap
     :rule-names rulenames
     :strategy strat}))

(defn mk-rule-funs [rules vars]
  (loop [rules rules, res []]
    (if (seq rules)
      (let [[rname [lhs rhs]] (first rules)]
        (recur (rest rules) (conj res rname (s/rule-expander lhs rhs vars))))
      res)))

(mk-rule-funs '[[plus-zero [(x + 0) x]]
                [times-one [(x * 1) x]]] '#{x})

(defn mk-default-strategy [rules vars]
  (s/rules-expander rules vars))

(mk-default-strategy '[[(x + 0) x]
                       [(x * 1) x]] '#{x})

(defmacro rewrite
  "Definition of a rewrite system."
  [vars & args]
  {:style/indent [1]}
  (let [{:keys [rules rule-names strategy]} (parse-rewrite args)
        varset (into #{} vars)
        strat-fn (or strategy (mk-default-strategy (map (fn [[_ [lhs rhs]]]
                                                          [lhs rhs]) rules) varset))
        params (gensym "params")]
    `(let ~(mk-rule-funs rules varset)
       (let [strat-fn# ~strat-fn]
         (fn [& ~params]
           (case (count ~params)
             0 strat-fn#
             1 (strat-fn# (first ~params))
             (throw (ex-info "Wrong number of arguments (expecting 0 or 1)" {:args ~params}))))))))

(macroexpand-1 '(rewrite [x]
                         lplus-zero (0 + x) -> x
                         rplus-zero (x + 0) -> x
                         ltimes-one (x * 1) -> x
                         rtimes-one (1 * x) -> x))

((rewrite [x]
  lplus-zero (0 + x) -> x
  rplus-zero (x + 0) -> x
  ltimes-one (x * 1) -> x
  rtimes-one (1 * x) -> x)
 '(0 + 4))

((rewrite [x]
  lplus-zero (0 + x) -> x
  rplus-zero (x + 0) -> x
  ltimes-one (x * 1) -> x
  rtimes-one (1 * x) -> x)
 42)




