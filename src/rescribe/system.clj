(ns rescribe.system
  "This namespace defines the `defsystem` macro that
 is the principal mean of encoding rewrite systems
  in *rescribe*."
  (:require [rescribe.term :refer [rule-wff?]]
            [rescribe.match :refer [match]]))


(defn- rule-to-map
  [rname lhs mid rhs]
  (let [rule [(list 'quote lhs) (list 'quote rhs)]]
    (if (not (apply rule-wff? rule))
      (throw (ex-info "Malformed rule." {:rule-name rname :lhs lhs :rhs rhs})))
    (if (not (= (name mid) "->"))
      (throw (ex-info "Missing arrow -> in rule." {:rule-name rname})))
    [(keyword (name rname)) rule]))


(defn- check-rule
  [rname lhs mid rhs]
  (if (not (rule-wff? lhs rhs))
    (throw (ex-info "Malformed rule." {:rule-name rname :lhs lhs :rhs rhs})))
  (if (not (= (name mid) "->"))
    (throw (ex-info "Missing arrow -> in rule." {:rule-name rname})))
  true)

(defn rewrite
  [term lhs rhs]
  (when-let [s (match lhs term)]
    (s rhs)))

(defn- mk-rule-fun
  [rname lhs _ rhs]
  (let [tvar (gensym "term")]
    `(defn ~rname [~tvar] (rewrite ~tvar (quote ~lhs) (quote ~rhs)))))

(defn- map4
  [f s] (if (seq s)
          (let [args (take 4 s)]
            (cons (apply f args)
                  (map4 f (drop 4 s))))
          '()))

(defn- mk-sys-strategy
  [sys-name strat]
  `(def ~(symbol (str sys-name "-strategy"))
     ~strat))

(defn- mk-sys-fun
  [sys-name]
  (let [term-param (gensym "term")
        strat-param (gensym "strat")]
    `(defn ~(symbol sys-name)
       ~(str "Rewrite term with the optional strategy for system: " sys-name)
       ([~term-param ~strat-param] (~strat-param ~term-param))
       ([~term-param] (~(symbol (str sys-name "-strategy")) ~term-param)))))

(defmacro defsystem
  "Definition of a rewrite system with a default
  rewrite strategy."
  [sys-name [& rules] with-kw strat]
  `(do
     ~@(map4 mk-rule-fun rules)
     ~(mk-sys-strategy sys-name strat)
     ~(mk-sys-fun sys-name)))

