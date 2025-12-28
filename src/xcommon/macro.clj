(ns xcommon.macro
  (:refer-clojure :exclude [str concat])
  (:require [clojure.core :as c]))

(defmacro whens
  "Processes any and all expressions whose tests evaluate to true.
   Example:
   (let [m (java.util.HashMap.)]
    (whens
     false (.put m :z 0)
     true  (.put m :a 1)
     true  (.put m :b 2)
     nil   (.put m :w 3))
    m)
   => {:b=2, :a=1}
  "
  [& [test expr :as clauses]]
  (when clauses
    `(do (when ~test ~expr)
         (whens ~@(nnext clauses)))))

;; Inverse Helper Functions: Unordered -> Ordered
(defmacro forv [& body]
  `(vec (for ~@body)))

(defn- make-concat-xform
  [safe?]
  (comp (partition-by string?)
        (filter some?)
        (mapcat (fn [part]
                  (if (string? (first part))
                    [(apply c/str part)]
                    (if safe?
                      (map (fn [o] (list 'js* "(~{} ?? \"\")" o)) part)
                      (map (fn [o] (list 'js* "(~{})" o)) part)))))))


(defmacro concat
  "A macro variant of the clojure.core/str function that performs
  considerably faster string concatenation operation on CLJS (on
  JVM/CLJ it only applies basic simplification and then relies on the
  `clojure.core/str`)."
  [& params]
  (if (:ns &env)
    (let [xform  (make-concat-xform true)
          params (into [] xform params)
          stmpl  (reduce c/str "\"\"" (repeat "+~{}" (count params)))]
      (cons 'js* (cons stmpl params)))
    (cons `c/str params)))

(defmacro str
  [& params]
  `(concat ~@params))
