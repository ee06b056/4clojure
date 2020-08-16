(ns clojure-practice.macro
  (:require
   [clojure.test :refer [report]]))

(defmacro backwards
  [form]
  (println (type form))
  (reverse form))

(macroexpand '(when boolean-exp
                exp1
                exp2))

(defmacro infix [form]
  (prn form)
  (list (second form) (first form) (last form)))

(macroexpand '(infix (1 + 2)))
(macroexpand-1 '(infix (1 + 2)))

(defmacro autoAdd [form]
  (list '+ (first form) (second form)))

(autoAdd (1 2))

(macroexpand '(autoAdd (1 2)))

(defmacro my-print [exp]
  (list 'let [] (list '+ 1 2)))

(defmacro my-print
  [exp]
  (list 'let ['result exp]
        (list 'println 'result)
        'result))
(my-print "libo")

(macroexpand '(my-print "libo"))

(defmacro code-critic
  [bad good]
  (list 'do
        (list 'println "Great squid of Madrid, this is bad code:"
              (list 'quote bad))
        (list 'println "Sweet gorilla of Manila, this is good code:"
              (list 'quote good))))

(code-critic (1 + 1) (+ 1 1))

(defmacro code-critic
  [bad good]
  `(do
     (println "bad: " '~bad)
     (println "good: " '~good)))

(code-critic (1 + 1) (+ 1 1))

(defn criticize-code
  [criticism code]
  `(println ~criticism '~code))

(defmacro code-critic
  [bad good]
  `(do
     ~(criticize-code "bad: " bad)
     ~(criticize-code "good: " good)))


(report (= 1 1))
(report (= 1 2))

(doseq [code ['(= 1 1) '(= 1 2)]]
  (report code))

(macroexpand-1 '(report (= 1 1)))

(def order-details
  {:name "Mitchard"
   :email "mitchard@gmail.com@"})

(def order-details-validations
  {:name ["please enter a name" not-empty]
   :email ["please enter an email address" not-empty
           "Your email address doesn't look like an email address" #(or (empty? %) (= 1 (count (re-seq #"@" %))))]})

(defn error-message-for
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-message-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(validate order-details order-details-validations)

(let [errors (validate order-details order-details-validations)]
  (if (empty? errors)
    (println :success)
    (println :failure errors)))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(defmacro when-valid
  [to-validate validations & body]
  `(let [error-messages# (validate ~to-validate ~validations)]
     (when-not (empty? error-message#)
       ~@body)))

(defmacro my-or
  ([] true)
  ([exp] exp)
  ([exp & next]
   `(let [or# ~exp]
      (if or#
        or#
        (my-or ~@next)))))

(my-or true false)