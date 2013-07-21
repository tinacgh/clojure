(ns clinter (:require [clojure.repl :refer [doc]]))

;;; (length => (count
(count "This is a string.")

;;; (char STR n) => (get STR n)
(get "abc" 2)

;;; #\a => \a, clj has \newline and \space
;;; (symbol-name => (str
;;; opposite is (symbol STR) 
;;; (type-of => (class

;;; *package* => *ns* (namespace)
;;; (in-package 'PACKAGE) => (ns NAMESPACE)

;;; #' indicates clojure.lang.Var

;;; LISTS
;;; (cons... second argument must be a list
;;; (first, (last, (rest, (nth all present
;;; (length => (count

;;; Chapter 10
;;; (defn NAME DOC-STR [PARAMS] BODY)
(defn list3
  "Returns a list of its three arguments"
  [o1 o2 o3]
  (cons o1 (cons o2 (cons o3 '()))))

;;; Chapter 13
;;; (number?, (integer?, (float?, (char?, (string?, (symbol?,
;;; (list?, (set?, (vector? ... etc. see clojure.org/cheatsheet
;;; and search for ?

;;; (cond has fewer parentheses.
;;; (cond PAIR-A-1 PAIR-A2
;;;       PAIR-B-1 PAIR-B2... etc
(defn sign
  "Takes a number and returns -, 0, or +"
  [n]
  (cond (< n 0) '-
        (= n 0) 0
        true '+))

;;; Chapter 15 - Recursion
(defn sum
  "Returns sum of two nonnegative integers."
  [n1 n2]
  (if (zero? n1) n2
      (sum (- n1 1) (+ n2 1))))