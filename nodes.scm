
;;
;; node types
;;

(declare (unit nodes)
         (uses class))

(include "class-syntax")

;; HLIL: A basic lambda language

(define-class <if> (test conseq altern)
  (test   test)
  (conseq conseq)
  (altern altern))

(define-class <lambda> (name args body)
  (name      name)
  (args      args)
  (body      body)
  (free-vars '()))

(define-class <comb> (args)
  (args args))

(define-class <constant> (value)
  (value value))

(define-class <variable> (name)
  (name name))

(define-class <fix> (defs body)
  (defs defs)
  (body body))
  
;; LLIL: A machine executable language

;; this language borrows <constant>, <variable> , and <if> from HLIL, for they have the same semantics in LLIL. 

;; A label for a block of code
(define-class <label> (name args body)
  (name name)
  (args args)
  (body body))

;; Function application. The compiler overloads this operation for primitive application.
;; This operation never returns, but merely passes control to its continuation
(define-class <app> (name args)
  (name name)
  (args args))

;; Selects a value from a record and binds it to 'name in cexp
(define-class <select> (index record name cexp)
  (index  index)
  (record record)
  (name   name)
  (cexp   cexp))

;; Creates a record from a list of values and binds it to 'name in cexp
(define-class <record> (values name cexp)
  (values values)
  (name name)
  (cexp cexp))