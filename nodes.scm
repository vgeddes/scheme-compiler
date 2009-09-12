
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

(define-class <prim> (name args result cexp)
  (name name)
  (args args)
  (result result)
  (cexp cexp))

(define-class <null> ())

;; LLIL: A machine executable language

;; this language borrows <constant>, <variable> , and <if> from HLIL, for they have the same semantics in LLIL. 

;; A label for a block of code
(define-class <label> (name)
  (name name))

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


(define-class <block> (head tail pred succ notes)
  (head head)
  (tail tail)
  (pred pred)
  (succ succ))



(define-class <code> (entry labels)
  (entry entry)
  (labels labels))

(define-class <fun> (label args blocks notes)
  (label label)
  (args args)
  (blocks blocks)
  (notes notes))
  
(define-class <x86/add> (x y)
  (x x) (y y))

(define-class <x86/sub> (x y)
  (x x) (y y))

(define-class <x86/mul> (x)
  (x))

(define-class <x86/cmp> (x y)
  (x x) (y y))

(define-class <x86/je> (x)
  (x))

(define-class <x86/jne> (x)
  (x))

(define-class <x86/jl> (x)
  (x))

(define-class <x86/jle> (x)
  (x))

(define-class <x86/jg> (x)
  (x))

(define-class <x86/jge> (x)
  (x))

(define-class <x86/jmp> (x)
  (x))

(define-class <x86/sete> (x)
  (x x))

(define-class <x86/setne> (x)
  (x x))

(define-class <x86/and> (x y)
  (x x) (y y))

(define-class <x86/or> (x y)
  (x x) (y y))

(define-class <x86/shr> (x y)
  (x x) (y y))

(define-class <x86/shl> (x y)
  (x x) (y y))
  
(define-class <x86/mov> (x y)
  (x x) (y y))
