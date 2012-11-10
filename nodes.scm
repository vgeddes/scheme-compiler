
;;
;; node types
;;

(declare (unit nodes))

(include "struct-syntax")

;; HLIL: A basic lambda language

(define-struct if       (test conseq altern))
(define-struct lambda   (name args body free-vars))
(define-struct comb     (args))
(define-struct constant (value))
(define-struct variable (name))
(define-struct fix      (defs body))
(define-struct prim     (name args result cexp))
(define-struct nil      ())

;; LLIL: A machine executable language

;;; this language borrows <constant>, <variable> , and <if> from HLIL, for they have the same semantics at this level. 

;;; A label for a block of code
(define-struct label   (name))

;;; Function application. The compiler overloads this operation for primitive application.
;;; This operation never returns, but merely passes control to its continuation
(define-struct app     (name args))

;;; Selects a value from a record and binds it to 'name in the continuation expression 'cexp
(define-struct select  (index record name cexp))

;;; Creates a record from a list of values and binds it to 'name in cexp
(define-struct record  (values name cexp))


(define-struct block   (label code))

(define-struct module  (contexts))

(define-struct node     (id type value pred succ))
(define-struct context  (formals start blocks))

;; aggregate structures 
(define-struct machine-module   (contexts))
(define-struct machine-context  (name args start))
(define-struct machine-block    (name head tail successors))

;; instructions
(define-struct machine-descriptor       (name format verifiers defs uses))

(define-struct machine-instr            (descriptor ops next prev data))
 
;; operands 

(define-struct machine-addr-x86-64  (base disp))
(define-struct machine-vreg         (name))
(define-struct machine-imm          (size value))

;; data

(define-struct machine-data     (name size)) 




