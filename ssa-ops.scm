

(define-struct ssa-op (name print constant-fold iterate-uses replace-uses list-uses))

(define <ssa-op-add>
  (make-ssa-op
   'add
   ssa-format-add
   ssa-constant-fold-add
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-sub>
  (make-ssa-op
   'sub
   ssa-format-sub
   ssa-constant-fold-sub
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-mul>
  (make-ssa-op
   'mul
   ssa-format-mul
   ssa-constant-fold-mul
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-and>
  (make-ssa-op
   'and
   ssa-format-and
   ssa-constant-fold-and
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-or>
  (make-ssa-op
   'or
   ssa-format-or
   ssa-constant-fold-or
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-xor>
  (make-ssa-op
   'xor
   ssa-format-xor
   ssa-constant-fold-xor
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-shl>
  (make-ssa-op
   'shl
   ssa-format-shl
   ssa-constant-fold-shl
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-shr>
  (make-ssa-op
   'shr
   ssa-format-shr
   ssa-constant-fold-shr
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))




  