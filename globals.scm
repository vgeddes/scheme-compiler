(declare (unit globals))

(module globals *

  (import scheme)
  (import chicken)

  (import helpers)

  ;; The default selected architecture
  (define *arch*          #f)
  (define *hreg-start-id* #f)
  (define *hreg-table*    #f)

  (define-struct arch-impl
      (init
       make-context
       operand-format
       vreg-uses
       vreg-defs
       generate-bridge-context
       assign-stack-slot
       rewrite
       print-frame-info
       emit-statement))

)
