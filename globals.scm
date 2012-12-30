(declare (unit globals))

(module globals *

  (import scheme)
  (import chicken)

  (import helpers)

  ;; The default selected architecture
  (define *arch* #f)

  (define-struct arch-descriptor
      (make-context
       operand-format
       vregs-read
       vregs-written
       generate-bridge-context
       emit-statement))

)
