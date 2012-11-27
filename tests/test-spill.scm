
(declare (uses arch machine utils liveness))

(include "arch-syntax")

;; create test input for spill

(define (generate-test-code)
  (let* ((mod (mc-make-module))
         (cxt (make-mc-context 'test_spill_01 '() '() '()))
         (blk (mc-make-block cxt 'test_spill_01)))
    (arch-emit-code x86-64 blk
      
      ;; prolog
      (push.r   (hreg rbp))
      (mov.rr   (hreg rsp) (hreg rbp))

      ;; initialize the first three vregs to 1
      (mov.i64r (imm i32 1) (vreg 't1))
      (mov.i64r (imm i32 1) (vreg 't2))
      (mov.i64r (imm i32 1) (vreg 't3))
  
      ;; clear the fourth vreg
      (mov.i64r (imm i64 0) (vreg 't4))

      ;; add the contents of t1,t2,t3 to t4
      (add.rr   (vreg 't1)   (vreg 't4))
      (add.rr   (vreg 't2)   (vreg 't4))
      (add.rr   (vreg 't3)   (vreg 't4))

      ;; epilog: return t4 via rax (x86 calling conv)
      (mov.rr   (vreg 't4)  (hreg rax))
      (mov.rr   (hreg rbp)  (hreg rsp))
      (pop.r    (hreg rbp))
      (retnear))

    (mc-context-start-set! cxt blk)
    (make-mc-module (list cxt))))

(define (test-spill-01)
  (let ((mod (generate-test-code)))
     (alloc-regs-test mod '(rsi rdi r8))
     (mc-module-print mod (current-output-port))))


(test-spill-01)
 
