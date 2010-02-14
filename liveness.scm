
(declare (unit liveness)
         (uses nodes munch utils))

(use matchable)
(use srfi-1)

(include "class-syntax")
(include "munch-syntax")

(define (for-each-block fun start)
  (define (walk-cfg node)
    (cond
     ((eq? (node-type node) 'block)
      (fun node)))
    (for-each walk-cfg (node-succ node)))
  (walk-cfg start))

(define (select-each-block start)
  (let ((blocks '()))
    (for-each-block
      (lambda (node)
        (set! blocks (cons node blocks)))
      start)
    (reverse blocks)))

(define (for-each-instr fun block)
  (define (walk-cfg node)
    (cond
     ((eq? (node-type node) 'instr)
      (fun node)
      (for-each walk-cfg (node-succ node)))))
  (walk-cfg (first (node-succ block))))

(define (select-each-instr block)
  (let ((instrs '()))
    (for-each-instr
      (lambda (node)
        (set! instrs (cons node instrs)))
      block)
    (reverse instrs)))

(define (print-cfg-with-formatters bfun ifun context)
  (pretty-print
   (list 'context (node-value (context-start context)) (context-formals context)
         (map (lambda (block)
                (cons (bfun block)
                      (list (map (lambda (instr)
                                   (ifun instr))
                                 (select-each-instr block)))))
              (select-each-block
               (context-start context))))))

(define (print-cfg context)
  (print-cfg-with-formatters
    (lambda (block)
      (node-value block))
    (lambda (node)
      (node-value node))
    context))

(define (build-cfg context)

  (define (make-block block counter)
    (match block
      ((label (successor* ...) (instr* ...))
       (let* ((nodes
               (map (lambda (instr)
                      (make-node (counter) 'instr instr '() '()))
                    instr*))
              (head
               (car nodes))
              (tail (car (reverse nodes)))
              (successors
               (map (lambda (label)
                      (cond
                       ((assq label (context-blocks context))
                        => (lambda (block)
                             (make-block block counter)))
                       (else (error 'make-block) (assert-not-reached 'succ label))))
                    successor*))
              (basic-block
               (make-node (counter) 'block label '() (list head))))
         
         (let link-pred-succ ((cur (car nodes)) (rest (cdr nodes)))
           (match rest
             (() cur)
             ((r . r*)
              (node-succ-set! cur (list r))
              (node-pred-set! r   (list cur))
              (link-pred-succ r r*))))

         (node-succ-set! tail successors)

         ;; set predecessor pointers
         (for-each (lambda (successor)
                     (node-pred-set! successor (list tail)))
                   successors)
         basic-block))))

  (struct-case context
    ((context formals start blocks)
     (cond 
      ((assq start blocks)
       => (lambda (block)
            (let* ((start-node (make-block block (make-count-generator))))
              (make-context formals start-node '()))))
      (else (assert-not-reached 'build-cfg start))))
    (else (assert-not-reached 'build-cfg))))

(define assert-not-reached
  (lambda (proc . irritants)
    (apply error `(assert-not-reached ,proc ,@irritants))))

(define (allocate-registers module)
  (struct-case module
    ((module contexts)
     (let ((contexts (map build-cfg contexts)))
       (for-each analyse-liveness contexts)
       (make-module contexts)))
    (else (assert-not-reached 'allocate-registers))))

(define (sort-reverse-pre-order node)
  (define (walk node)
    (cons node (apply append (map walk (node-succ node)))))
  (reverse (walk node)))


(define (instr-use-fold instr fun init)
  ((instr-descriptor-use-fold-proc (instr-descriptor instr))
   instr fun init))

(define (instr-def-fold instr fun init)
  ((instr-descriptor-def-fold-proc (instr-descriptor instr))
   instr fun init))

(define (def-at node)
  (cond
   ((eq? (node-type node) 'block) '())
   (else
    (instr-def-fold
     (node-value node)
     (lambda (def acc)
       (cons def acc))
     '()))))

(define (use-at node)
  (cond
   ((eq? (node-type node) 'block) '())
   (else
    (instr-use-fold
     (node-value node)
     (lambda (def acc)
       (cons def acc))
     '()))))

(define (analyse-liveness context)
  (let* ((node* (sort-reverse-pre-order (context-start context)))
         (len  (length node*))
         (in   (make-vector len '()))
         (out  (make-vector len '()))
         (def  (make-vector len '()))
         (use  (make-vector len '())))

    (for-each
     (lambda (node)
       ;; compute def/use for each node
      ;; (cond
     ;;   ((eq? (node-type node) 'instr)
      ;;   (pretty-print
      ;;    (list (format-instr (node-value node))
      ;;        (list 'def (def-at node))              
      ;;        (list 'use (use-at node))))))
       (vector-set! def (node-id node) (def-at node))
       (vector-set! use (node-id node) (use-at node)))
     node*)

    (for-each
     (lambda (node)

       (vector-set! out (node-id node)
         (fold (lambda (succ acc)
                 (append (vector-ref in (node-id succ)) acc))
               '()
               (node-succ node)))
       
       (vector-set! in  (node-id node)
         (lset-union eq?
           (vector-ref use (node-id node))
           (lset-difference eq?
             (vector-ref out (node-id node))
             (vector-ref def (node-id node))))))
     node*)

    (print-cfg-with-formatters
      (lambda (block)
        (node-value block))
      (lambda (node)
        (list (format-instr (node-value node)) (vector-ref in (node-id node))))
      context)
    in))
