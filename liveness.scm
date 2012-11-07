
(declare (unit liveness)
         (uses nodes arch utils))

(use matchable)
(use srfi-1)

(include "struct-syntax")
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
(match context
    (('context name args start)
  (print
   (list 'context (node-value start) args
         (map (lambda (block)
                (cons (bfun block)
                      (list (map (lambda (instr)
                                   (ifun instr))
                                 (select-each-instr block)))))
              (select-each-block
               start)))))))

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
      (('block name (successor* ...) (instr* ...))
       (let* ((nodes
                (map (lambda (instr)
                       (make-node (counter) 'instr instr '() '()))
                     instr*))
              (head
                (car nodes))
              (tail (car (reverse nodes)))
              (successors
                (map (lambda (succ)
                       (make-block succ counter))
                     successor*))
              (basic-block
                (make-node (counter) 'block name '() (list head))))
         
         (let f ((cur (car nodes)) (rest (cdr nodes)))
           (match rest
             (() cur)
             ((r . r*)
              (node-succ-set! cur (list r))
              (node-pred-set! r   (list cur))
              (f r r*))))

         (node-succ-set! tail successors)

         ;; set predecessor pointers
         (for-each (lambda (successor)
                     (node-pred-set! successor (list tail)))
                   successors)
         basic-block))))

  (match context
    (('context name args start)
     (let* ((start-node (make-block start (make-count-generator))))
       (list 'context name args start-node)))))


(define assert-not-reached
  (lambda (proc . irritants)
    (apply error `(assert-not-reached ,proc ,@irritants))))


(define (sort-reverse-pre-order node)
  (define (walk node)
    (cons node (apply append (map walk (node-succ node)))))
  (reverse (walk node)))


(define (use-at node)
  (cond
   ((eq? (node-type node) 'block) '())
   (else
    (instr-use-list (node-value node)))))

(define (def-at node)
  (cond
   ((eq? (node-type node) 'block) '())
   (else
    (instr-def-list (node-value node)))))

(define (analyze-liveness context)

(match context
  (('context name args entry)

  (let* ((node* (sort-reverse-pre-order entry))
         (len  (length node*))
         (in   (make-vector len '()))
         (out  (make-vector len '()))
         (def  (make-vector len '()))
         (use  (make-vector len '())))

    (for-each
     (lambda (node)
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

   (for-each
     (lambda (node)
       (cond
         ((eq? (node-type node) 'instr)
          (instr-data-set! (node-value node) (vector-ref in (node-id node))))))
     node*)


;;    (print-cfg-with-formatters
;;     (lambda (block)
;;        (node-value block))
 ;;     (lambda (node)
;;        (format "~a\t\t~a" (format-instr (node-value node)) (vector-ref in (node-id node))))
;;      context)

    '()))))



(define (allocate-registers-pass mod)

 (define (analyze-defs block)
    (match block
      (('block name (successor* ...) (instr* ...))
            (apply lset-union 
               (cons eq? (append (map (lambda (instr)
                                   (instr-def-list instr))
                                  instr*)
                                 (map analyze-defs successor*)))))))

  (match mod
    (('module contexts)
     (for-each 
        (lambda (cxt)
           (match cxt
             (('context name args start)
              (pretty-print (analyze-defs start)))))
         contexts)))
        
      mod)

(define (analyze-liveness-pass mod)
  (match mod
    (('module contexts)
     (let ((contexts (map build-cfg contexts)))
       (for-each analyze-liveness contexts)
       mod))))

