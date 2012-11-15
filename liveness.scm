
(declare (unit liveness)
         (uses nodes mc utils))

(use matchable)
(use srfi-1)

(include "struct-syntax")
(include "munch-syntax")

(define (build-cfg context)

  (define (make-block block counter)
    (match block
      (($ mc-block name head tail (successor* ...) cxt)
       (let* ((nodes (let f ((instr head) (nodes '()))
                       (cond
                         ((null? instr)
                          (reverse nodes))
                         (else
                          (f (mc-instr-next instr)
                             (cons (make-node (counter) 'instr instr '() '()) nodes))))))
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

  (struct-case context
    ((mc-context name args start vreg-pool)
     (make-block start (make-count-generator)))))


(define assert-not-reached
  (lambda (proc . irritants)
    (apply error `(assert-not-reached ,proc ,@irritants))))


(define (sort-reverse-pre-order node)
  (define (walk node)
    (cons node (apply append (map walk (node-succ node)))))
  (reverse (walk node)))


(define (def-use-at node)
  (cond
   ((eq? (node-type node) 'block)
    (values '() '()))
   (else
    (mc-instr-def-use (node-value node)))))


(define (vreg-eq? v1 v2)
  (eq? (mc-vreg-name v1) (mc-vreg-name v2)))

(define (analyze-liveness cfg)

  (let* ((node* (sort-reverse-pre-order cfg))
         (len   (length node*))
         (in    (make-vector len '()))
         (out   (make-vector len '()))
         (def   (make-vector len '()))
         (use   (make-vector len '())))
    (for-each
      (lambda (node)
        (let-values (((defs uses) (def-use-at node)))
          (vector-set! def (node-id node) defs)
          (vector-set! use (node-id node) uses)))
       node*)
    (for-each
     (lambda (node)
       (vector-set! out (node-id node)
         (fold (lambda (succ acc)
                 (append (vector-ref in (node-id succ)) acc))
               '()
               (node-succ node)))
       (vector-set! in (node-id node)
         (lset-union vreg-eq?
           (vector-ref use (node-id node))
           (lset-difference vreg-eq?
             (vector-ref out (node-id node))
             (vector-ref def (node-id node))))))
     node*)
   (for-each
     (lambda (node)
       (cond
         ((eq? (node-type node) 'instr)
          (mc-instr-data-set! (node-value node)
            (lset-union vreg-eq? (vector-ref in  (node-id node))
                                 (vector-ref def (node-id node)))))))
     node*)

    ))

(define (analyze-liveness-pass mod)
  (struct-case mod
    ((mc-module contexts)
     (let ((cfg* (map build-cfg contexts)))
       (for-each analyze-liveness cfg*)
       mod))))

(define-struct info (name range data))

(define (allocate-registers-pass mod)

 (define *temp-info* '())

 (define (temp-info tmp)
   (cond
     ((assq tmp *temp-info*) => cdr)
     (else 
       (let ((info (make-info tmp (cons -1 -1) '())))
          (set! *temp-info* (cons (cons tmp info) *temp-info*))
          info))))

 (define (analyze-defs block)
    (match block
      (('block name (successor* ...) (instr* ...))
            (apply lset-union 
               (cons vreg-eq? (append (map (lambda (instr)
                                   (instr-def-list instr))
                                  instr*)
                                 (map analyze-defs successor*)))))))

  (define (compute-live-range-for-temp temp start)
    (let f ((block start) (id-gen (make-count-generator)))
      (match block
        (('block name (successor* ...) (instr* ...))
           (let g ((i* instr*))
             (match i*
                (() '())
                ((i . i*)
                 (let ((id (id-gen)))    
                  (cond
                     ((memq temp (instr-data i))
                      (let ((info (temp-info temp)))
                        (match (info-range info)
                          ((start . end)
                           (if (< start 0)
                             (info-range-set! info (cons id end)))
                           (if (> id end)
                             (info-range-set! info (cons (car (info-range info)) id)))))))))
                  (g i*))))
            (for-each (lambda (block) 
                        (f block id-gen))
               successor*))))
        (info-range (temp-info temp)))

  (match mod
    (('module contexts)
     (for-each 
        (lambda (cxt)
           (match cxt
             (('context name args start)
              (let ((defs (analyze-defs start)))
                 (for-each
                   (lambda (def)
                       (pretty-print (list def (compute-live-range-for-temp def start))))
                   defs)))))
         contexts)))
  mod)

