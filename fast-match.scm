
(use matchable)
(use srfi-1)
(use srfi-95)

(import-for-syntax matchable)
(import-for-syntax srfi-1)
(import-for-syntax srfi-95)
(import-for-syntax expand-full)
 
(define (fast-match-fail)
  (error 'fast-match))

(define-syntax fast-match

  (lambda (e r c)
   

    (define (make-node path binding branches live-indices)
      (vector 'node path binding branches live-indices #f #f #f))

    (define (make-leaf-node index bindings action)
      (vector 'leaf-node '() '() '() '() index bindings action))

    (define (node-type     x) (vector-ref x 0))
    (define (node-path     x) (vector-ref x 1))
    (define (node-binding  x) (vector-ref x 2))
    (define (node-branches x) (vector-ref x 3))
    (define (node-indices  x) (vector-ref x 4))
    (define (node-index    x) (vector-ref x 5))
    (define (node-bindings x) (vector-ref x 6))
    (define (node-action   x) (vector-ref x 7))

    (define (node? x)
      (eq? (node-type x) 'node))

    (define (leaf-node? x)
      (eq? (node-type x) 'leaf-node))

    (define (make-branch constr succ)
      (cons constr succ))
    
    (define (branch-constr br) (car br))
    (define (branch-succ   br) (cdr br))

    (define (parse-rule index pat action)

      (define (parse pat path cont)
        (cond
         ((eq? pat '_)
          (make-node path '() (list (make-branch `(any) cont)) (list index)))
         ((symbol? pat)
          (make-node path (list pat) (list (make-branch `(any) cont)) (list index)))
         ((null? pat)
          (make-node path '() (list (make-branch `(null) cont)) (list index)))            
         ((or (boolean? pat) (string? pat) (number? pat) (char? pat))
          (make-node path '() (list (make-branch `(constant ,pat) cont)) (list index)))
         ((and (list? pat) (eq? (car pat) 'quote))
          (make-node path '() (list (make-branch `(constant ,pat) cont)) (list index)))
         ((pair? pat)
          (make-node path '() (list (make-branch `(pair)
                                      (parse (car pat) (cons 'car path)
                                        (parse (cdr pat) (cons 'cdr path) cont)))) (list index)))
         ((vector? pat)
          (make-node path '()
                     (list
                       (make-branch `(vector ,(vector-length pat))
                         (parse-vector pat path 0 cont)))
                     (list index)))
         (else (error "exit"))))
        
      (define (parse-vector pat path index cont)
        ;; Parse all sub-pattern in `pat`.
        (cond
         ((< index (vector-length pat))
          (parse (vector-ref pat index)
                 (cons (+ 1 index) path)
                 (parse-vector pat path (+ index 1) cont)))
         (else cont)))
    
      (define (extract-names pat)
        ;; Extract all names in the source pattern `pat`.
        (match pat
          ('_ '())
          (() '())
          ((? symbol?)
           (list pat))
          ((or #t #f (? string?) (? number?) (? char?) ('quote _))
           '())
          ((p . q)
           (append (extract-names p) (extract-names q)))
          (#(_ ...)
           (append-map extract-names (vector->list pat)))))
      
      (define *root-path*
        ;; the access path for the root value
        '(root))

      (parse pat *root-path* (make-leaf-node index (extract-names pat) action)))

    (define (merge-branches x y)

      (define (branch=? x y)
        (equal? (branch-constr x) (branch-constr y)))

      (let iterate-x ((x x) (y y) (z '()))
        (match x
          (() (append y z))
          ((a . a*)
           (let g ((u y) (v '()) (found '()))
             (match u
               (()
                (if (null? found)
                    (iterate-x a* v (cons a z))
                    (let ((merged
                           (make-branch
                            (branch-constr a)
                            (merge-two-trees (branch-succ a) (branch-succ found)))))
                      (iterate-x a* v (cons merged z)))))
               ((ui . ui*)
                (if (branch=? a ui)
                    (g ui* v ui)
                    (g ui* (cons ui v) found)))))))))

    (define (merge-two-trees x y)
      (cond
       ((and (leaf-node? x) (leaf-node? y))
        (if (< (node-index x) (node-index y))
            x 
            y))
       ((and (node? x) (node? y))
        (cond
         ((equal? (node-path x) (node-path y))
          (let ((merged-branches
                 (merge-branches (node-branches x) (node-branches y)))
                (merged-indices
                 (lset-union eq? (node-indices x) (node-indices y)))
                (merged-bindings
                 (lset-union eq? (node-binding x) (node-binding y))))
            (make-node (node-path x) merged-bindings merged-branches merged-indices)))
         (else (error "exit"))))
        (else (error "exit"))))

    (define (merge-trees trees)
      (cond
       ((null? trees) trees)
       (else
        (reduce
         (lambda (tree acc)
           (merge-two-trees acc tree))
         '()
         trees))))

    (define (nfa->dfa x)
      
      (define (convert-branch br) 
        (make-branch (branch-constr br) (nfa->dfa (branch-succ br))))
      
      (define (extract-any branches)
        (let f ((xs branches) (acc '()) (any '()))
          (match xs
            (()
             (cons any acc)) 
            ((x . xs)
             (case (car (branch-constr x))
               ((any)
                (f xs acc x))
               (else
                (f xs (cons x acc) any)))))))
      
      (case (node-type x)
        ((node)
         (let ((any-branches (extract-any (map convert-branch (node-branches x)))))
           (if (null? (car any-branches))
               (make-node (node-path x) (node-binding x) (cdr any-branches) (node-indices x))
               (let ((branches
                      (reverse
                       (cons (car any-branches)
                             (map (lambda (br)
                                    (make-branch
                                     (branch-constr br)
                                     (nfa->dfa
                                      (merge-two-trees
                                       (branch-succ br)
                                       (branch-succ (car any-branches))))))
                                  (cdr any-branches))))))
                 (make-node (node-path x) (node-binding x) branches (node-indices x))))))
        ((leaf-node) x)))
    
    (define (gen-dfa node value)
      ;; Compiles the DFA `node` into native scheme code which will match against the scheme datum `value`. 
  
      (define (gen-ref path table)
        ;; Generates scheme code to reference a part of an aggregate scheme datum.
        (let ((%car        (r 'car))
              (%cdr        (r 'cdr))
              (%vector-ref (r 'vector-ref)))
          (match path
            ((root)
             (let ((parent
                    (cond
                     ((assoc '(root) (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               parent))
            (('car rest ...)
             (let ((parent
                    (cond
                     ((assoc rest (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               `(,%car ,parent)))
            (('cdr rest ...)
             (let ((parent
                    (cond
                     ((assoc rest (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               `(,%cdr ,parent)))
            ((x ...)
             (let ((parent
                    (cond
                     ((assoc (cdr x) (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               `(,%vector-ref ,parent ,(- (car x) 1)))))))

      (define (gen-branches branches name mappings)
        ;; Generates code for outgoing edges and successors of the decision node.

        (let walk ((branches branches))
          (match branches
            (() `(fast-match-fail))
            ((x . xs)
             (let ((rest            (walk xs))
                   (succ            (gen-node (branch-succ x) mappings))
                   (%if             (r 'if))
                   (%and            (r 'and))
                   (%vector?        (r 'vector?))
                   (%vector-length  (r 'vector-length))
                   (%pair?          (r 'pair?))
                   (%equal?         (r 'equal?))
                   (%null?          (r 'null?))
                   (%=              (r '=)))
               (match (branch-constr x)
                 (('vector size)
                  `(,%if (,%and (,%vector? ,name) (,%= (,%vector-length ,name) ,size))
                         ,succ
                         ,rest))
                 (('null)
                  `(,%if (,%null? ,name)
                         ,succ
                         ,rest))
                 (('pair)
                  `(,%if (,%pair? ,name)
                         ,succ
                         ,rest))
                 (('constant data)
                  `(,%if (,%equal? ,name ,data)
                         ,succ
                         ,rest))
                 (('any)
                  succ)))))))
      
      (define (extend-table path bindings name table)
        (cons
         (cons (cons path name) (car table))
         (if (null? bindings)
             (cdr table)  
             (append (map (lambda (binding)
                            (cons binding name))
                          bindings)
                     (cdr table)))))
      
      (define (gen-node x table)
        (case (node-type x)
          ((node)
           (let* ((path         (node-path x))
                  (binding      (node-binding x))
                  (branches     (node-branches x))
                  (name         (gensym))
                  (ref-code     (gen-ref path table))
                  (branch-code  (gen-branches branches name (extend-table path binding name table)))
                  (%let         (r 'let)))
             `(,%let ((,name ,ref-code))
                ,branch-code)))
          ((leaf-node)
           (let* ((action   (node-action x))
                  (bindings (map (lambda (binding)
                                   (cond
                                    ((assq binding (cdr table))
                                     => (lambda (binding)
                                          (list (car binding) (cdr binding))))
                                    (else (error "exit"))))
                                 (node-bindings x)))
                  (%let (r 'let)))
             `(,%let ,bindings ,@action)))))
      
      (define (init-mappings root name)
        (let ((binding (node-binding root)))
          (cons (list (cons '(root) name))
                (if (null? binding)
                    '()
                    (list (cons binding name))))))
      
      (let* ((root-name (gensym))
             (code (gen-node node (init-mappings node root-name)))
             (%let (r 'let)))
        `(,%let ((,root-name ,value)) ,code)))
 
  (define (index-generator)
    (let ((k 0))
      (lambda ()
        (let ((ret k))
          (set! k (+ k 1))
          ret))))

  (match e
    (('fast-match data cls* ...)
     (let* ((forest
             (let ((index (index-generator)))
               (map (lambda (cls)
                      (match cls
                        ((pat action* ...)
                         (parse-rule (index) pat action*))))
                    cls*)))
            (nfa  (merge-trees forest))
            (dfa  (nfa->dfa nfa))
            (code (gen-dfa dfa data)))
       code)))))


;; (define (action) #f)

;; (define (test-fast-match data) 
;;   (fast-match data
;;               (#('add #(_  _  _) '6)
;;                (action))
;;               (#('add #('sub 'y #f) 'z)
;;                (action))
;;               (#('add #(_ 'u #f) 'z) 
;;                (action)) 
;;               (#(_ 'x 'z) 
;;                (action))          
;;               (45        
;;                (action))
;;               (#('and 't 'z)
;;                (action))           
;;               (#('xor)      
;;                (action))           
;;               (('xor ('bor _) 6)
;;                (action))       
;;               (('xor ('bor 'fo) 7)
;;                (action))
;;               (#('add #('sub var #f) 'z)
;;                (action))
;;               (('xor (5) 7)
;;                (action))
;;               (('xor (6 7) 7)
;;                (action))
;;               (('xor (6 7 9 5) 7)
;;                (action))
;;               (('xor (_ 'fo) 7)
;;                (action))))

;; (define (test-match data)
;;   (match data
;;     (#('add #(_  _  _) '6)
;;      (action))
;;     (#('add #('sub 'y #f) 'z)
;;      (action))
;;     (#('add #(_ 'u #f) 'z) 
;;      (action)) 
;;     (#(_ 'x 'z) 
;;      (action))          
;;     (45        
;;      (action))
;;     (#('and 't 'z)
;;      (action))           
;;     (#('xor)      
;;      (action))           
;;     (('xor ('bor _) 6)
;;      (action))       
;;     (('xor ('bor 'fo) 7)
;;      (action))
;;     (#('add #('sub var #f) 'z)
;;      (action))
;;     (('xor (5) 7)
;;      (action))
;;     (('xor (6 7) 7)
;;      (action))
;;     (('xor (6 7 9 5) 7)
;;      (action))
;;     (('xor (_ 'fo) 7)
;;      (action))))


;; (define N 500000)

;; (time
;;  (let f ((i 0))
;;    (cond
;;     ((< i N)
;;      (test-fast-match '#(add #(sub x #f) z))
;;      (test-fast-match '(xor (4 fo) 7))
;;      (f (+ i 1))))))

;; (print)

;; (time
;;  (let f ((i 0))
;;    (cond
;;     ((< i N)
;;      (test-match '#(add #(sub x #f) z))
;;      (test-match '(xor (4 fo) 7))
;;      (f (+ i 1))))))


