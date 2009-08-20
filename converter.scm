


(declare (unit convert)
         (uses nodes class))

(use matchable)
(use srfi-1)

(include "class-syntax")

;; do some macro processing

(define (expand e)
   (match e
          (('let (bindings ...) body ...)
           (expand
            `((lambda ,(map car bindings)
                (begin ,@body))
              ,@(map (lambda (binding) (car (cdr binding))) bindings))))
          (('begin) '())
          (('begin body)
           (expand body))
          (('begin body1 body2 ...)
           (expand
            (let ((a (gensym)))
              `((lambda (,a)
                  (begin ,@body2)) ,body1))))
          (('or) #f)         
          (('or clause)
           (expand
            (let ((a (gensym)))
              `(let ((,a ,clause))
                 (if ,a ,a ,a)))))
          (('or clause1 clause2 ...)
           (expand
            (let ((a (gensym)))
              `(let ((,a ,clause1))
                 (if ,a
                     ,a
                     (or ,@clause2))))))
          (('and) #t)
          (('and clause)
           (expand
            (let ((a (gensym)))
              `(let ((,a ,clause))
                 (if ,a ,a ,a)))))
          (('and clause1 clause2 ...)
           (expand
            (let ((a (gensym)))
              `(let ((,a ,clause1))
                 (if ,a
                     (and ,@clause2)
                     ,a)))))
          (('lambda (bindings ...) e1 e2 rest ...)
           (expand `(lambda (,@bindings)
                      (begin ,e1 ,e2 ,@rest))))
          ((combination ...)
           (map expand combination))
          (_ e)))

;; convert to high-level AST
(define (convert-source e)
  (define (cs e)
    (match e
           (('if x y)
            (make <if> (convert-source x) (convert-source y) '()))
           (('if x y z)
            (make <if> (convert-source x) (convert-source y) (convert-source z)))
           (('if _ ...)
            (error convert-source "ill-formed conditional expression"))
           (('lambda (bindings ...) body)
            (make <lambda> bindings (convert-source body)))
           (('lambda _ ...)
            (error convert-source "ill-formed conditional expression"))
           ((? null?)
            (make <constant> e))
           ((? boolean?)
            (make <constant> e))
           ((? number?)
            (make <constant> e))
           ((_ ...)
            (make <comb> (map convert-source e)))
           ((? symbol?)
            (make <variable> e))))
;;  (make <lambda> (list) (cs e)))
  (cs e))
(define builtins
  '(+ - * / < > <= = => car cdr cons null? list? boolean? number? string? pair?))

(define (find-mapping name scopes)
  (let f ((scopes scopes))
    (if (null? scopes)
        (if (memq name builtins) name)
        (let ((mapping (assq name (car scopes))))
          (if mapping
              (cdr mapping)
              (f (cdr scopes)))))))
  
(define (alpha-convert node scopes)
  (match-object node
    ((<lambda> args body)
     (let ((mappings (map (lambda (arg) (cons arg (gensym))) args)))
       (make <lambda>
         (map cdr mappings)
         (alpha-convert body (cons mappings scopes)))))
    ((<if> test conseq altern)
     (make <if>
       (alpha-convert test scopes)
       (alpha-convert conseq scopes)
       (alpha-convert altern scopes)))
    ((<comb> args)
     (make <comb>
       (map (lambda (arg) (alpha-convert arg scopes)) args)))
    ((<variable> name)
     (make <variable> (find-mapping name scopes)))
    (else node)))
    

(define (cps-convert node cont)
  (match-object node
    ((<constant>)
     (if (null? cont) node
         (make <comb> (list cont node))))
    ((<variable>)
     (if (null? cont) node
         (make <comb> (list cont node))))
    ((<lambda> args body)
     (let* ((tn (gensym))
            (fn (make <lambda>
                  `(,@args ,tn)
                  (cps-convert body (make <variable> tn))))) 
       (if (null? cont) fn
           (make <comb> (list cont fn)))))
    ((<comb> args)
     (let f ((x args) (y '()) (z '()))
       (cond
        ((null? x)
         (let g ((form (make <comb> (reverse (cons cont y)))) (y y) (z z))
           (if (null? z)
               form
               (if (null? (car z))
                   (g form (cdr y) (cdr z))
                   (g (cps-convert (car z) (make <lambda> (list (slot-ref (car y) 'name)) form))
                      (cdr y)
                      (cdr z))))))
        ((is-a? (car x) <constant>)
         (f (cdr x) (cons (car x) y) (cons '() z)))
        ((is-a? (car x) <variable>)
         (f (cdr x) (cons (car x) y) (cons '() z)))
        ((is-a? (car x) <lambda>)
         (f (cdr x) (cons (cps-convert (car x) '()) y) (cons '() z)))
        (else 
         (f (cdr x) (cons (make <variable> (gensym)) y) (cons (car x) z))))))
    ((<if> test conseq altern)
     (let ((kn (gensym)))
       (make <comb>
         (list
          (make <lambda> (list kn)  
            (cps-convert test
              (let ((tn (gensym)))
                (make <lambda> (list tn)
                  (make <if>
                    (make <variable> tn)
                    (cps-convert conseq (make <variable> kn))
                    (cps-convert altern (make <variable> kn)))))))
          cont))))
    (else (error 'cps-convert "not an AST node" node))))

(define (annotate-free-vars node)
  (let ((union (lambda lists
                 (apply lset-union (cons eq? lists))))
        (diff  (lambda lists
                 (apply lset-difference (cons eq? lists)))))
    (match-object node
      ((<variable> name)
       (diff (list name) builtins))
      ((<constant>) '())
      ((<if> test conseq altern)
       (union
        (annotate-free-vars test)
        (annotate-free-vars conseq)
        (annotate-free-vars altern)))
      ((<comb> args)
       (let f ((x '()) (args args))
         (if (null? args)
             x
             (f (union x (annotate-free-vars (car args))) (cdr args)))))
      ((<lambda> args body)
       (slot-set! node
                  'free-vars
                  (diff (annotate-free-vars body) args))
       (slot-ref node 'free-vars))
      (else (error "not an AST node" node)))))

(define (closure-index name names)
  (let f ((i 1) (names names))
    (if (null? names)
        (error "should not reach here") 
        (if (eq? name (car names))
            i
            (f (+ i 1) (cdr names))))))

(define (closure-convert-body node c-name free-vars)
  (match-object node
    ((<lambda> args body free-vars)
     (let ((c-name (gensym 'c)))
       (let ((mk (make <lambda> (cons c-name args) (closure-convert-body body c-name free-vars))))
         (slot-set! mk 'free-vars free-vars)
         mk)))
    ((<comb> args)
     (let f ((x args) (y '()) (z '()))
       (cond
        ((null? x)
         (let g ((cexp (make <comb> (reverse y))) (z z))
           (if (null? z)
               cexp
               (g (make <select>
                    (closure-index (caar z) free-vars)
                    (make <variable> c-name)
                    (make <variable> (cdar z))
                    cexp)
                  (cdr z)))))
         ((is-a? (car x) <variable>)
          (let ((name (slot-ref (car x) 'name)))
            (if (memq name free-vars)
                (let ((exists (assq name z)) (temp (gensym 't)))   
                  (if exists
                    (f (cdr x) (cons (make <variable> (cdr exists)) y) z)
                    (f (cdr x) (cons (make <variable> temp) y) (cons (cons name temp) z))))
                (f (cdr x) (cons (car x) y) z))))
         (else (f (cdr x) (cons (closure-convert-body (car x) #f '()) y) z)))))
    ((<variable> name)
     (if (memq name free-vars)
         (let ((temp (gensym 't)))
           (make <select>
             (closure-index name free-vars)
             (make <variable> c-name)
             (make <variable> temp)
             (make <variable> temp)))
         node))
    ((<if> test conseq altern)
     (make <if>
       (closure-convert-body test c-name free-vars)
       (closure-convert-body conseq c-name free-vars)
       (closure-convert-body altern c-name free-vars)))
    (else node)))

(define (flatten node)
  (letrec ((queue  (list))
           (queue-push! (lambda (node)
                          (set! queue
                                (append queue (list node)))))
           (queue-pop!  (lambda ()
                          (let ((front (car queue)))
                            (set! queue
                                  (cdr queue))
                           front)))
           (queue-empty? (lambda ()
                           (null? queue)))
           (flatten-node (lambda (node)
                           (match-object node
                             ((<lambda> args body free-vars)
                              (let ((fname (gensym 'f)))
                                (queue-push! (cons node fname))
                                (let ((temp (gensym 'c)))
                                  (make <record> (map (lambda (t)
                                                   (make <variable> t))
                                                 (cons fname free-vars))
                                            (make <variable> temp)
                                            (make <variable> temp)))))
                             ((<if> test conseq altern)
                              (make <if>
                                (flatten-node test)
                                (flatten-node conseq)
                                (flatten-node altern)))
                             ((<comb> args)
                              (make <comb> (map flatten-node args)))
                             ((<select> index record name cexp)
                              (make <select>
                                index
                                record
                                name
                                (flatten-node cexp)))
                             (else node)))))
    (let f ((labels (list (make <label> (gensym 'f) (list) (flatten-node node)))))
      (if (queue-empty?)
          labels
          (let* ((pair (queue-pop!))
                 (label (cdr pair))
                 (node (car pair))
                 (flattened (flatten-node (slot-ref node 'body))))
            (f (cons (make <label>
                       label
                       (slot-ref node 'args)
                       flattened)
                     labels)))))))

(define (closure-convert node)
  (closure-convert-body node #f '()))