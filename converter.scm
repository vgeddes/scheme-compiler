


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
;;
  
(define (convert-source e)
  (define (cs e)
    (match e
      (('if x y)
       (make <if> (cs x) (cs y) '()))
      (('if x y z)
       (make <if> (cs x) (cs y) (cs z)))
      (('if _ ...)
       (error cs "ill-formed conditional expression"))
      (('lambda (bindings ...) body)
       (make <lambda> (gensym 'f) bindings (cs body)))
      (('lambda _ ...)
       (error cs "ill-formed conditional expression"))
      ((? null?)
       (make <constant> e))
      ((? boolean?)
       (make <constant> e))
      ((? number?)
       (make <constant> e))
      ((_ ...)
     (make <comb> (map cs e)))
      ((? symbol?)
       (make <variable> e))))
  (make <lambda> (gensym 'f) (list) (cs e)))
  
(define (make-normal-lambda node)
  ;; raise all lambda definitions to the top of the this lambda body
  (define (collect node)
    ;; collect nested lambda nodes in this scope
    (match-object node
      ((<lambda>)
       (list node))
      ((<if> test conseq altern)
       (append (collect test) (collect conseq) (collect altern)))
      ((<comb> args)
        (apply append (map collect args)))
      (else (list))))
  (define (rewrite node)
    ;; rewrite tree, replacing lambda nodes with their names
    (match-object node
      ((<lambda> name)
       (make <variable> name))
      ((<if> test conseq altern)
       (make <if> (rewrite test) (rewrite conseq) (rewrite altern)))
      ((<comb> args)
       (make <comb> (map rewrite args)))
      (else node)))
  (define (normalize node)
    (let* ((name (slot-ref node 'name))
           (args (slot-ref node 'args))
           (body (slot-ref node 'body))
           (defs (map normalize (collect body))))
      (if (null? defs)
          node
          (make <lambda> name args (make <fix> defs (rewrite body))))))
  (let ((node (normalize node)))
    (make <fix> (list node)
      (make <comb>
        (list (make <variable> (slot-ref node 'name)))))))
      
  
(define builtins
  '(+ - * / < > <= = => car cdr cons null? list? boolean? number? string? pair?))

(define (find-mapping name scopes)
  (let f ((scopes scopes))
    (if (null? scopes)
        (if (memq name builtins) name name)
        (let ((mapping (assq name (car scopes))))
          (if mapping
              (cdr mapping)
              (f (cdr scopes)))))))
  
(define (alpha-convert node scopes)
  (match-object node
    ((<fix> defs body)
     (make <fix>
       (map (lambda (def)
              (alpha-convert def scopes))
            defs)
       (alpha-convert body scopes)))
    ((<lambda> name args body)
     (let ((mappings (map (lambda (arg) (cons arg (gensym 't))) args)))
       (make <lambda>
         name
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
    ((<fix> defs body)
     (make <fix>
       (map (lambda (def)
              (cps-convert def '()))
            defs)
       (cps-convert body cont)))
    ((<lambda> name args body)
     (let* ((cn (gensym 'c)))
       (make <lambda>
         name
         `(,@args ,cn)
         (cps-convert body (make <variable> cn)))))
    ((<comb> args)
     ;; x - combination args
     ;; y - new args for combination (x -> y)
     ;; z - args which are not atoms
     ;; w - names for continuations
     (let f ((x args) (y '()) (z '()) (w '()))
       (cond
        ((null? x)
         (let g ((form (make <comb> (reverse (cons cont y)))) (y y) (z z) (w w))
           (if (null? z)
               form
               (if (null? (car z))
                   (g form (cdr y) (cdr z) (cdr w))
                   (g (make <fix>
                        (list
                          (make <lambda>
                            (car w)
                            (list (slot-ref (car y) 'name))
                            form))
                        (cps-convert (car z) (make <variable> (car w))))
                      (cdr y)
                      (cdr z)
                      (cdr w))))))
        ((is-a? (car x) <constant>)
         (f (cdr x) (cons (car x) y) (cons '() z) (cons '() w)))
        ((is-a? (car x) <variable>)
         (f (cdr x) (cons (car x) y) (cons '() z) (cons '() w)))
        (else
         (f (cdr x) (cons (make <variable> (gensym 't)) y) (cons (car x) z) (cons (gensym 'c) w))))))
    ((<if> test conseq altern)
     (let ((kn (gensym 't))
           (fn (gensym 'f))
           (cn (gensym 'c))
           (tn (gensym 't)))
       (make <fix>
         (list (make <lambda> fn (list kn)
                 (make <fix>
                   (list (make <lambda> cn (list tn)
                           (make <if>
                             (make <variable> tn)
                             (cps-convert conseq (make <variable> kn))
                             (cps-convert altern (make <variable> kn)))))
                   (cps-convert test (make <variable> cn)))))
         (make <comb> (list (make <variable> fn) cont)))))
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
      ((<fix> defs body)
       (apply union (append (map annotate-free-vars defs)
                            (list (diff (annotate-free-vars body)
                                        (map (lambda (def)
                                               (slot-ref def 'name))
                                             defs))))))
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
    ((<fix> defs body)
     (make <fix>
       (map (lambda (def)
              (closure-convert-body def c-name free-vars))
            defs)
       (closure-convert-body body c-name free-vars)))
    ((<lambda> name args body free-vars)
     (let ((cn (gensym 'c)))
       (let ((converted (make <lambda> name (cons cn args) (closure-convert-body body cn free-vars))))
         (slot-set! converted 'free-vars free-vars)
         converted)))
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
                             ((<fix> defs body)
                              (let f ((x defs) (y '()) (form body))
                                (cond
                                 ((null? x)
                                  form)
                                 (else (f (cdr x)
                                          (make <record>
                                            (map (lambda (t)
                                                   (make <variable> t))
                                                 (cons fname free-vars))
                                            (make <variable> (gensym 'r))
                                            form))
                              
                             ((<lambda> args body free-vars)
                              (queue-push! node)
                              (let ((rn (gensym 'r)))
                                (make <record>
                                  (map (lambda (t)
                                         (make <variable> t))
                                       (cons fname free-vars))
                                  (make <variable> rn)
                                  (make <variable> rn))))
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
          (make <fix> labels 
          (let* ((node (queue-pop!))
                 (body (flatten-node (slot-ref node 'body))))
            (f (cons (make <lambda>
                       (slot-ref node 'name)
                       (slot-ref node 'args)
                       body)
                     labels)))))))

(define (closure-convert node)
  (closure-convert-body node #f '()))