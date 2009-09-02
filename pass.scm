


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

(define (primitive? op)
  (case op
    ((+ - * / < > <= >= =  car cdr cons null? list? boolean? number? pair?) #t)
    (else #f)))

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
  (cs e))


(define select-matching
  (lambda (fn lst)
    (reverse
     (fold (lambda (x acc)
             (if (fn x)
                 (cons x acc)
                 acc))
           (list)
           lst))))


  
(define builtins
  '(+ - * / < > <= >= = car cdr cons null? list? boolean? number? string? pair?))

(define (find-mapping name scopes)
  (let f ((scopes scopes))
    (if (null? scopes)
        (if (primitive? name)
            name
            (error 'alpha-convert "symbol not found" name))
        (let ((mapping (assq name (car scopes))))
          (if mapping
              (cdr mapping)
              (f (cdr scopes)))))))
  
(define (alpha-convert node)
  (define (alpha-convert node scopes)
    (match-object node
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
      ((<prim> name args cexp)
       (make <prim>
         name
         (map (lambda (arg) (alpha-convert arg scopes)) args)
         (alpha-convert cexp scopes)))
      ((<comb> args)
       (make <comb>
         (map (lambda (arg) (alpha-convert arg scopes)) args)))
      ((<variable> name)
       (make <variable> (find-mapping name scopes)))
      (else node)))
  (alpha-convert node (list)))

(define (cps-convert node)
  (define (cps-convert node cont)
    (match-object node
      ((<constant>)
       (if (null? cont)
           node
           (make <comb> (list cont node))))
      ((<variable>)
       (if (null? cont)
           node
           (make <comb> (list cont node))))
      ((<lambda> name args body)
       (let* ((cn (gensym 'c))
              (node-cps (make <lambda>
                          name
                          `(,@args ,cn)
                          (cps-convert body (make <variable> cn)))))
         (if (null? cont)
             node-cps
             (make <comb> (list cont node-cps)))))
      ((<comb> args)
       ;; x - combination args
       ;; y - new args for combination (x -> y)
       ;; z - args which are not atoms
       ;; w - names for continuations
       (let f ((x args) (y '()) (z '()) (u '()) (w '()))
         (cond
          ((null? x)
           (let g ((form (make <comb> (reverse (cons cont y)))) (y y) (z z) (u u) (w w))
             (if (null? z)
                 form
                 (g
                  (cps-convert (car z)
                               (make <lambda>
                                 (car w)
                                 (list (car u))
                                 form))
                  (cdr y)
                  (cdr z)
                  (cdr u)
                  (cdr w)))))
          ((is-a? (car x) <constant>)
           (f (cdr x) (cons (car x) y) z u w))
          ((is-a? (car x) <variable>)
           (f (cdr x) (cons (car x) y) z u w))
          ((is-a? (car x) <lambda>)
           (f (cdr x) (cons (cps-convert (car x) '()) y) z u w))
          (else
           (let ((nm (gensym 't)))
             (f (cdr x) (cons (make <variable> nm) y) (cons (car x) z) (cons nm u) (cons (gensym 't) w)))))))
      ((<if> test conseq altern)
       (let ((kn (gensym 't))
             (fn (gensym 'f))
             (cn (gensym 'f))
             (tn (gensym 't)))
         (make <comb>
           (list (make <lambda> fn (list kn)
                       (cps-convert test
                                    (make <lambda> cn (list tn)
                                          (make <if>
                                            (make <variable> tn)
                                            (cps-convert conseq (make <variable> kn))
                                            (cps-convert altern (make <variable> kn))))))
                 cont))))
      (else (error 'cps-convert "not an AST node" node))))
  (let ((cn (gensym 'c))
        (tn (gensym 't)))
    (cps-convert node (make <lambda> cn (list tn) (make <variable> tn)))))

(define (identify-primitives cexp)
  (match-object cexp
    ((<variable>)
     cexp)
    ((<constant>)
     cexp)
    ((<if> test conseq altern)
     (make <if>
        (identify-primitives test)
        (identify-primitives conseq)
        (identify-primitives altern)))
    ((<comb> args)
     (if (primitive? (slot-ref (car args) 'name))
         (let* ((cont (car (reverse args)))
                (result (cond
                         ((is-a? cont <lambda>)
                          (make <variable> (car (slot-ref cont 'args))))
                         ((is-a? cont <variable>)
                          (make <variable> (gensym 't)))
                         (else (error 'identify-primitives))))
                (new-cont (cond
                           ((is-a? cont <lambda>)
                            (identify-primitives (slot-ref cont 'body)))
                           ((is-a? cont <variable>)
                            (make <app> cont (list result)))
                           (else (error 'identify-primitives)))))
           (make <prim>
             (car args)
             (cdr (reverse (cdr args)))
             result
             new-cont))
         (make <app> (identify-primitives (car args)) (map identify-primitives (cdr args)))))
      ((<lambda> name args body)
       (make <lambda>
         name
         args
         (identify-primitives body)))
      (else (error 'identify-primitives "not an AST node" cexp))))


(define (basic-lambda-lift node)
  ;; raise all lambda definitions to the top of the this lambda body
  (define (filter lst)
    (select-matching
      (lambda (x)
       (and (instance? x) (is-a? x <lambda>)))
      lst))
  (define (collect node)
    ;; collect nested lambda nodes in this scope
    (match-object node
      ((<lambda>)
       (list node))
      ((<prim> args cexp)
       (append (filter args) (collect cexp)))
      ((<if> test conseq altern)
       (append (collect test) (collect conseq) (collect altern)))
      ((<app> name args)
       (filter (cons name args)))
      (else (list))))
  (define (rewrite node)
    ;; rewrite tree, replacing lambda nodes with their names
    (match-object node
      ((<lambda> name)
       (make <variable> name))
      ((<prim> name args result cexp)
       (make <prim> name (map rewrite args) result (rewrite cexp)))
      ((<if> test conseq altern)
       (make <if> (rewrite test) (rewrite conseq) (rewrite altern)))
      ((<app> name args)
       (make <app> (rewrite name) (map rewrite args)))
      (else node)))
  (define (normalize node)
    (if (is-a? node <lambda>)
        (let* ((name (slot-ref node 'name))
               (args (slot-ref node 'args))
               (body (slot-ref node 'body))
               (defs (map normalize (collect body))))
     
          (if (null? defs)
              node
              (make <lambda> name args (make <fix> defs (rewrite body)))))
        (let* ((name (gensym 'f))
               (args (list))
               (body node)
               (defs (map normalize (collect body))))
          (if (null? defs)
              node
              (make <lambda> name args (make <fix> defs (rewrite body)))))))
  (let ((node (normalize node)))
    (make <fix> (list node)
          (make <app>
            (make <variable> (slot-ref node 'name)) (list)))))


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
      ((<app> name args)
       (let f ((x '()) (args (cons name args)))
         (if (null? args)
             x
             (f (union x (annotate-free-vars (car args))) (cdr args)))))
      ((<prim> args result cexp)
       (let f ((x (list)) (args args))
         (if (null? args)
             (union x (diff (annotate-free-vars cexp) (list (slot-ref result 'name))))
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
      (else (error 'annotate-free-vars "not an AST node" node)))))

(define (closure-index name names)
  (let f ((i 1) (names names))
    (if (null? names)
        (error "should not reach here") 
        (if (eq? name (car names))
            i
            (f (+ i 1) (cdr names))))))


(define (primitive-application args)
  (let ((fun (car args)))
    (if (memq (slot-ref fun 'name) builtins)
        (error 'primitive-application "should not reach here" (slot-ref fun 'name))
        (let ((fn (gensym 't)))
          (make <select> 0 fun (make <variable> fn)
                (make <app> (make <variable> fn) (cons fun (cdr args))))))))


(define (closure-convert-body node c-name free-vars)
  (match-object node
    ((<fix> defs body)
     (let ((old-names (map (lambda (def)
                             (slot-ref def 'name))
                           defs))
           (x (map (lambda (def)
                     (closure-convert-body def c-name free-vars))
                   defs)))
       (make <fix>
         x       
         (closure-convert-body
          (let f ((x x) (y old-names) (form body))
            (if (null? x)
                form
                (f (cdr x)
                   (cdr y)
                   (make <record>
                     (cons (make <variable> (slot-ref (car x) 'name))
                           (map (lambda (v) (make <variable> v))
                                (slot-ref (car x) 'free-vars)))
                     (make <variable> (car y))
                     form))))
          c-name free-vars))))
    ((<lambda> name args body free-vars)
     (let* ((cn (gensym 'c))
            (converted (make <lambda> (gensym 'f) (cons cn args) (closure-convert-body body cn free-vars))))
       (slot-set! converted 'free-vars free-vars)
       converted))
    ((<app> name args)
     (let f ((x (cons name args)) (y '()) (z '()))
       (cond
        ((null? x)
         (let g ((cexp (primitive-application (reverse y))) (z z))
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
   ((<prim> name args result cexp)
     (let f ((x args) (y (list)) (z (list)))
       (if (null? x)
           (let g ((cexp (make <prim> name (reverse y) result (closure-convert-body cexp c-name free-vars))) (z z))
             (if (null? z)
                 cexp
                 (g (make <select>
                      (closure-index (caar z) free-vars)
                      (make <variable> c-name)
                      (make <variable> (cdar z))
                      cexp)
                    (cdr z))))
           (if (is-a? (car x) <variable>)
               (let ((name (slot-ref (car x) 'name)))
                 (if (memq name free-vars)
                     (let ((exists (assq name z)) (temp (gensym 't)))   
                       (if exists
                    (f (cdr x) (cons (make <variable> (cdr exists)) y) z)
                    (f (cdr x) (cons (make <variable> temp) y) (cons (cons name temp) z))))
                     (f (cdr x) (cons (car x) y) z)))
               (f (cdr x) (cons (closure-convert-body (car x) #f '()) y) z)))))
   ((<record> values name cexp)
     (let f ((x values) (y (list)) (z (list)))
       (if (null? x)
           (let g ((cexp (make <record> (reverse y) name (closure-convert-body cexp c-name free-vars))) (z z))
             (if (null? z)
                 cexp
                 (g (make <select>
                      (closure-index (caar z) free-vars)
                      (make <variable> c-name)
                      (make <variable> (cdar z))
                      cexp)
                    (cdr z))))
           (let ((name (slot-ref (car x) 'name)))
             (if (memq name free-vars)
                 (let ((exists (assq name z)) (temp (gensym 't)))
                   (if exists
                       (f (cdr x) (cons (make <variable> (cdr exists)) y) z)
                       (f (cdr x) (cons (make <variable> temp) y) (cons (cons name temp) z))))
                 (f (cdr x) (cons (car x) y) z))))))
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

(define (closure-convert node)
  (closure-convert-body node #f '()))


#| * 1. create closure record for each lambda
   * 2. In fix body, replace each lambda reference (those not in the operator position) with closure reference
   * 3. rewrite lambda applications. Extract function label from closure and apply function to closure + args
 |#

(define (flatten node)
  (letrec ((queue (list))
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
                              (let f ((x defs))
                                (if (null? x) 
                                    body
                                    (begin
                                      (queue-push! (car x))
                                      (f (cdr x))))))
                             (else node)))))
    (map queue-push! (slot-ref node 'defs))
    (let f ((labels (list)))
      (if (queue-empty?)
          (make <fix> labels (slot-ref node 'body))
          (let* ((node (queue-pop!)))
            (f (cons (make <lambda>
                       (slot-ref node 'name)
                       (slot-ref node 'args)
                       (flatten-node (slot-ref node 'body)))
                       labels)))))))