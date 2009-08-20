
(declare (unit class))

(use matchable)
(use srfi-1)

;; class protocol

(define-record wrapper object)

(define-record-printer (wrapper x port)
  (fprintf port "#,(instance ~S)" (class-name (class-of x))))

(define unwrap-instance
  (lambda (wrapped)
    ((wrapper-object wrapped))))

(define wrap-instance
  (lambda (object)
    (make-wrapper (lambda () object))))

(define instance-marker 'instance-marker)

(define identity-initializer (lambda (o) o))

(define (class-of x)
  (instance-ref x 0))

(define (class-name x)
  (instance-ref x 1))

(define (class-super x)
  (instance-ref x 2))

(define (class-slot-names x)
  (instance-ref x 3))

(define (class-accessors x)
  (instance-ref x 4))

(define (class-initializer x)
  (instance-ref x 5))

(define (instance? x)
  (and (wrapper? x)
       (let ((x (wrapper-object x)))
         (and (wrapper? x)
              (vector? x)
              (< 0 (vector-length x))
              (eq? (vector-ref x 0) instance-marker)))))

(define (instance-set! x i v)
  (vector-set! (unwrap-instance x) (+ i 1) v))

(define (instance-ref x i)
  (vector-ref (unwrap-instance x) (+ i 1)))

(define (allocate-instance size)
  (let* ((object (make-vector (+ 1 size) '())))
    (vector-set! object 0 instance-marker)
    (wrap-instance object)))
    
  
(define slots-for-class '(name super slot-names accessors initializer))

(define <class> (allocate-instance (+ 1 (length slots-for-class))))

(let ((accessors
       (let f ((slots slots-for-class) (i 0) (x '()))
         (if (null? slots) x
             (f (cdr slots)
                (+ i 1)
                (cons (list (car slots)
                            (lambda (o)   (instance-ref o (+ i 1)))
                            (lambda (o v) (instance-set! o (+ i 1) v)))
                      x))))))
  (instance-set! <class> 0 <class>)
  (instance-set! <class> 1 '<class>)
  (instance-set! <class> 2 '())
  (instance-set! <class> 3 slots-for-class)
  (instance-set! <class> 4 accessors)
  (instance-set! <class> 5 (lambda (o) o)))

(define (make-class name super slot-names initializer)
  (let* ((slots
          (let f ((slots slot-names) (super super))
            (if (null? super) slots
                (f (append (class-slot-names super) slots)
                   (class-super super)))))
         (accessors
          (let f ((slots slots) (i 0) (x '()))
            (if (null? slots) x
                (f (cdr slots)
                   (+ i 1)
                   (cons (list (car slots)
                           (lambda (o)   (instance-ref o (+ i 1)))
                           (lambda (o v) (instance-set! o (+ i 1) v)))
                         x)))))
         (new (allocate-instance (+ 1 (length slots-for-class)))))
    (instance-set! new 0 <class>)
    (instance-set! new 1 name)
    (instance-set! new 2 super)
    (instance-set! new 3 slot-names)
    (instance-set! new 4 accessors)
    (instance-set! new 5 (initializer new))
    new))

(define (make class . args)
  (cond
   ((eq? class <class>)
    (match-let (((name super slot-names initializer) args))
      (make-class name super slot-names initializer)))
   (else
    (let ((new (allocate-instance (+ 1 (length (class-accessors class)))))) 
      (instance-set! new 0 class)
      (apply (class-initializer class) (cons new args))
      new))))

(define (slot-ref x name)
  (let ((info (assq name
                    (class-accessors (class-of x)))))
    (if info ((second info) x)
        (error "no such slot found" name))))

(define (slot-set! x name value)
  (let ((info (assq name
                    (class-accessors (class-of x)))))
    (if info ((third info) x value)
        (error "no such slot found" name))))

(define (subclass? x y)
  (cond
   ((null? x) #f)
   ((eq? x y) #t)
   (else (subclass? (class-super x) y))))

(define (is-a? x y)
  (subclass? (class-of x) y))

(define <top>
  (make <class> '<top> '() '() (lambda (class) (lambda (o) o))))
