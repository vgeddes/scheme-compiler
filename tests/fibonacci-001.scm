;; fibonacci
;; we haven't implemented 'letrec' or 'define' yet, so we get around that using some tricks as seen below

(let ((fib (lambda (n f)
               (if (fx<= n 2)
                   1
                   (fx+ (f (fx- n 2) f) (f (fx- n 1) f))))))
    (fib 23 fib))
