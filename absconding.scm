(load "base.scm")
(define leave)
(define (walk l)
  (cond ((null? l) '())
        ((atom? (car l)) (leave (car l)))
        ((else
           (let ()
            (walk (car l))
            (walk (cdr l)))))))
(define (start-it l)
  (call-with-current-continuation
    (lambda (here)
      (set! leave here)
      (walk l))))
