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

(define (waddle l)
  (cond ((null? l) '())
        ((atom? (car l))
         (let ()
           (call-with-current-continuation
             (lambda (rest)
               (set! fill rest)
               (leave (car l))))
           (waddle (cdr l))))
        (else
          (let ()
           (waddle (car l))
           (waddle (cdr l))))))

(define (start-it2 l)
  (call-with-current-continuation
    (lambda (here)
      (set! leave here)
      (waddle l))))
