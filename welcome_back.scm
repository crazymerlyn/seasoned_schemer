(load "numbers.scm")
(define (two-in-a-row? lat)
  (cond ((null? lat) #f)
        (else
          (two-in-a-row-b? (car lat) (cdr lat)))))

(define (two-in-a-row-b? preceding lat)
  (cond ((null? lat) #f)
        ((eq? preceding (car lat)) #t)
        (else (two-in-a-row-b? (car lat) (cdr lat)))))

(define (sum-of-prefixes-b sum tup)
  (cond ((null? tup) '())
        (else (cons (+ sum (car tup))
                    (sum-of-prefixes-b (+ sum (car tup))
                                       (cdr tup))))))

(define (sum-of-prefixes tup) (sum-of-prefixes-b 0 tup))

(define (scramble-b tup rev-pre)
  (cond ((null? tup) '())
        (else
          (cons (pick (car tup)
                      (cons (car tup) rev-pre))
                (scramble-b (cdr tup) (cons (car tup) rev-pre))))))

(define (scramble tup) (scramble-b tup '()))
