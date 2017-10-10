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

(define (member? a lat)
  (letrec ((yes? (lambda (lat)
                   (cond ((null? lat) #f)
                         (else (or (eq? a (car lat))
                                   (yes? (cdr lat))))))))
    (yes? lat)))

(define (union s1 s2)
  (letrec ((union1 (lambda (s1)
                     (cond ((null? s1) s2)
                           ((member? (car s1) s2) (union1 (cdr s1)))
                           (else (cons (car s1) (union1 (cdr s1)))))))
           (member? (lambda (a lat)
                      (cond ((null? lat) #f)
                            (else (or (eq? (car lat) a)
                                      (member? a (cdr lat))))))))
    (union1 s1)))
