(load "numbers.scm")
(load "base.scm")
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

(define (intersect s1 s2)
  (letrec ((intersect1 (lambda (s1)
                     (cond ((null? s1) '())
                           ((member? (car s1) s2) (cons (car s1) (intersect1 (cdr s1))))
                           (else (intersect1 (cdr s1)))))))
    (intersect1 s1)))

(define (intersect-all lset)
  (call-with-current-continuation
    (lambda (hop)
      (letrec ((A (lambda (lset)
                    (cond ((null? (car lset)) (hop '()))
                          ((null? (cdr lset)) (car lset))
                          (else (intersect (car lset)
                                           (A (cdr lset))))))))
        (cond ((null? lset) '())
              (else (A lset)))))))

(define (rember-beyond-first a lat)
  (letrec ((R (lambda (lat)
                (cond ((null? lat) '())
                      ((eq? a (car lat)) '())
                      (else (cons (car lat) (R (cdr lat))))))))
    (R lat)))

(define (rember-upto-last a lat)
  (call-with-current-continuation
    (lambda (skip)
      (letrec ((R (lambda (lat)
                    (cond ((null? lat) '())
                          ((eq? a (car lat)) (skip (R (cdr lat))))
                          (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))

(define (leftmost l)
  (cond ((null? l) '())
        ((atom? (car l)) (car l))
        (else
          (let ((first (leftmost (car l))))
           (if (atom? first)
               first
               (leftmost (cdr l)))))))
