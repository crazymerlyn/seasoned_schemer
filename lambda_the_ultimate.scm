(load "base.scm")
(define (rember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? a (car l)) (cdr l))
          (else (cons (car l) (rember-f test? a (cdr l)))))))

(define (insert-f test? merge)
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((test? old (car lat)) (merge new old (cdr lat)))
          (else (cons (car lat) ((insertL-f test? merge) new old (cdr lat)))))))

(define (insertL-f test?)
  (insert-f test? (lambda (new old lat)
                    (cons new (cons old lat)))))

(define (insertR-f test?)
  (insert-f test? (lambda (new old lat)
                    (cons old (cons new lat)))))

(define (subst-f test?)
  (insert-f test? (lambda (new old lat)
                    (cons new lat))))

(define (multi-insertLR&co new oldL oldR lat col)
  (cond ((null? lat)
         (col '() 0 0))
        ((eq? oldL (car lat))
         (multi-insertLR&co
           new oldL oldR (cdr lat)
           (lambda (newlat left right)
             (col (cons new (cons oldL newlat))
                  (add1 left) right))))
        ((eq? oldR (car lat))
         (multi-insertLR&co
           new oldL oldR (cdr lat)
           (lambda (newlat left right)
             (col (cons oldR (cons new newlat))
                  left (add1 right)))))
        (else
          (multi-insertLR&co
            new oldL oldR (cdr lat)
            (lambda (newlat left right)
              (col (cons (car lat) newlat)
                   left right))))))

(define (even? n) (equal? (mul (quot n 2) 2) n))
(define (evens-only* l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((even? (car l))
                (cons (car l) (evens-only* (cdr l))))
               (else (evens-only* (cdr l)))))
        (else (cons (evens-only* (car l))
                    (evens-only* (cdr l))))))

(define (evens-only*&co l col)
  (cond ((null? l) (col '() 1 0))
        ((atom? (car l))
         (if (even? (car l))
             (evens-only*&co
               (cdr l)
               (lambda (newl even-mul odd-sum)
                 (col (cons (car l) newl)
                      (mul even-mul (car l))
                      odd-sum)))
             (evens-only*&co
               (cdr l)
               (lambda (newl even-mul odd-sum)
                 (col newl
                      even-mul
                      (add odd-sum (car l)))))))
        (else
          (evens-only*&co
            (car l)
            (lambda (newl even-mul odd-sum)
              (evens-only*&co
                (cdr l)
                (lambda (newl-rest even-mul-rest odd-sum-rest)
                  (col (cons newl newl-rest)
                       (mul even-mul even-mul-rest)
                       (add odd-sum odd-sum-rest)))))))))
