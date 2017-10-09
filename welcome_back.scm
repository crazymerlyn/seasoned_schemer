(define (two-in-a-row? lat)
  (cond ((null? lat) #f)
        (else
          (two-in-a-row-b? (car lat) (cdr lat)))))

(define (two-in-a-row-b? preceding lat)
  (cond ((null? lat) #f)
        ((eq? preceding (car lat)) #t)
        (else (two-in-a-row-b? (car lat) (cdr lat)))))
