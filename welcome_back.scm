(define (two-in-a-row? lat)
  (cond ((null? lat) #f)
        (else
          (or (is-first? (car lat) (cdr lat))
              (two-in-a-row? (cdr lat))))))

(define (is-first? a lat)
  (cond ((null? lat) #f)
        (else (eq? a (car lat)))))
