(load "base.scm")

(define (looking a lat)
  (keep-looking a (pick 0 lat) lat))

(define (keep-looking needle to-check lat)
  (cond ((eq? needle to-check) #t)
        ((number? to-check)
         (keep-looking needle (pick to-check lat) lat))
        (else #f)))

(define (shift l)
  (build (first (first l)) (build (second (first l)) (second l))))

(define (align pora)
  (cond ((atom? pora) pora)
        ((a-pair? (first pora))
         (align (shift pora)))
        (else (build (first pora)
                     (align (second pora))))))
