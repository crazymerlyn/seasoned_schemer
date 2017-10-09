(load "shadows.scm")
(define (set? l)
  (cond ((null? l) #t)
        ((member? (car l) (cdr l)) #f)
        (else (set? (cdr l)))))

(define (makeset l)
  (cond ((null? l) '())
        (else (cons (car l)
                    (makeset
                      (multi-rember (car l) (cdr l)))))))

(define (subset? s1 s2)
  (if (null? s1)
      #t
      (and (member? (car s1) s2)
           (subset? (cdr s1) s2))))

(define (eqset? s1 s2)
  (and (subset? s1 s2)
       (subset? s2 s1)))

(define (intersect? s1 s2)
  (if (null? s1)
      #f
      (or (member? (car s1) s2)
          (intersect? (cdr s1) s2))))

(define (intersect s1 s2)
  (cond ((null? s1) '())
        ((member? (car s1) s2)
         (cons (car s1) (intersect (cdr s1) s2)))
        (else (intersect (cdr s1) s2))))

(define (union s1 s2)
  (cond ((null? s1) s2)
        ((member? (car s1) s2)
         (union (cdr s1) s2))
        (else (cons (car s1) (union (cdr s1) s2)))))

(define (intersect-all lset)
  (cond ((null? (cdr lset)) (car lset))
        (else (intersect (car lset)
                         (intersect-all (cdr lset))))))

(define (a-pair? l)
  (cond ((atom? l) #f)
        ((null? l) #f)
        ((null? (cdr l)) #f)
        (else (null? (cddr l)))))

(define (first p) (car p))
(define (second p) (cadr p))
(define (build s1 s2) (cons s1 (cons s2 '())))

(define (fun? rel)
  (set? (firsts rel)))

(define (revrel rel)
  (if (null? rel)
      '()
      (cons (build (second (car rel))
                   (first (car rel)))
            (revrel (cdr rel)))))

(define (fullfun? fun)
  (set? (seconds fun)))
