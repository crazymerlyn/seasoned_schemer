(define ingredients '())
(define (sweet-toothR food)
  (set! ingredients (cons food ingredients))
  (cons food (cons 'cake '())))

(define (Y-bang f)
  (letrec ((h (f (lambda (arg) (h arg)))))
    h))

(define (L length)
  (lambda (l)
    (if (null? l) 0 (+ 1 (length (cdr l))))))

(define len (Y-bang L))

(define biz
  (let ((x 0))
   (lambda (f)
     (set! x (+ 1 x))
     (lambda (a)
       (if (= a x) 0 (f a))))))

(define B (Y-bang biz))
