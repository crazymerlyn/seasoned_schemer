(load "friends_and_relations.scm")

(define new-entry build)

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help
    name (first entry) (second entry) entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond ((null? names) (entry-f name))
        ((eq? name (car names)) (car values))
        (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f))))

(define extend-table cons)

(define (lookup-in-table name table table-f)
  (if (null? table)
      (table-f name)
      (lookup-in-entry
        name (car table)
        (lambda (name)
          (lookup-in-table name (cdr table) table-f)))))

(define (expression-to-action e)
  (if (atom? e)
      (atom-to-action e)
      (list-to-action e)))

(define (atom-to-action e)
  (cond ((number? e) *const)
        ((eq? e #t) *const)
        ((eq? e #f) *const)
        ((eq? e 'cons) *const)
        ((eq? e 'car) *const)
        ((eq? e 'cdr) *const)
        ((eq? e 'null?) *const)
        ((eq? e 'eq?) *const)
        ((eq? e 'atom?) *const)
        ((eq? e 'zero?) *const)
        ((eq? e 'add1) *const)
        ((eq? e 'sub1) *const)
        ((eq? e 'number?) *const)
        (else *identifier)))

(define (list-to-action e)
  (cond ((atom? (car e))
         (cond ((eq? (car e) 'quote) *quote)
               ((eq? (car e) 'lambda) *lambda)
               ((eq? (car e) 'cond) *cond)
               (else *application)))
        (else *application)))

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*const e table)
  (cond ((number? e) e)
        ((eq? e #t) #t)
        ((eq? e #f) #f)
        (else (build 'primitive e))))

(define (*quote e table)
  (text-of e))

(define text-of second)

(define (*identifier e table)
  (lookup-in-table e table value-not-found))

(define (value-not-found name)
  '())

(define (*lambda e table)
  (build 'non-primitive
         (cons table (cdr e))))

(define table-of car)
(define formals-of cadr)
(define body-of caddr)

(define (evcon lines table)
  (cond ((else? (question-of (car lines)))
         (meaning (answer-of (car lines))
                  table))
        ((meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines))
                  table))
        (else (evcon (cdr lines) table))))

(define (else? x)
  (and (atom? x) (eq? x 'else)))

(define question-of car)
(define answer-of cadr)

(define (*cond e table)
  (evcon (cond-lines e) table))
(define cond-lines cdr)

(define (evlis es table)
  (if (null? es)
      '()
      (cons (meaning (car es) table)
            (evlis (cdr es) table))))

(define (*application e table)
  (*apply (meaning (function-of e) table)
          (evlis (arguments-of e) table)))

(define function-of car)
(define arguments-of cdr)

(define (primitive? f) (eq? (car f) 'primitive))
(define (non-primitive? f) (eq? (car f) 'non-primitive))

(define (*apply fun vals)
  (cond ((primitive? fun)
         (apply-primitive (second fun) vals))
        ((non-primitive? fun)
         (apply-closure (second fun) vals))))

(define (apply-primitive name vals)
  (cond ((eq? name 'cons)
         (cons (first vals) (second vals)))
        ((eq? name 'car)
         (car (first vals)))
        ((eq? name 'cdr)
         (cdr (first vals)))
        ((eq? name 'null?)
         (null? (first vals)))
        ((eq? name 'eq?)
         (eq? (first vals) (second vals)))
        ((eq? name 'atom?)
         (atom? (first vals)))
        ((eq? name 'zero?)
         (zero? (first vals)))
        ((eq? name 'add1)
         (add1 (first vals)))
        ((eq? name 'sub1)
         (sub1 (first vals)))
        ((eq? name 'number?)
         (number? (first vals)))))

(define (apply-closure e vals)
  (meaning (body-of e)
           (extend-table (new-entry (formals-of e) vals)
                         (table-of e))))
