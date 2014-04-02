;; Preparing ourselves

(define atom?
  (lambda (x)
	(and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
	(+ n 1)))

(define sub1
  (lambda (n)
	(- n 1)))

;; Begin Chapter 11!

;; We remember member

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

(define old-two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (is-first-b? (car lat) (cdr lat))))))

;; First attempt at is-first?

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? a (car lat))))))

;; Decision making in is-first? instead of two-in-a-row?

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (old-two-in-a-row? lat))))))

;; 

(define two-in-a-row?
  (lambda 