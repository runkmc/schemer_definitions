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
