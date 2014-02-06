(define atom?
  (lambda (x)
	(and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
	(cond
	  ((null? l) #t)
	  ((atom? (car l)) (lat? (cdr l)))
	  (else #f))))

(define member?
  (lambda (a lat)
	(cond
	  ((null? lat) #f)
	  (else (or (eq? a (car lat)) (member? a (cdr lat))) #t))))

(define rember
  (lambda (a lat)
	(cond
	  ((null? lat) '())
	  ((eq? a (car lat)) (cdr lat))
	  (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
	(cond
	  ((null? l) '())
	  (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  (else
		(cond
		  ((eq? old (car lat)) (cons old (cons new (cdr lat))))
		  (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  (else
		(cond
		  ((eq? old (car lat)) (cons new lat))
		  (else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  (else
		(cond
		  ((eq? old (car lat)) (cons new (cdr lat)))
		  (else (cons (car lat) (subst new old (cdr lat)))))))))

(define subst2
  (lambda (new old1 old2 lat)
	(cond 
	  ((null? lat) '())
	  (else
		(cond
		  ((or (eq? old1 (car lat)) (eq? old2 (car lat))) (cons new (cdr lat)))
		  (else (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))))

(define multirember
  (lambda (a lat)
	(cond
	  ((null? lat) '())
	  (else
		(cond
		  ((eq? a (car lat)) (multirember a (cdr lat)))
		  (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
	  (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
	  (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
	  (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
  (lambda (n)
	(+ n 1)))

(define sub1
  (lambda (n)
	(- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
	(cond
	  ((zero? m) 0)
	  (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2) 
    (cond
      ((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o< n m) #f)
      ((o> n m) #f)
      (else #t))))

(define o^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (exponent n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
      ((> m n) 0)
      (else (add1 (o/ (o- n m) m))))))

(define olength
  (lambda (lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (a)
    (o= a 1)))