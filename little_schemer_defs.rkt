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
	  (else (or (equal? a (car lat)) (member? a (cdr lat)))))))

(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((oequal? s (car l)) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

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
		  ((equal? a (car lat)) (multirember a (cdr lat)))
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
      (else (o* n (o^ n (sub1 m)))))))

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

(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? a (car lat)) (rember* a (cdr lat)))
         (else (cons (car lat) (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? old (car lat)) (cons old (cons new (insertR* new old (cdr lat)))))
         (else (cons (car lat) (insertR* new old (cdr lat))))))
      (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

(define occur*
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((atom? (car lat))
       (cond
         ((eq? a (car lat)) (add1 (occur* a (cdr lat))))
         (else (occur* a (cdr lat)))))
      (else (o+ (occur* a (car lat)) (occur* a (cdr lat)))))))

(define subst*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? old (car lat)) (cons new (subst* new old (cdr lat))))
         (else (cons (car lat) (subst* new old (cdr lat))))))
      (else (cons (subst* new old (car lat)) (subst* new old (cdr lat)))))))

(define insertL*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? old (car lat)) (cons new (cons old (insertL* new old (cdr lat)))))
         (else (cons (car lat) (insertL* new old (cdr lat))))))
      (else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(define member*
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat)) (or (eq? a (car lat)) (member* a (cdr lat))))
      (else (or (member* a (car lat)) (member a (cdr lat)))))))

(define leftmost
  (lambda (lat)
    (cond
      ((atom? (car lat)) (car lat))
      (else (leftmost (car lat))))))

(define eqlist?
  (lambda (ls1 ls2)
    (cond
      ((and (null? ls1) (null? ls2)) #t)
      ((or (null? ls1) (null? ls2) #f))
      (else (and (oequal? (car ls1) (car ls2)) (eqlist? (cdr ls1) (cdr ls2)))))))

(define oequal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else
       (eqlist? s1 s2)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr aexp))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +)) (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *)) (o* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      (else (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))


(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset) (intersectall (cdr lset)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define seconds
  (lambda (s)
    (cond
      ((null? s) '())
      (else (cons (second (car s)) (seconds (cdr s)))))))