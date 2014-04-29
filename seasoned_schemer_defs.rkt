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

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

;; We're going to need these as well for R5RS

(define-syntax letcc 
  (syntax-rules () 
    ((letcc var body ...) 
     (call-with-current-continuation 
       (lambda (var)  body ... ))))) 

(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (letcc success 
       (letcc var (success a)) . b)))) 

;; Begin Chapter 11!

;; We remember member

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

;; Let's find two in a row

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (or (is-first? (car lat) (cdr lat)) (two-in-a-row? (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))

;; Putting more responsibility into is-first?

(define two-in-a-row2?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (two-in-a-row2? lat))))))

;; Last run through

(define two-in-a-row-b?
  (lambda (preceeding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? preceeding (car lat)) (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row-final?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

;; Sum of prefixes

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else 
       (cons (+ sonssf (car tup)) (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

;; We will need pick in order to scramble

(define one?
  (lambda (n)
    (eq? n 1)))

(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;; Let's scramble

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup) (cons (car tup) rev-pre))
             (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))


;; Begin Chapter 12!

;; New multirember

(define multirember-first
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? a (car lat)) (mr (cdr lat)))
                  (else (cons (car lat) (mr (cdr lat))))))))
       mr) lat)))

;; Multirember simplified

(define multirember
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                 ((null? lat) '())
                 ((eq? a (car lat)) (mr (cdr lat)))
                 (else (cons (car lat) (mr (cdr lat))))))))
      (mr lat))))

;; Multirember-f, like before, only new

(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f
          (lambda (a lat)
            (cond
              ((null? lat) '())
              ((test? a (car lat)) (m-f a (cdr lat)))
              (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))

;; Same treatment for member

(define member-letrec?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
      (yes? lat))))

;; Same treatment for union, with member built right in

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) set2)
                ((M? (car set) set2) (U (cdr set)))
                (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                            ((null? lat) #f)
                            ((eq? a (car lat)) #t)
                            (else (N? (cdr lat)))))))
                 (N? lat)))))
      (U set1))))

;; And for two-in-a-row

(define two-in-a-row-letrec?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? a (car lat)) (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

;; Sum-of-prefixes...

(define sum-of-prefixes-letrec
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
                ((null? tup) '())
                (else (cons (+ sss (car tup)) (S (+ sss (car tup)) (cdr tup))))))))
      (S 0 tup))))

;; Finish it off with scramble

(define scramble-letrec
  (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) '())
              (else (cons (pick (car tup) (cons (car tup) rp)) (P (cdr tup) (cons (car tup) rp))))))))
    (lambda (tup)
      (P tup '()))))


;; Chapter 13

;; Intersect with the 12th commandment

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) '())
                ((member? (car set) set2) (cons (car set) (I (cdr set))))
                (else (I (cdr set)))))))
      (I set1))))
      
;; Old intersectall with a null check that happens every time through

(define intersectall-old
  (lambda (lset)
    (cond
      ((null? lset) '())
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset) (intersectall-old (cdr lset)))))))

;; Intersectall with the 12th commandment

(define intersectall-letrec
  (lambda (lset)
    (letrec
        ((I
          (lambda (lset1)
            (cond
              ((null? (cdr lset1)) (car lset1))
              (else (intersect (car lset1) (I (cdr lset1))))))))
      (cond
        ((null? lset) '())
        (else (I lset))))))

;; Intersectall with letcc

(define intersectall-letcc
  (lambda (lset)
    (letcc hop
      (letrec
          ((I (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (intersect (car lset) (I (cdr lset))))))))
        (cond
          ((null? lset) '())
          (else (I lset)))))))

;; Intersectall with letcc and intersect built in -- also with letcc!
;; Also I'm pretty sure the book has a mistake in the member? line of I. It conses on else instead of member?.
;; The version I think is right is below.

(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (I (car lset) (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                    ((J (lambda (s1)
                          (cond
                            ((null? s1) '())
                            ((member? (car s1) s2) (cons (car s1) (J (cdr s1))))
                            (else (J (cdr s1)))))))
                  (cond
                    ((null? s2) (hop '()))
                    (else (J s1)))))))
        (cond
          ((null? lset) '())
          (else (A lset)))))))

;; Moving on, let's try rember with letrec

(define rember-letrec
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (cdr lat))
                (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

;; Rember beyond first, setting us up for...

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) '())
                (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

;; rember-upto-last

(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? (car lat) a) (skip (R (cdr lat))))
                  (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))

;; Chapter 14
;; Our old friend leftmost, new and improved with let

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost (cdr l)))))))))

;; rember1 fixed with the 12th commandment

(define rember1
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? a (car l)) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (cond
                   ((eqlist? (R (car l)) (car l)) (cons (car l) (R (cdr l))))
                   (else (cons (R (car l)) (cdr l)))))))))
      (R l))))

;; rember1 fixed with the 15th commandment as well

(define rember2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((eqlist? (car l) av) (cons (car l) (R (cdr l))))
                     (else (cons av (cdr l))))))))))
      (R l))))

;; here is depth

(define depth-first
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth-first (cdr l)))
      (else
       (cond
         ((> (depth-first (cdr l)) (add1 (depth-first (car l)))) (depth-first (cdr l)))
         (else (add1 (depth-first (car l)))))))))

(define depth-let
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth (cdr l)))
      (else
       (let ((a (add1 (depth (car l)))) (d (depth (cdr l))))
         (cond
           ((> d a) d)
           (else a)))))))

;; Let's use if to make our lives easier

(define max*
  (lambda (m n)
    (if (> m n) m n)))

(define depth
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth (cdr l)))
      (else (max*
             (add1 (depth (car l))) (depth (cdr l)))))))

;; leftmost again, but better, faster, stronger.

(define new-leftmost
  (lambda (l)
    (letcc skip (lm l skip))))

(define lm
  (lambda (l out)
    (cond
      ((null? l) '())
      ((atom? (car l)) (out (car l)))
      (else (let ()
              (lm (car l) out)
              (lm (cdr l) out))))))