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
      