;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname explicit_ycomb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define Y
  (lambda (le)
    ((lambda (f) (f f)) 
     (lambda (f) (le (lambda (x) ((f f) x)))))))

(define ylength
  (Y (lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))))

(ylength '(a b c d e))