;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ycomb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ())))
(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (list)
     (cond
       ((null? list) 0)
       (else
        (add1 ((mk-length mk-length) (cdr list))))))))
 '(a b c d e f g h i j))