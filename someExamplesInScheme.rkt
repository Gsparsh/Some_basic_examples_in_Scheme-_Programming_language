#lang racket
;*************************************************
;Quadratic formula
;**************************************************
(define (roots a b c) ;Function roots 
  (cond 
    ;Code for quadratic formula
    ((< (- (* b b) (* 4 a c)) 0) 
         "These are complex roots")
         ((display (list (/ (+ (- b) (sqrt (- (* b b) (* 4 (* a c ))))) (* 2 a))
                         (/ (- (- b) (sqrt (- (* b b) (* 4 (* a c))))) (* 2 a)))))))
                      
(roots 1 4 4) 
(roots 1 4 5) ;Case demonstrating complex roots 
(roots 4 30 1)  

;*************************************************
;Recusive function
;*************************************************
(define (myPower base exp)
  (cond 
    ((< exp 0) (/ 1 (myPower base (- exp))))
    ((> exp 0) (* base (myPower base (- exp 1))))
    ((= exp 0) 1)))

(myPower 2 3) ;Test case for 2^3=8

;*************************************************
;Delete atom Function 
;*************************************************
(define (deleteAll atom list )
  (cond ((null? list) '()) ;Checks if elements stored in list 
        ((= atom (car list)) (deleteAll atom (cdr list)))
        (else (cons (car list) (deleteAll atom (cdr list)))))) ;Recurses through rest of set 

(deleteAll 0 '(1 2 0 3 4 0 5 6 0 7)) ;Test condition to remove zero's

;*************************************************
;Function nzeros to return number of zeros in list 
;*************************************************
(define nzeros
  (lambda (list)
    (cond ((null? list) 0)
          ((= 0 (car list)) (+ 1(nzeros(cdr list))))
          (else (+ 0 (nzeros (cdr list))))
    )
  )
)

(nzeros (list 0 8 0 1 0 6 0 4 0 2 3 0 4 0))