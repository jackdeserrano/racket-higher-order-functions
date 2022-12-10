#lang racket

;; my-andmap: (X -> Bool) (listof X) -> Bool
(define my-andmap
  (lambda (f? lox)
    (foldr (lambda (x rror) (and (f? x) rror)) (and) lox)))

;; my-ormap: (X -> Bool) (listof X) -> Bool
(define my-ormap
  (lambda (f? lox)
    (foldr (lambda (x rror) (or (f? x) rror)) (or) lox)))

;; my-remove-duplicates: (listof X) -> (listof X)
(define my-remove-duplicates
  (lambda (lox)
    (foldr (lambda (x rror) (cons x (filter (lambda (y) (not (eq? x y)) rror)))) '() lox)))

;; a ZO is either:
;; * '()
;; * (cons ZO '())

;; nat->zo: Nat -> ZO
(define nat->zo
  (lambda (n)
    (cond [(zero? n) '()]
	  [else (cons (nat->zo (sub1 n)) '())])))

;; a VNO is either:
;; * '()
;; * (append v (list v))
;; Requires: v is a VNO.

;; nat->vno: Nat -> VNO
(define nat->vno
  (lambda (n)
    (cond [(zero? n) '()]
          [else (local [(define rror (nat->vno (sub1 n)))]
                  (append rror (list rror)))])))
