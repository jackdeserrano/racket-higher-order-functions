#lang racket

;; andmap/: (X -> Bool) (listof X) -> Bool
(define andmap/
  (lambda (f? lox)
    (foldr (lambda (x rror) (and (f? x) rror)) (and) lox)))

;; ormap/: (X -> Bool) (listof X) -> Bool
(define ormap/
  (lambda (f? lox)
    (foldr (lambda (x rror) (or (f? x) rror)) (or) lox)))

;; remove-duplicates/: (listof X) -> (listof X)
(define remove-duplicates/
  (lambda (lox)
    (foldr (lambda (x rror) (cons x (filter (lambda (y) (not (eq? x y)) rror)))) '() lox)))

;; zip/: (listof X) (listof Y) -> (listof (list X Y))
(define zip/
  (lambda (lox loy)
    (map list lox loy)))

