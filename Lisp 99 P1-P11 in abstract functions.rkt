;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |p1-10 in abs fun|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (p01 lst)
  (foldr (lambda (ele ans)
           (cond[(empty? ans) (cons ele ans)]
                [else ans]))
         empty
         lst))
(define (p02 lst)
  (foldr (lambda (ele ans)
           (cond[(empty? (second ans)) (cons empty (cons ele empty))]
                [(empty? (first ans)) (cons ele (rest ans))]
                [else ans]))
         (list empty empty)
         lst))
(define (p03 lst k)
  (first (foldr (lambda (ele ans)
           (cond[(= (foldr (lambda (ele ans) (add1 ans)) 0 ans) (+ (-(foldr (lambda (ele ans) (add1 ans)) 0 lst) k) 1)) ans]
                [else (cons ele ans)]))
         empty
         lst)))
(define (p04 lst)
  (foldr (lambda (ele ans) (add1 ans)) 0 lst))
(define (p05 lst)
  ;; (append lone ltwo)consumes two list: lone and ltwo, produces a list that combines
  ;;   lone and ltwo while still remaining the order of their elements.
  ;; append: (listof Any) (listof Any) -> (listof Any)
  (local[(define (append lone ltwo)
           (foldr (lambda (ele ans)
                         (cons ele ans))
                  ltwo
                  lone))]
  (foldr (lambda (ele ans) (append ans (list ele)))
         empty
         lst)))
(define (p06 lst)
  (equal? (p05 lst) lst))
(define (p07 lst)
  ;; (append lone ltwo)consumes two list: lone and ltwo, produces a list that combines
  ;;   lone and ltwo while still remaining the order of their elements.
  ;; append: (listof Any) (listof Any) -> (listof Any)
  (local[(define (append lone ltwo)
           (foldr (lambda (ele ans)
                         (cons ele ans))
                  ltwo
                  lone))]
    (foldr (lambda (ele ans)
           (cond[(cons? ele) (append (p07 ele) ans)]
                [else (cons ele ans)]))
           empty
           lst)))
(define (p08 lst)
  (second(foldr (lambda(ele ans)
           (cond[(empty? (first ans)) (list ele (cons ele empty))]
                 [(equal? ele (first ans)) ans]
                [else (list ele (cons ele (second ans)))]))
         (list empty empty)
         lst)))
(define (p09 lst)
  (second (foldr (lambda(ele ans)
           (cond[(empty? (first ans)) (list ele (list(cons ele empty)))]
                [(equal? ele (first ans)) (list ele (append (list (cons ele (first (second ans)))) (rest (second ans))))]
                [else (list ele (append (list(cons ele empty)) (second ans)))]))
         (list empty (list empty))
         lst)))
(define (p10 lst)
  (map (lambda (sub)
         (cons (foldr (lambda (ele ans) (add1 ans))0 sub) (cons (first sub) empty)))
   (p09 lst)))
;; p11 same as before