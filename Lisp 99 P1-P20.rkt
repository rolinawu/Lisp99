;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |p20 and above|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (p01 lst)
(define (p01 lst)
  (cond[(empty? (rest lst)) lst]
       [else (p01 (rest lst))]))
(define (p02 lst)
  (cond[(empty? ((rest (rest lst)))) lst]
       [else (p01 (rest lst))]))
;; require: k < (length lst)
(define (p03 lst k)
  (cond[(= 1 k) (first lst)]
       [else (p03 (rest lst) (sub1 k))]))
(define (p04 lst)
  (cond[(empty? lst) 0]
       [else (+ 1 (p04 (rest lst)))]))
(define (p05 lst)
  (cond[(empty? lst) empty]
       [else (cons (first lst) (p05 (rest lst)))]))
(define (p06 lst)
  (equal? (p05 lst) lst))
#|
'() '()
'(a) '(a)
'((a)) '(a)
'(c (a) (b)) '(a b)
|#
(define (p07 lst)
  (cond[(empty? lst) empty]
       [(not (cons? (first lst))) (cons (first lst) (p07 (rest lst)))]
       [else (append (p07 (first lst)) (p07 (rest lst)))]))
#|
'() '()
'(a) '(a) ='(a  ()) '(a)
'(a a) '(a)
'(a b) '(a b)
|#
(define (p08 lst)
  (cond[(empty? lst) empty]
       [(empty? (rest lst)) (cons (first lst) empty)]
       [(equal? (first lst) (second lst)) (p08 (rest lst))]
       [else (cons (first lst) (p08 (rest lst)))]))
;; p09: (listof Any) -> (listof (listof Any))
(define (p09 lst)
  (cond[(empty? lst) (list empty)]
       [(empty? (rest lst)) (list (list (first lst)))]
       [(equal? (first lst) (first (first (p09 (rest lst)))))
        (cons (cons (first lst) (first (p09 (rest lst))))
              (rest (p09 (rest lst))))]
       [else (cons (list (first lst)) (p09 (rest lst)))]))
;; p09 ((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))
(define (p10 lst) ;;p04 is find length
  (map(lambda(ele)
        (cons (p04 ele) (cons (first ele) empty)))
      (p09 lst)))
       ;[else (append (cons (p04 (first lst)) (first(first lst))) (p04 (rest lst)))]))
(define (p11 lst)
  (map(lambda(ele)
        (cond[(= 1 (first ele)) (second ele)]
             [else ele]))
      (p10 lst)))
(define (p12 lst)
  (cond[(empty? lst) empty]
       [(cons? (first lst))
        (append (make-list (first(first lst)) (second (first lst))) (p12 (rest lst)))]
       [else (cons (first lst) (p12 (rest lst)))]))
(define (p13 lst)
  (cond[(empty? lst) (list empty)]
       [(empty? (rest lst)) (cons (first lst) empty)]
       [(and (cons? (first (p13 (rest lst))))(equal? (first lst) (second (first (p13 (rest lst))))))
      (cons (list (add1 (first (first (p13 (rest lst))))) (second (first (p13(rest lst)))))
              (rest (p13 (rest lst))))]
       [(equal? (first lst) (second lst)) (cons (list 2 (first lst)) (p13 (rest (rest lst))))]
       [else (cons (first lst) (p13 (rest lst)))]))
(define (p14 lst)
  (map (lambda (ele) (cons ele (cons ele empty))) lst))
#|
'() '()
'(a) 1  '(a)
'(a) 2  '(a a)
'(a b) 2 '(a a b b)
|#
(define (p15 lst n)
  (cond[(empty? lst) empty]
       [(= n 0) empty]
       [else (append (cons (first lst) (p15 (list (first lst)) (sub1 n))) (p15 (rest lst) n))]))
(define (p18 lst k n)
  (cond[(empty? lst) empty]
       [(= n 1) lst]
       [(= k 1) (cons (first lst) (p18 (rest lst) 0 (sub1 n)))]
       [else (p18 (rest lst) (sub1 k) (sub1 n))]))

  



#|(define (sub lst)
  (cond[(empty? lst) empty]
       [(empty? (rest lst)) (cons (first lst) empty)]
       [(equal? (first lst) (second lst))
        (append (list (first lst)) (sub (rest lst)))]
       [else (cons (cons (first lst) empty) (sub (rest lst)))]))


(define (inserter a lst)
  (cons (cons a (first lst)) (rest lst)))

(inserter 'a (list (list 'a) (list 'b)))


(define (pack lst)
  (cond [(empty? lst) empty]
        [(empty? (rest lst)) (cons (cons (first lst) empty) empty)]
        [(equal? (first lst) (first (first (pack (rest lst)))))
         (inserter (first lst) (pack (rest lst)))]
        [else (cons (cons (first lst) empty) (pack (rest lst)))]))

(pack `(a a a a b b c c a a c c))

;;lst -> lst of lsts
|#