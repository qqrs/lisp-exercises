#lang racket

(define (amap fn li)
  (if (empty? li)
      '()
      (cons (fn (first li)) (amap fn (rest li)))))

;(amap - '(1 2 3))

;(define (afilter fn li)
;  (if (empty? li)
;      '()
;      (if (fn (first li))
;          (cons (first li) (afilter fn (rest li)))
;          (afilter fn (rest li)))))


(define (afilter fn li)
  (cond [(empty? li) '()] 
        [(fn (first li)) (cons (first li) (afilter fn (rest li))) ]
        [#t (afilter fn (rest li))]))
  
;(afilter even? '(1 2 3 4 5 6 7 8))


(define (areduce fn acc li)
  (if (empty? li) 
      acc
      (areduce fn (fn acc (first li)) (rest li))))
  
 
;(areduce - 0 '(1 1 1 2 3 4))




