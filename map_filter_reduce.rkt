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




(define (take-upto n li)
  (cond [(empty? li) '()]
        [(zero? n) '()]
        [#t (cons (first li) (take-upto (- n 1) (rest li)))]))

(define (drop-upto n li)
  (cond [(empty? li) '()]
        [(zero? n) li]
        [#t (drop-upto (- n 1) (rest li))]))

;(take-upto 2 '(1))
;(drop-upto 2 '(1))
;(take-upto 2 '(1 2 3))
;(drop-upto 2 '(1 2 3))


(define (partition-all n li)
  (if (empty? li)
      '()
      (cons (take-upto n li) (partition-all n (drop-upto n li)))))

;(partition-all 2 '(1 2 3 4))
;(partition-all 2 '(1 2 3 4 5))
;(partition-all 2 '())

;('(1 2) (partition-all 2 '(3 4)))
;('(1 2) '(3 4))


;'(5 4 3 2 1)
;'(4 5) '((3) (1 2))
;'(4 5) '(1 2 3)
;'(1 2 3 4 5)

(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(< (first l1) (first l2))
         (cons (first l1) (merge (rest l1) l2))]
        [#t
         (cons (first l2) (merge l1 (rest l2)))]))

;(merge '() '())
;(merge '(1 2) '())
;(merge '() '(1 2))
;(merge '(1 3) '(2 4))
;(merge '(1 1) '(1 1))

(define (msort li)
  (let* [[pairs (partition-all 2 li)]
         [merged (amap (lambda (pair) 
                         (if (empty? (rest pair)) 
                             (first pair)
                             (merge (first pair) (second pair))))
                       pairs)]]
    (if (empty? (rest merged))
        (first merged)
        (msort merged))))
            

(define (mergesort li)
  (msort (amap list li)))


(mergesort '(5 4 3 2 1))
(mergesort '(2 2 2))
(mergesort '(1 2 3))
(mergesort '(5 1 4 2 3))