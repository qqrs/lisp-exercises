#lang racket

;'(5 4 3 2 1)
;'(5) '(4 3 2 1)
;'(4 5) '(3 2 1)
;'(3 4 5) '(2 1)
;'(2 3 4 5) '(1)
;'(1 2 3 4 5) '()

(define (insert el li)
  (cond [(empty? li) (list el)]
        [(<= el (first li)) (cons el li)]
        [#t (cons (first li) (insert el (rest li)))]))
  
  
;(insert 4 '(1 2 3 5))
;(insert 4 '())
;(insert 4 (insert 3 '()))
;(foldl insert '() '(3 2 1))

(define (inssort li)
  (foldl insert '() li))
  
(inssort '(5 4 3 2 1))
(inssort '())
(inssort '(5 2 3 4 1))
