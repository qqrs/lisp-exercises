(defn ms-merge [xs ys]
    (cond
        (empty? xs) ys
        (empty? ys) xs
        (<= (first xs) (first ys)) (cons (first xs) (ms-merge (rest xs) ys))
        :else (cons (first ys) (ms-merge xs (rest ys)))
    )
)

(defn ms-chunk [xs]
    (cond
        (empty? xs) '()
        (= (count xs) 1) xs
        :else (cons (ms-merge (first xs) (second xs)) 
                    (ms-chunk (drop 2 xs)))))

(defn ms-msort [xs]
    (if (= (count xs) 1)
        xs
        (ms-msort (ms-chunk xs))))

(defn mergesort [xs]
    (first (ms-msort (map list xs))))
            

#(print (ms-merge '(1 3 5) '(2 4 6)))
#(print (ms-chunk '((1) (3) (5) (2) (4) (6))))
#(print (ms-chunk '((1 3 5) (2 4 6))))
#(print (ms-msort '((1 3 5) (2 4 6))))
(print (mergesort '(1 3 5 2 4 6)))
(print (mergesort '(5 4 3 2 1)))
(print (mergesort '(1 2)))
(print "\n")

