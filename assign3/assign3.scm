(define (author)
	(println "AUTHOR: Adam Pluth apluth@crimson.ua.edu")
)
;----preliminary---;
(define (filter pred seq)
	(cond ((null? seq) '())
		((pred (car seq))
		(cons (car seq)
			(filter pred (cdr seq))))
		(else (filter pred (cdr seq)))
	)
)

(define scons cons-stream)
(define scar stream-car)
(define scdr stream-cdr)

(define (integers-from n) 
	(scons n 
		(integers-from (+ 1 n))
	)
)

(define ints (integers-from 2))

(define (print-stream s)
        (println (scar s))
	(print-stream (scdr s))
)

(define (zero? n)(= n 0))
(define (divisible? n x)
	(cond	((= x 0) nil)
		(else (zero? (remainder n x)))
	)
)

;(print-stream ints)
;---------task 1------------;



;---------test 1------------;
(define (run1)

)
;---------task 2------------;



;---------test 2------------;
(define (run2)

)
;---------task 3------------;



;---------test 3------------;
(define (run3)

)
;---------task 4------------;



;---------test 4------------;
(define (run4)

)
;---------task 5------------;



;---------test 5------------;
(define (run5)

)
;---------task 6------------;
(define (big-gulp)
	(define (bg-iter s)
		(scons 7 
			(bg-iter 
				(filter
					(lambda (x)
						(not
							(divisible? x (car s))
						)
					)
					(scdr s)
				)
			)
		)
	)
	(bg-iter (integers-from 11))
)

(define (stream-display strm n)
	(define (iter-display s x)
		(cond 
			((= x 0 )
				(print (scar s))
				(print "\n"))
			(else
				(print (scar s))
				(print "\n")
				(iter-display (scdr s) (- x 1))
			)
		)
	)
	(iter-display strm n)
)

;---------test 6------------;
(define (run6)
;stream-display test;
(define s (integers-from 0))
(stream-display s 10)
(define bgs (big-gulp))
(stream-display bgs 4)
;(stream-display bgs 8)
;(stream-display bgs 20)
)
(run6)
;---------task 7------------;



;---------test 7------------;
(define (run7)

)
;---------task 8------------;



;---------test 8------------;
(define (run8)

)
;---------task 9------------;



;---------test 9------------;
(define (run9)

)



(author)
