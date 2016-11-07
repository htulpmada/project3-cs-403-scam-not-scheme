(define (author)
	(println "AUTHOR: Adam Pluth apluth@crimson.ua.edu")
)
;----preliminary---;
(define scons cons-stream)
(define scar stream-car)
(define scdr stream-cdr)

(define (filter pred seq)
	(cond ((null? seq) '())
		((pred (car seq))
		(cons (car seq)
			(filter pred (cdr seq))))
		(else (filter pred (cdr seq)))
	)
)
(define (sfilter pred seq)
	(cond ((null? seq) '())
		((pred (scar seq))
		(cons (scar seq)
			(filter pred (scdr seq))))
		(else (filter pred (scdr seq)))
	)
)

(define (smap proc s)
	(scons (proc (scar s))
		(smap proc (scdr s))
	)
)


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
	(inspect n)
	(inspect x)
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
				(sfilter
					(lambda (x)
						(not
							(divisible? x (scar s))
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
	(print "[")
	(define (iter-display s x)
		(cond 
			((= x 1 )
				(print (scar s))
				(print "]")
				(print "\n")
			)
			(else
				(print (scar s))
				(print ",")
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
;(define bgs (big-gulp))
;(stream-display bgs 4)
;(stream-display bgs 8)
;(stream-display bgs 20)
)
(run6)
;---------task 7------------;

(define (signal f x dx)
	(scons (f x ) (signal f (+ x dx) dx))
)
(define (integral s dx)

)
(define(differential s dx)

)


;---------test 7------------;
(define (run7)

)
;---------task 8------------;



;---------test 8------------;
(define (run8)

)
;---------task 9------------;
(define (trip e) (* e e e))
(define (trip-sum e) (+ (trip (car e)) (trip (cadr e))))
(define (mergeWeight s1 s2 weight)
        (cond ((stream-null? s1) s2)
                  ((stream-null? s2) s1)
                  (else
                        (let ((cars1 (scar s1))
                                  (cars2 (scar s2)))
                          (cond ((< (weight cars1) (weight cars2))
                                 (scons cars1 
                                                       (mergeWeight (stream-cdr s1) s2 weight)))
                            ((= (weight cars1) (weight cars2)) 
                                     (scons cars1 
                                                            (mergeWeight (stream-cdr s1) s2 weight)))
                                (else (scons cars2
                                                        (mergeWeight s1 (stream-cdr s2) weight))
				)
			  ) 
			)
		)
	)
)

(define (weight-pairs s1 s2 w)
	(scons (list (scar s1) (scar s2))
		(mergeWeight (smap (lambda (x) (list (scar s1) x))
				(scdr s2))
		(weight-pairs (scdr s1) (scdr s2) w)
		w)
	)
)

(define (ramanujan)
	(define (scadr s) (scar(scdr s)))
	(define (scddr s) (scdr(scdr s)))
	(define (ramMerge strm)
		(if(= (trip-sum (scar strm)) (trip-sum (scadr strm)))
			(scons (list (trip-sum (scar strm)) (scar strm) (scadr strm))
				(ramMerge (scddr strm)))
			(ramMerge (scdr strm))
		)
	)
	(ramMerge (weight-pairs ints ints trip-sum))
)


;---------test 9------------;
(define (run9)
(define ramNum (ramanujan))
(inspect ramNum)
(stream-display ramNum 5)
)

(run9)

(author)
