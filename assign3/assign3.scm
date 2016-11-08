(define (author)
	(println "AUTHOR: Adam Pluth apluth@crimson.ua.edu")
)
;----preliminary---;
(define scons cons-stream)
(define scar stream-car)
(define scdr stream-cdr)
(define (scadr s) (scar(scdr s)))
(define (scddr s) (scdr(scdr s)))
(define (max a b)
	(if(< a b) b a )
)
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
(define (div7? n)
	(inspect n)
	(cond	((= n 0) #f)
		(else 	(or 
			(not(zero? (remainder n 7)))
			(not(zero? (remainder n 11)))
			)
		)
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
(define (avl)
(define s 0)
(define tree '())
(define (getTree) tree)
(define (node tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define (height tree)
	(cond ((null? tree) 0)
		(else (cadddr tree))
	)
)
(define (make-node node) (list node '() '() 1))
(define (make-avl node left right)
	(list node left right (+ 1 (max (height left) (height right))))
)
(define (contains? x tree)
	(cond((null? tree) #f)
		((= x (node tree)) #t)
		((< x (node tree)) (contains? x (left tree)))
		((> x (node tree)) (contains? x (right tree)))
	)
)
(define (find x) 
	(contains? x tree)
)
(define (rotate-l tree)
	(print "left\n")
	(cond	((null? tree) tree)
		(else (make-avl (node (left tree)) (left (left tree)) (make-avl (node tree) (right (left tree)) (right tree))))
	)
)
(define (rotate-r tree)
	(print "right\n")
	(cond ((null? tree) tree)
        	(else (make-avl (node (right tree)) (make-avl (node tree) (left tree) (left (right tree))) (right (right tree))))
	)      
)
(define (rotate-left-right tree)
	(print "left-right\n")
	(make-avl 
		(node (right (left tree))) 
		(make-avl (node (left tree)) (left (left tree)) (left (right (left tree)))) 
		(make-avl (node tree) (right (right (left tree))) (right tree))
	)
)
(define (rotate-right-left tree)
	(print "right-left\n")
	(make-avl
		(node (left (right tree)))
		(make-avl (node tree) (left tree) (left (left (right tree))))
		(make-avl (node (right tree)) (right (left (right tree))) (right (right tree)))
	)
)
(define (balance-avl tree)
	(define (factor tree) (- (height (right tree)) (height (left tree))))
	(inspect (factor tree))
	(let ((tree-factor (factor tree))) 
		(cond 	((= tree-factor 2) 
			  (cond (
				(< (factor (right tree)) 0) (rotate-right-left tree))
			  (else (rotate-r tree))))
			((= tree-factor -2) 
		  	  (cond (
				(> (factor (left tree)) 0) (rotate-left-right tree))
			  (else (rotate-l tree))))
         		(else tree)
		)
	)  
)

(define (size) s)

(define (statistics)
	(define que (Queue))
	(define que (enqueue que (left tree)))
	(define que (enqueue que (right tree)))
	(inspect que)
	(define (iterStats q t)
;	(inspect t)
	(define (factor tr) (- (height (right tr)) (height (left tr))))
		(if(empty? q) #f
			(begin
;	(inspect q)
				(define q (enqueue q (left t)))
;	(inspect q)
				(define q (enqueue q (right t)))
;	(inspect q);need to print head of queue
				(print (node t))
				(print ":")
				(print (factor t))
				(print " ")
				(iterStats (dequeue q) (qpeek q))
			)
		)
	)
	(iterStats que tree)
)

(define (insert x)
	(print "inserted ")
	(print x)
	(print "\n")
	(define (insertInner x tree)
		(cond	((null? tree) (make-node x))
			((< x (node tree))
				(let* ((newL (insertInner x (left tree))) (newRoot (make-avl (node tree) newL (right tree))))
					(balance-avl newRoot)
				)
			)
			((> x (node tree)) 
				(let* ((newR (insertInner x (right tree))) (newRoot (make-avl (node tree) (left tree) newR))) 
             				(balance-avl newRoot)
             			)
			)
			(else tree)
		)
	)
	(cond ((contains? x tree) tree)
		(else 
			(set! s (+ 1 s))
			(set! tree (insertInner x tree)))
	)
)
this
)

;----QUEUE----;
(define (Queue)
	(define size 0)
	(define store nil)
	(cons size store)
)
(define (enqueue store x)
	(define size (+ (car store) 1))
	(define s (append (cdr store) (list x)))
	(cons size s)
)
(define (dequeue store)
	(define size (- (car store) 1))
	(define store (cdr (cdr store)))
	(cons size store)
)
(define (qpeek store)
	(if(empty? store) #f
	(car (cdr store)))
)
(define (empty? store)
	(eq? (car store) 0)
)
(define (qsize store)
	(car store)
)

(define (treeNode val left right)
        (list val left right)
)
(define (getL t)
        (getElement t 1)
)
(define (getR t)
        (getElement t 2)
)
(define (getV t)
        (getElement t 0)
)

(define (accu op init seq)
        (if (null? seq)
                init
                (op (car (car seq))
                        (accu op init (cdr seq))
                )
        )
)

(define (treeflatten tree)
        (define (flat t dep)
                (cond   ((null? t) '())
                        ((and (null? (getR t)) (null? (getL t))) (list(list dep (getV t))))
                        (else   (append (flat (getL t)(+ dep 1))
                                        (flat (getR t)(+ dep 1))
                                )
                        )
                )
        )
        (flat tree 0)
)

(define (treedepth tree)
        (define new-tree (treeflatten tree))
        (define l (real(length new-tree)))
        ( / (accu + 0 new-tree) l)
)



;---------test 3------------;
(define (run3)
(define t (avl))
(ppTable t)
((t 'insert)3)
;(ppTable t)
(define test ((t 'getTree)))
(inspect test)
((t 'insert)4)
;(ppTable t)
(define test ((t 'getTree)))
(inspect test)
((t 'insert)5)
;(ppTable t)
(define test ((t 'getTree)))
(inspect test)
((t 'insert)1)
;(ppTable t)
(define test ((t 'getTree)))
(inspect test)
((t 'insert)0)
((t 'statistics))
(ppTable t)
(define f ((t 'find) 5))
(inspect f)
(define s ((t 'size)))
(inspect s)
(define test ((t 'getTree)))
(inspect test)
(inspect (treeflatten test))
)
(run3)
;---------task 4------------;



;---------test 4------------;
(define (run4)

)
;---------task 5------------;



;---------test 5------------;
(define (run5)

)
;---------task 6------------;

;---streams--;
(define (s-from n) 
	(scons n 
		(s-from (* 7 n))
	)
)
(define (e-from n) 
	(scons n 
		(e-from (* 11  n))
	)
)

(define seven (s-from 7))
(define eleven (e-from 11))
;--mergre streams--;
(define (sev11 s e)
	(let ((cars1 (scar s))
		(cars2 (scar e)))
		(cond 	((< cars1 cars2)
				(scons cars1
					(sev11 (stream-cdr s) e)))
			((= cars1 cars2)
				(scons cars1
					(sev11 (stream-cdr s) e)))
			(else 	(scons cars2
					(sev11 s (stream-cdr e)))
			)
		)
	)
)


(define (big-gulp)
	(sev11 seven eleven)
)
;--print stream--;
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
(stream-display seven 4)
(stream-display eleven 4)
(define bgs (big-gulp))
(stream-display bgs 4)
(stream-display bgs 8)
(stream-display bgs 20)
)
;(run6)
;---------task 7------------;

(define (signal f x dx)
	(scons (f x ) (signal f (+ x dx) dx))
)
(define (integral s dx)

)
(define(differential s dx)

)

(define (deriv g)
	(lambda (x)
		(/ (- (g (+ x dx)) ( g x)) dx)
	)
)
(define (integr f a b dx)
	(* (sum f (+ a (/ dx 2.0))
		(lambda (x) (+ x dx))
		b)
	dx)
)

;---------task 7------------;
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

;(run9)

;--final run test for completion--;
(author)