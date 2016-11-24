(define (author)
	(println "AUTHOR: Adam Pluth apluth@crimson.ua.edu")
)
;----preliminary---;
(define scons cons-stream)
(define scar stream-car)
(define scdr stream-cdr)
(define (scadr s) (scar(scdr s)))
(define (scddr s) (scdr(scdr s)))
(define (error s l)
;	(print s)
;	(print l)
)
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

(define (psum s)
	(scons (scar s) (sop + (scdr s) (psum s)))
	;(sop + s (scons 0.0 (psum s)))
)

(define (sref s n)
	(if (= n 0)
		(scar s)
		(sref (scdr s) (- n 1)))
)

(define (sget s n)
	(if (= n 0)
		s
		(sget (scdr s) (- n 1)))
)


(define (smap proc s)
	(scons (proc (scar s))
		(smap proc (scdr s))
	)
)

(define (sop op s1 s2)
;	(print (scar s1))
;	(print op)
;	(print (scar s2))
;	(println "")
	(scons (op (scar s1) (scar s2))
		(sop op (scdr s1) (scdr s2)))
)

(define (shuffle s1 s2)
	(scons (scar s1)
		(shuffle s2 s1))
)

(define (skip s)
	(scons (scar s) (skip (scdr (scdr s))))
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
;(define (run1)
;
;)
;---------task 2------------;



;---------test 2------------;
;(define (run2)
;
;)
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
;	(print "left\n")
	(cond	((null? tree) tree)
		(else (make-avl (node (left tree)) (left (left tree)) (make-avl (node tree) (right (left tree)) (right tree))))
	)
)
(define (rotate-r tree)
;	(print "right\n")
	(cond ((null? tree) tree)
        	(else (make-avl (node (right tree)) (make-avl (node tree) (left tree) (left (right tree))) (right (right tree))))
	)      
)
(define (rotate-left-right tree)
;	(print "left-right\n")
	(make-avl 
		(node (right (left tree))) 
		(make-avl (node (left tree)) (left (left tree)) (left (right (left tree)))) 
		(make-avl (node tree) (right (right (left tree))) (right tree))
	)
)
(define (rotate-right-left tree)
;	(print "right-left\n")
	(make-avl
		(node (left (right tree)))
		(make-avl (node tree) (left tree) (left (left (right tree))))
		(make-avl (node (right tree)) (right (left (right tree))) (right (right tree)))
	)
)
(define (balance-avl tree)
	(define (factor tree) (- (height (right tree)) (height (left tree))))
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
	(define que (enqueue que tree))
	(define (iterStats q)
	(define t (qpeek q))
	(define (factor tr) (- (height (left tr)) (height (right tr))))
		(if(or(empty? q)(null? t)) (println "")
			(begin
				(print (node t))
				(print ":")
				(print (factor t))
				(print " ")
				(define q (enqueue q (left t)))
				(define q (enqueue q (right t)))
				(iterStats (dequeue q))
			)
		)
	)
	(iterStats que)
)

(define (insert x)
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

;---------test 3------------;
(define (run3)
(define t (avl))
((t 'insert)3)
((t 'insert)4)
((t 'insert)5)
((t 'insert)1)
((t 'insert)0)
((t 'statistics))
(inspect ((t 'find) 5))
(define size ((t 'size)))
(inspect size)
(define test ((t 'getTree)))
(inspect test)
)
;(run3)
;---------task 4------------;

(define (call-each procs)
	(if (null? procs)
		'done
		(begin
			((car procs))
			(call-each (cdr procs))
		)
	)
)

(define (make-connector)
	(let 	((value #f)
		(resolved #f)
		(action-procs '()))

	(define (accept-action! proc)
		(set! action-procs
			(cons proc action-procs )
		)
	)
	(define(accept-action! proc)
		(set! action-procs
			(cons proc action-procs)
		)
	)
	(define (set-value! new-value)
		(cond	(( not (has-value? self))
				(begin
					(set! value new-value)
					(set! resolved #t)
					(call-each action-procs)))
				((not (= new-value value))
					(error "Contradiction" (list value new-value)))
				(else 'ignored)
		)
	)
	(define(forget-value!)
		(begin
			(set! value #f)
			(set! resolved #f)
			'done
		)
	)
	(define (self m)
		(cond
			((eq? m 'get-value) value)
			((eq? m 'add-action!) accept-action!)
			((eq? m 'set-value!) set-value!)
			((eq? m 'forget-value!) (forget-value!))
			((eq? m 'has-value?) resolved)
			(else (error "Uknown method for connector" m))
		)
	)
	self))

;-------helpers

(define (get-value connector)
	(connector 'get-value)
)
(define (forget-value! connector)
	(connector 'forget-value!)
)
(define (has-value? connector)
	(connector 'has-value?)
)
(define (set-value! connector new-value)
	((connector 'set-value!) new-value)
)
(define (add-action! connector action)
	((connector 'add-action!)action)
)


(define(resolved-count connectors)
	(define (iter l c)
		(if	(null? l)
			c
			(iter (cdr l)
				(+ c (if (car l) 1 0))
			)
		)
	)
	(iter (map has-value? connectors) 0)
)

(define (enforce-relation connector proc @)
	(define (apply-when-resolved)
		(let ((resolved-res
			(if (= (length @) (resolved-count @))
				(apply proc (map get-value @))
				#f)))
		(if resolved-res
			(set-value! connector resolved-res))))
	(for-each (lambda (other)
		(add-action! other apply-when-resolved)) @)
)

(define (constant value connector)
	(set-value! connector value)
)

(define (adder a1 a2 sum)
	(enforce-relation a1 (lambda(a2 sum)
				(- sum a2)) a2 sum)
	(enforce-relation a2 (lambda(a1 sum)
				(- sum a1)) a1 sum)
	(enforce-relation sum (lambda(a2 sum)
				(+ a1 a2)) a1 a2)
)

(define (square x)
	(* x x)
)

(define (make-grav f m1 m2 r g)
;f=(* g (/ (* m1 m2 ) r )))
	(enforce-relation f 	
		(lambda(m1 m2 r g)
			(* g (/ (* m1 m2 ) (square r)))
			) 
			m1 m2 r g)
;m1=(/ (* (/ f g) (square r) ) m2)
	(enforce-relation m1 
		(lambda(f m2 r g)
			(/ (* (/ f g) (square r) ) m2)
			) 
			f m2 r g)
;m2=(/ (* (/ f g) (square r ) m1)
	(enforce-relation m2
	 	(lambda(f m1 r g)
			(/ (* (/ f g) (square r) ) m1)
			) 
			f m1 r g)
;(square r)=(/ (* g m1 m2) f)
	(enforce-relation r
	 	(lambda(f m1 m2 g)
			(sqrt(/ (* g m1 m2) f))
			) 
			f m1 m2 g)
)


(define (gravity f m1 m2 r)
	(let
		((f (make-connector))
		(m1 (make-connector))
		(m2 (make-connector))
		 (r (make-connector))
		(G (make-connector)))
;	(constant 0.00667300  G)
	(make-grav f m1 m2 r G)
	(constant 0.00667300  G)
	'ok)
)


(define (probe-connector name connector)
	(add-action!connector
		(lamdba ()
		(newline)
		(display name (display " ")
		(display " New-value = ")
		(display (get-value connector))))
	)
)


;---------test 4------------;
;(define (run4)
;	(define F (make-connector))
;	(define M1 (make-connector))
;	(define M2 (make-connector))
;	(define R (make-connector))
;	(constant 100 M1)
;	(constant 150 M2)
;	(constant 5 R)
;	(constant 4.0038 F)
;	(println "4.0038= G ((100 * 150)/(5*5))")
;	(gravity F M1 M2 R)
;	(inspect (get-value F))
;)
;---------task 5------------;



;---------test 5------------;
;(define (run5)
;
;)
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

(define (mix i j)
	(scons (* (scar i) (scar j))
		(mix (scdr i) (scdr j))
	)
)
(define mixStrm (mix seven eleven))

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
	(sev11 (sev11 seven eleven) mixStrm)
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
;(stream-display s 10)
;(stream-display seven 4)
;(stream-display eleven 4)
(inspect (stream-display mixStrm 8) )
(define bgs (big-gulp))
(stream-display bgs 4)
(stream-display bgs 8)
(stream-display bgs 20)
)
;(run6)
;---------task 7------------;

(define (delay # $x)
	(cons $x #))
(define (force x)
	(eval (car x) (cdr x)))

(define (signal f x dx)
	(scons (f x ) (signal f (+ x dx) dx))
)

(define(differential s dx)

)

(define (deriv g)
	(lambda (x)
		(/ (- (g (+ x dx)) ( g x)) dx)
	)
)
(define (integral s x dx)
	(define int
		(scons
			x
			(sop +
				(smap (lambda (a) (* a dx)) (force s))
				int)))
	int
)

(define y (integral (delay y) 1 .001))
(stream-display y 3)


;---------task 7------------;
;(define (run7)
;
;)
;---------task 8------------;

(define (one n) 
	(scons n (one n))
)

(define ones (one 1.0 ))

(define (negOne s)
	(scons  (- 0.0 (scar s)) (negOne (scdr s)))
)

(define negOnes (negOne ones))

(define (altOne s1 s2)
	(shuffle s1 s2)
)

(define altOnes (altOne ones negOnes))

(define evens
	(skip (integers-from 0.0))
)

(define facto
	(scons 1.0 (sop * (integers-from 1.0)  facto))
)

(define (Ns n)
	(scons (real n) (Ns n))
)

(define (even-pow x) 
	(sop ^ (Ns x) evens)
)

(define even-fact
	(skip facto)
)

(define alt-even-fact
	(sop * altOnes even-fact)
)

(define (mystery x)

	(sop * (even-pow x) (sop / ones alt-even-fact))
)

(define (ps-mystery x)
	(psum(mystery x))
)

(define (square x)
	(* (real x) (real x))
)

(define (et s)
	(define s0 (real(sref s 0)))
	(define s1 (real(sref s 1)))
	(define s2 (real(sref s 2)))
;	(print "s0= ")
;	(println s0)
;	(print "s1= ")
;	(println s1)
;	(print "s2= ")
;	(println s2)
;		(inspect(+ s0 (* -2.0 s1) s2))
	(scons
		(- s2 
			(/ (^ (- s2 s1) 2) (+ s0 (* -2 s1) s2)))
		(et (scdr s))
	)
)

(define (tableau t s)
	(scons 	s 
		(tableau t (t s)))
)

(define (acc-mystery x)
	(et (ps-mystery (real x)))
)

(define (super-mystery x)
	(smap scar (tableau et (ps-mystery (real x))))
)	




;---------test 8------------;
(define (run8)
;(stream-display alt-even-fact 100)

(stream-display (ps-mystery 1) 100)

(stream-display (mystery 1) 100)

;(stream-display (acc-mystery 1.0) 20)
;(super-mystery 1)
;(stream-display (super-mystery 1) 50)

)
(run8)
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
		(mergeWeight (smap (lambda (x) (list (scar s1)x))
				(scdr s2))
		(weight-pairs (scdr s1) (scdr s2) w)
		w)
	)
)

(define (ramanujan)
	(define (ramMerge strm)
		(if(= (trip-sum (scar strm)) (trip-sum (scadr strm)))
			(scons (trip-sum (scar strm))
				(ramMerge (scddr strm)))
			(ramMerge (scdr strm))
		)
	)
	(ramMerge (weight-pairs (integers-from 0) (integers-from 0) trip-sum))
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
