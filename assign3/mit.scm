;; booleans

(define true #t)
(define false #f)

;; variadic functions

(define scam-lambda lambda)
(define scam-define define)
(define (scam-to-mit-parameter-list params)
    (scam-define local nil)
    (scam-define (helper params)
        (cond
            ((null? params) nil)
            ((atom? params)
                (set! local (list (list 'define params '@)))
                (list '@)
                )
            ((eq? (car params) '.)
                (set! local (list (list 'define (cadr params) '@)))
                (list '@)
                )
            (else
                (cons (car params) (helper (cdr params)))
                )
            )
        )
    (scam-define newparams (helper params))
    (cons newparams local)
    )
(define (lambda # $params $)
    (scam-define newparams (scam-to-mit-parameter-list $params))
    (pass scam-lambda # (car newparams) (append (cdr newparams) $))
    )
(define (define # $)
    (cond
        ((atom? (car $))
            (pass scam-define # $)
            )
        (else
            (pass scam-define #
                (inspect (append (scam-to-mit-parameter-list (car $)) (cdr $))))
            )
        )
    )

;; concurrency

(define pexecute parallel-execute)
(define (test-and-set! cell)
    (lock)
    (if (car cell) #t (begin (set-car! cell #t) #f))
    (unlock)
    )
