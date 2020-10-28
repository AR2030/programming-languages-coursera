
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;2
(define (string-append-map xs suffix)
   (map (lambda (str) (string-append str suffix))xs))

;3
(define (list-nth-mod xs n)
  (cond [(< n 0)(error "list-nth-mod: negative number")]
        [(null? xs)(error "list-nth-mod: empty list")]
        [#t (let ([ith-elemnt (remainder n (length xs))])
              (car(list-tail xs ith-elemnt)))]))

;4
(define (stream-for-n-steps s n)
  (if (= n 0 )
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (= 1 (remainder x 2))
                    (cons "dan.jpg" (lambda () (f (+ x 1))))
                    (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;7
(define (stream-add-zero s)
  (lambda() (cons
                  (cons 0 (car (s)) )
                   (stream-add-zero (cdr (s)) ))))

;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
              (cons (cons (list-nth-mod xs n)
                          (list-nth-mod ys n))
                    (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))
;9
(define (vector-assoc v vec)
  (letrec ([length (vector-length vec)]
           [f (lambda (index)
                (cond [(>= index  length) #f]
                      [(not(pair?(vector-ref vec index))) (f(+ index 1))]
                      [(equal? (car(vector-ref vec index)) v) (vector-ref vec index) ]
                      [#t (f(+ index 1))]))])
     (f 0)))

;10
(define (cached-assoc lst n)
  (let ([cache (make-vector n #f)]
        [next-to-replace 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v lst)])
            (and ans
                 (begin (vector-set! cache next-to-replace ans)
                        (set! next-to-replace 
                              (if (= (+ next-to-replace 1) n)
                                  0
                                  (+ next-to-replace 1)))
                        ans)))))))
                     
