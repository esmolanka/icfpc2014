
(define (main x y)
  (cons 
    (let ((foo (lambda (x r) 
               (if-then-recur
                 (== x 0)
                 r
                 ((- x 1) (* x r))))))
      (foo 5 1))
    (fac 5)))

(define (fac x)
  (let ((go (lambda (x r)
              (if-then-recur (== 0 x)
                             r
                             ((- x 1) (* r x))))))
    (go x 1)))
