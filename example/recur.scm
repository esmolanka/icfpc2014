
(define (main x y)
  (cons
   (cons
    (letrec ((foo (lambda (x r)
               (if-then-recur
                (== x 0)
                r
                (foo (- x 1) (* x r))))))
      (foo 5 1))
    (fac 5))
   (mut 5)))

(define (fac x)
 (letrec ((go (lambda (x r)
             (if-then-recur (== 0 x)
                            r
                            (go (- x 1) (* r x))))))
   (go x 1)))

(define (mut n)
  (letrec ( (to (lambda (n)
               (if-then-recur (== n 0)
                   42
                   (go (+ n 1)))))
         (go (lambda (n) (to (- n 2)))))
    (to n)))
