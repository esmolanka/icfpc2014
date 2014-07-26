(define (map f xs)
    (if (and (atom? xs)
             (== 0 xs))
             0
             (cons (f (car xs))
                   (map f (cdr xs)))))


(define (main x y)
  (let* ((y 3)
         (foo (lambda (x) (* y x))))
    (map foo (list 1 2 3))))

