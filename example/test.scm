(define (main world undocumented)
  (map square (list 1 2 3 4 5)))

(define (square x)
  (* x x))

(define (map f xs)
  (if (and (atom? xs)
           (== 0 xs))
      0
      (cons (f (car xs))
            (map f (cdr xs)))))

