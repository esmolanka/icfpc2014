;; Tuple/lists accessors
(define (first xs) (car xs))
(define (second xs) (car (cdr xs)))
(define (third xs) (car (cdr (cdr xs))))
(define (fourth xs) (car (cdr (cdr (cdr xs)))))

;; unsafe when index is out of bounds
(define (nth i xs)
  (if (== i 0)
      (car xs)
      (nth (- i 1) (cdr xs))))

;; Checks for empty list.
;; NB: atom? check is necessary, otherwise we'll get tag error in
;; runtime
(define (nil? xs) (and (atom? xs) (== xs 0)))

;; Pattern match for lists.
(define (match xs nil_case cons_case)
  (if (nil? xs)
      nil_case
      (cons_case (car xs) (cdr xs))))

;; Usual HOF combinators (can be redefined using "match", but we don't
;; have lambda lifting yet to make it convenient

(define (map f xs)
  (if (nil? xs)
      0
      (cons (f (car xs)) (map f (cdr xs)))))

(define (filter pred xs)
  (if (nil? xs)
      0
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs)))))

;; uses Haskell foldr argument order
(define (foldr f acc xs)
  (if (nil? xs)
      acc
      (f (car xs) (foldr f acc (cdr xs)))))

(define (length xs)
  (if (nil? xs)
      0
      (+ 1 (length (cdr xs)))))


