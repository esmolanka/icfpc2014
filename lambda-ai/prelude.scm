;; Tuple/lists accessors
(define (first xs) (car xs))
(define (second xs) (car (cdr xs)))
(define (third xs) (car (cdr (cdr xs))))
(define (fourth xs) (car (cdr (cdr (cdr xs)))))
(define (fifth xs) (car (cdr (cdr (cdr (cdr xs))))))

;; unsafe when index is out of bounds
(define (nth i xs)
  (letrec ((go (lambda (i xs)
                  (if (== i 0)
                      (car xs)
                      (tailcall go (- i 1) (cdr xs))))))
    (go i xs)))

(define (div x y) (/ x y))
(define (mod x y) (- x (* y (div x y))))

(define (manhattan-distance x y)
  (+ (abs (- (car x) (car y)))
     (abs (- (cdr x) (cdr y)))))

(define (minimum-by f xs)
  (letrec ((go (lambda (xs min)
                  (if (nil? xs)
                      min
                      (let ((new-min (f (car xs))))
                        (tailcall go
                                  (cdr xs)
                                  (if (> min new-min)
                                      new-min
                                      min)))))))
    (if (nil? xs)
        +nil+
        (go (cdr xs) (f (car xs))))))

(define (member-by eq-pred x xs)
  (cond ((nil? xs)
         #f)
        ((eq-pred x (car xs))
         #t)
        (#t
         (member-by eq-pred x (cdr xs)))))

;; (x,y) -> (y,x)
(define (swap tuple)
  (cons (cdr tuple) (car tuple)))

;; Checks for empty list.
;; NB: atom? check is necessary, otherwise we'll get tag error in
;; runtime
(define (nil? xs) (and (atom? xs) (== xs +nil+)))

(define +nil+ 0)

;; Pattern match for lists.
(define (match xs nil_case cons_case)
  (if (nil? xs)
      nil_case
      (cons_case (car xs) (cdr xs))))

;; Usual HOF combinators (can be redefined using "match", but we don't
;; have lambda lifting yet to make it convenient

(define (map f xs)
  (if (nil? xs)
      +nil+
      (cons (f (car xs)) (map f (cdr xs)))))

(define (filter pred xs)
  (if (nil? xs)
      +nil+
      (if (pred (car xs))
          (cons (car xs) (filter pred (cdr xs)))
          (filter pred (cdr xs)))))

;; uses Haskell foldr argument order
(define (foldr f acc xs)
  (if (nil? xs)
      acc
      (f (car xs) (foldr f acc (cdr xs)))))

(define (foldl f acc xs)
  (letrec ((go (lambda (acc xs)
                 (if (nil? xs)
                     acc
                     (tailcall go (f acc (car xs)) (cdr xs))))))
    (go acc xs)))

(define (reverse lst) (foldl (lambda (x y) (cons y x)) 0 lst))

(define (sum xs) (foldl (lambda (a b) (+ a b)) 0 xs))

(define (length xs)
  (letrec ((iter (lambda (xs n)
                   (if (nil? xs)
                       n
                       (tailcall iter (cdr xs) (+ n 1))))))
    (iter xs 0)))

(define (abs x)
  (if (> x 0)
      x (- 0 x)))

(define (zip xs ys)
  (if (nil? xs)
      +nil+
      (cons (cons (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

(define (concat xs)
  (foldr append +nil+ xs))

(define (append xs ys)
  (if (nil? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (concat-map f xs)
  (concat (map f xs)))

(define (seq from to)
  (if (> from to)
      +nil+
      (cons from (seq (+ from 1) to))))

;; Debug helpers

(define (debug-it x)
  (debug x)
  x)

(define (negate x)
  (- 0 x))

;; Game helpers

;; World tuple accessors.

;; World is a 4-tuple consisting of

;; 1. The map;
;; 2. the status of Lambda-Man;
;; 3. the status of all the ghosts;
;; 4. the status of fruit at the fruit location.

(define (world-map world)
  (first world))

(define (lm-status world)
  (second world))

(define (ghost-status world)
  (third world))

(define (fruit-status world)
  (fourth world))


;; The map is encoded as a list of lists (row-major) representing the 2-d
;; grid. An enumeration represents the contents of each grid cell:

;;   * 0: Wall (`#`)
;;   * 1: Empty (`<space>`)
;;   * 2: Pill
;;   * 3: Power pill
;;   * 4: Fruit location
;;   * 5: Lambda-Man starting position
;;   * 6: Ghost starting position

(define +wall+ 0)
(define +empty+ 1)
(define +pill+ 2)
(define +power-pill+ 3)
(define +fruit+ 4)
(define +lm-start+ 5)
(define +ghost-start+ 6)

(define (map-cell wmap xy)
  (nth (car xy) (nth (cdr xy) wmap)))

(define (wall? wmap xy)
  (== +wall+ (map-cell wmap xy)))
(define (empty? wmap xy)
  (== +empty+ (map-cell wmap xy)))
(define (pill? wmap xy)
  (== +pill+ (map-cell wmap xy)))
(define (power-pill? wmap xy)
  (== +power-pill+ (map-cell wmap xy)))
(define (fruit? wmap xy)
  (== +fruit+ (map-cell wmap xy)))
(define (lman-start? wmap xy)
  (== +lm-start+ (map-cell wmap xy)))
(define (ghost-start? wmap xy)
  (== +ghost-start+ (map-cell wmap xy)))

(define (useful? wmap xy)
  (or (pill? wmap xy)
      (or (power-pill? wmap xy)
          (fruit? wmap xy))))

(define (non-blocked? wmap xy)
  (not (== +wall+ (map-cell wmap xy))))

;; height x width
(define (map-size wmap)
  (cons
   (length wmap)
   (length (car wmap))))

;; The Lambda-Man status is a 5-tuple consisting of:
;;   1. Lambda-Man's vitality;
;;   2. Lambda-Man's current location, as an (x,y) pair;
;;   3. Lambda-Man's current direction;
;;   4. Lambda-Man's remaining number of lives;
;;   5. Lambda-Man's current score.

;; Lambda-Man's vitality is a number which is a countdown to the expiry of
;; the active power pill, if any. It is 0 when no power pill is active.
;;   * 0: standard mode;
;;   * n > 0: power pill mode: the number of game ticks remaining while the
;;            power pill will will be active

(define (lm-vitality lm-status)
  (first lm-status))

(define (lm-location lm-status)
  (second lm-status))

(define (lm-direction lm-status)
  (third lm-status))

(define (lm-lives lm-status)
  (fourth lm-status))

(define (lm-score lm-status)
  (fifth lm-status))

;; The status of all the ghosts is a list with the status for each ghost.
;; The list is in the order of the ghost number, so each ghost always appears
;; in the same location in the list.

;; The status for each ghost is a 3-tuple consisting of
;;   1. the ghost's vitality
;;   2. the ghost's current location, as an (x,y) pair
;;   3. the ghost's current direction

(define (gh-no n ghosts)
  (nth n ghosts))

(define (gh-vitality gh-status)
  (first gh-status))

(define (gh-location gh-status)
  (second gh-status))

(define (gh-direction gh-status)
  (third gh-status))

;; The Ghosts' vitality is an enumeration:
;;   * 0: standard;
;;   * 1: fright mode;
;;   * 2: invisible.

(define (gh-standard? gh-vit)
  (== 0 gh-vit))

(define (gh-frightened? gh-vit)
  (== 1 gh-vit))

(define (gh-invisible? gh-vit)
  (== 2 gh-vit))


;; The Ghosts' and Lambda-Man's direction is an enumeration:
;;   * 0: up;
;;   * 1: right;
;;   * 2: down;
;;   * 3: left.

(define +up+ 0)
(define +right+ 1)
(define +down+ 2)
(define +left+ 3)

