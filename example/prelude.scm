;; Tuple/lists accessors
(define (first xs) (car xs))
(define (second xs) (car (cdr xs)))
(define (third xs) (car (cdr (cdr xs))))
(define (fourth xs) (car (cdr (cdr (cdr xs)))))
(define (fifth xs) (car (cdr (cdr (cdr (cdr xs))))))

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

;; Debug helpers

(define (debug-it x)
  (debug x)
  x)




;; Game helpers

;; World tuple accessors.

;; World is a 4-tuple consisting of

;; 1. The map;
;; 2. the status of Lambda-Man;
;; 3. the status of all the ghosts;
;; 4. the status of fruit at the fruit location.

(define (get-world-map world)
  (first world))

(define (get-lman-status world)
  (second world))

(define (get-ghost-status world)
  (third world))

(define (get-fruit-status world)
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

(define (map-cell wmap row col)
  (nth col (nth row wmap)))

(define (wall? wmap row col)
  (== 0 (map-cell wmap row col)))
(define (empty? wmap row col)
  (== 1 (map-cell wmap row col)))
(define (pill? wmap row col)
  (== 2 (map-cell wmap row col)))
(define (power-pill? wmap row col)
  (== 3 (map-cell wmap row col)))
(define (fruit? wmap row col)
  (== 4 (map-cell wmap row col)))
(define (lman-start? wmap row col)
  (== 5 (map-cell wmap row col)))
(define (ghost-start? wmap row col)
  (== 6 (map-cell wmap row col)))

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

;; The status of the fruit is a number which is a countdown to the expiry of
;; the current fruit, if any.
;;   * 0: no fruit present;
;;   * n > 0: fruit present: the number of game ticks remaining while the
;;            fruit will will be present.

