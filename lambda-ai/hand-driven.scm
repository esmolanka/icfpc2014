;; Helpers

(define (get-up xy)
  (cons (car xy) (- (cdr xy) 1)))

(define (get-down xy)
  (cons (car xy) (+ (cdr xy) 1)))

(define (get-left xy)
  (cons (- (car xy) 1) (cdr xy)))

(define (get-right xy)
  (cons (+ (car xy) 1) (cdr xy)))

(define (get-loc-in-direction xy direction)
  (cond ((== direction +up+)
         (get-up xy))
        ((== direction +down+)
         (get-down xy))
        ((== direction +left+)
         (get-left xy))
        ((== direction +right+)
         (get-right xy))))

;; Harness

(define (main world undocumented)
  (cons (cons 0 +down+) step))

;; Step function

(define (step state world)
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         (loc (lm-location lman))

         (prev-direction (cdr state))
         (curr-time (car state))

         (next-time (+ 1 curr-time))

         (hand (let* ((r (div curr-time 5))
                      (h (mod r 2)))
                 h))
         (dir (hand-driven prev-direction loc hand wmap)))
    (cons (cons next-time dir) dir)))

;; Algo itself

(define (next-left-dir dir)
  (cond ((== dir 0) 3)
        (#t (- dir 1))))

(define (next-right-dir dir)
  (cond ((== dir 3) 0)
        (#t (+ dir 1))))

;; hand = 1 (right hand) or 0 (left hand)

(define (hand-driven prev-dir loc hand wmap)
  (let* ((rot-dir1 (cond ((== hand 1) (next-right-dir prev-dir))
                         ((== hand 0) (next-left-dir  prev-dir))))
         (opp-dir  (cond ((== hand 1) (next-right-dir rot-dir1))
                         ((== hand 0) (next-left-dir  rot-dir1))))
         (rot-dir2 (cond ((== hand 1) (next-right-dir opp-dir))
                         ((== hand 0) (next-left-dir  opp-dir))))

         (rot-loc1 (get-loc-in-direction loc rot-dir1))
         (opp-loc  (get-loc-in-direction loc opp-dir ))
         (rot-loc2 (get-loc-in-direction loc rot-dir2))
         (prev-loc (get-loc-in-direction loc prev-dir))

         (next-dir (cond ((useful-now? wmap rot-loc1) rot-dir1)
                         ((useful-now? wmap prev-loc) prev-dir)
                         ((useful-now? wmap rot-loc2) rot-dir2)
                         ((useful-now? wmap opp-loc)  opp-dir)

                         ((non-blocked? wmap rot-loc1) rot-dir1)
                         ((non-blocked? wmap prev-loc) prev-dir)
                         ((non-blocked? wmap rot-loc2) rot-dir2)
                         (#t opp-dir))))
    next-dir))
