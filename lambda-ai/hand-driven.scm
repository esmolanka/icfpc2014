;; Helpers

(define (get-up rowcol)
  (cons (- (car rowcol) 1) (cdr rowcol)))

(define (get-down rowcol)
  (cons (+ (car rowcol) 1) (cdr rowcol)))

(define (get-left rowcol)
  (cons (car rowcol) (- (cdr rowcol) 1)))

(define (get-right rowcol)
  (cons (car rowcol) (+ (cdr rowcol) 1)))

(define (get-loc-in-direction rowcol direction)
  (cond ((== direction +up+)
         (get-up rowcol))
        ((== direction +down+)
         (get-down rowcol))
        ((== direction +left+)
         (get-left rowcol))
        ((== direction +right+)
         (get-right rowcol))))

;; Harness

(define (main world undocumented)
  (cons +down+ step))

;; Step function

(define (step state world)
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         (loc (swap (lm-location lman)))
         (prev-direction state)
         (dir (hand-driven prev-direction loc 0 wmap)))
    (cons dir dir)))

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

         (next-dir (cond ((non-blocked? wmap rot-loc1) rot-dir1)
                         ((non-blocked? wmap prev-loc) prev-dir)
                         ((non-blocked? wmap rot-loc2) rot-dir2)
                         (#t opp-dir))))
    (debug-it next-dir)))
