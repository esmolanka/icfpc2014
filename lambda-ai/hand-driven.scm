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

(define (next-right-dir dir)
  (cond ((== dir 0) 3)
        (#t (- dir 1))))

(define (next-left-dir dir)
  (cond ((== dir 3) 0)
        (#t (+ dir 1))))

;; hand = 1 (right hand) or 0 (left hand)
(define (hand-driven prev-dir loc hand wmap)
  (letrec ((next-loc (get-loc-in-direction loc prev-dir))
           (rotated-dir (cond ((== hand 1) (next-right-dir prev-dir))
                              ((== hand 0) (next-left-dir  prev-dir))))

           (next-dir (cond ((non-blocked? wmap next-loc) prev-dir)
                           (#t (hand-driven next-dir loc hand wmap)))))
    next-dir))
