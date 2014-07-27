(define (next-right-dir dir)
  (cond ((== dir 0) 3)
        (#t (- dir 1))))

(define (next-left-dir dir)
  (cond ((== dir 3) 0)
        (#t (+ dir 1))))

;; hand = 1 (right hand) or 0 (left hand)
(define (hand-driven prev-dir loc hand wmap)
  (let* ((next-loc (get-loc-in-direction loc prev-dir))
         (rotated-dir (cond ((== hand 1) (next-right-dir prev-dir))
                            ((== hand 0) (next-left-dir  prev-dir))))

         (next-dir (cond ((non-blocked? wmap next-loc) prev-dir)
                         (#t (hand-driven next-dir loc hand wmap)))))
    next-dir)
