(define +done+ 42)

(define (main world undoc)
  (cons (power-pills-positions (world-map world)) step))

(define (debug-tuple t)
  (debug (car t))
  (debug (cdr t)))

(define (step state world)
  (debug-tuple (first state))
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         (loc (lm-location lman))
         (dir (dir-to-target loc (first state))))
    (if (== dir +done+)
        (cons (cdr state) +up+)
        (cons state dir))))

(define (dir-to-target from to)
  (let* ((dx (- (car to) (car from)))
         (dy (- (cdr to) (cdr from))))
    (if (and (== dx 0) (== dy 0))
        +done+
        (if (> (abs dy) (abs dx))
            ;; up or down
            (if (> dy 0) +up+ +down+)
            ;; left or right
            (if (> dx 0) +right+ +left+)))))

(define (filter-wmap pred? wmap)
  (let* ((size (map-size wmap))
         (width (cdr size))
         (height (car size))
         (xs (seq 0 (- width 1)))
         (ys (seq 0 (- height 1))))

    (concat-map
     (lambda (rowix-row)
       (concat-map
        (lambda (colix-cell)
          (if (pred? (cdr colix-cell))
              (list (cons (car colix-cell) (car rowix-row)))
              +nil+))
        (zip ys (cdr rowix-row))))
     (zip xs wmap))))

(define (power-pills-positions wmap)
  (filter-wmap (lambda (c) (== +power-pill+ c)) wmap))
