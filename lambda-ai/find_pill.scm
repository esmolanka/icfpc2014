(define +done+ 42)

(define (main world undoc)
  (cons
   (cons +down+ (power-pills-positions (world-map world))) step))

(define (step state world)
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         (loc (lm-location lman))
         (curr-dir (car state))
         (curr-pill (first (cdr state)))
         (neighbors (free-neighbors wmap loc))
         (dir (get-dir wmap loc curr-pill curr-dir neighbors)))
    (if (== dir +done+)
        (cons (cons dir (cdr (cdr state))) +up+)
        (cons (cons dir (cdr state)) dir))))

(define (free-neighbors wmap xy)
  (concat
   (list
    (if (wall? wmap (get-up xy)) +nil+ (list +up+))
    (if (wall? wmap (get-down xy)) +nil+ (list +down+))
    (if (wall? wmap (get-left xy)) +nil+ (list +left+))
    (if (wall? wmap (get-right xy)) +nil+ (list +right+)))))

(define (other dir ns) ;; assumes that n is list of two elems
  (if (== dir (opposite (first ns)))
      (second ns)
      (first ns)))

(define (next-cell wmap xy dir)
  (let ((next (cond
               ((== dir +up+) (get-up xy))
               ((== dir +down+) (get-down xy))
               ((== dir +left+) (get-left xy))
               ((== dir +right+) (get-right xy)))))
    (map-cell wmap next)))

(define (done? from to)
  (and (== (car from) (car to))
       (== (cdr from) (cdr to))))

(define (get-dir wmap from to curr-dir neighbors)
  (let ((n (debug-it (length neighbors))))
    (if (done? from to)
        +done+

        (cond
         ((== n 1) (opposite curr-dir)) ;; dead end
         ((== n 2) (other curr-dir neighbors)) ;; corridor
         ((== n 3)
                          ;; 3-junction
          (let ((dir (dir-to-target from to)))
            (if (== +wall+ (next-cell wmap from dir))
                (opposite curr-dir) ;; turn back if we hit the wall
                dir)))
          ;; (dir-to-target from to))
         (#t (dir-to-target from to)))))) ;; 4-junction

(define (dir-to-target from to)
  (let* ((dx (- (car to) (car from)))
         (dy (- (cdr to) (cdr from))))
    (if (done? from to)
        +done+
        (if (> (abs dy) (abs dx))
            ;; up or down
            (if (> dy 0) +down+ +up+)
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
        (zip xs (cdr rowix-row))))
     (zip ys wmap))))

(define (power-pills-positions wmap)
  (filter-wmap (lambda (c) (== +power-pill+ c)) wmap))
