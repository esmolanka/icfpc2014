(define +done+ 42)

;; modes
(define +search+ 0)
(define +cycle-break+ 1)

(define +cycle-limit+ 30)
(define +hand-limit+ 15)

(define (loc= loc-a loc-b)
  (and (== (car loc-a) (car loc-b))
       (== (cdr loc-a) (cdr loc-b))))

(define (main world undoc)
  (cons
   (list +down+
         (cons 0 0)
         0
         0
         +search+
         (interesting-objects (world-map world)))
   step))

(define (step state world)
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         (loc (lm-location lman))

         (curr-dir (first state))
         (period-start (second state))
         (period-counter (third state))
         (hand-counter (fourth state))
         (mode (fifth state))
         (all-pills (nth 5 state))
         (curr-pill (first all-pills))

         (gstatus (ghost-status world))

         (period-start-new
          (if (== period-counter 0)
              loc period-start))

         (mode-new
          (if (== mode +search+)
              (if (and (> period-counter 0) (loc= period-start loc)) +cycle-break+ +search+)
              (if (> hand-counter +hand-limit+) +search+ +cycle-break+)))

         (period-counter-new
          (if (== mode +search+)
              (mod (+ 1 period-counter) +cycle-limit+) 0))

         (hand-counter-new
          (if (== mode +cycle-break+)
              (+ 1 hand-counter) 0))

         (neighbors (free-neighbors wmap loc))

         (dir (get-dir wmap loc curr-pill curr-dir neighbors mode-new))
         (safe-dir (safe-direction dir loc (get-loc-in-direction loc dir) (ghost-locations wmap gstatus))))

    (debug (list mode (length all-pills) curr-pill))

    (cons

     (list
      safe-dir
      period-start-new
      period-counter-new
      hand-counter-new
      mode-new
      (filter (lambda (x) (not (loc= x loc))) all-pills))

     (if (== dir +done+) +up+ safe-dir))))

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

(define (get-dir wmap from to curr-dir neighbors mode)
  (cond
   ((== mode +cycle-break+) (hand-driven curr-dir from 1 wmap))
   (#t (search-driven wmap from to curr-dir neighbors))))

(define (search-driven wmap from to curr-dir neighbors)
  (let ((n (length neighbors)))
    (if (done? from to)
        +done+

        (cond
         ((== n 1) (opposite curr-dir)) ;; dead end
         ((== n 2) (other curr-dir neighbors)) ;; corridor
         ((== n 3) ;; 3-junction
          (let ((dir (dir-to-target from to)))
            (if (== +wall+ (next-cell wmap from dir))
                (opposite curr-dir) ;; turn back if we hit the wall
                dir)))
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

(define (interesting-objects wmap)
  (let ((power-pills (filter-wmap (lambda (c) (== +power-pill+ c)) wmap))
        (pills (filter-wmap (lambda (c) (== +pill+ c)) wmap)))
    (append power-pills pills)))

;; ghost-locations :: [Ghost] -> [Location]
(define (ghost-locations wmap ghosts)
  (concat-map (lambda (ghost)
                (let ((loc (gh-location ghost)))
                  (filter (lambda (loc)
                            (non-blocked? wmap loc))
                          (list (get-up loc)
                                (get-down loc)
                                (get-right loc)
                                (get-left loc)))))
              ghosts))

;; update-step :: Direction -> Location -> [Location] -> Direction
(define (safe-direction curr-dir curr-loc next-loc ghost-locations)
  (if (member-by loc= next-loc ghost-locations)
      (let ((new-dirs
             (filter (lambda (dir)
                       (let ((new-loc
                              (get-loc-in-direction curr-loc dir)))
                         (not (member-by loc= new-loc ghost-locations))))
                     (list +left+
                           +right+
                           +down+
                           +up+))))
        (if (nil? new-dirs)
            curr-dir
            (car new-dirs)))
      curr-dir))
