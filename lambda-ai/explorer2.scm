(define (main world undocumented)
  (let* ((wmap (world-map world))
         (pills (collect-pills wmap))
         (location (swap (lm-location (lm-status world)))))
    (cons (make-state +down+ location pills)
          step)))

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

(define (collect-pills wmap)
  (filter-wmap (lambda (c) (== +pill+ c)) wmap))

;; (define (collect-pills wmap)
;;   (letrec ((go
;;             (lambda (row wmap pills)
;;               (letrec ((go-row
;;                         (lambda (col cells pills)
;;                           (if (nil? cells)
;;                               (tailcall go (+ row 1) (cdr wmap) pills)
;;                               (let ((cell (car cells)))
;;                                 (tailcall go-row
;;                                           (+ col 1)
;;                                           (cdr cells)
;;                                           (if (== +pill+ cell)
;;                                               (cons cell pills)
;;                                               pills)))))))
;;                 (if (nil? wmap)
;;                     pills
;;                     (go-row 0 (car wmap)))))))
;;     (go 0 wmap +nil+)))

(define (make-state dir loc pills)
  (cons dir (cons loc pills)))

(define (state-dir state)
  (car state))

(define (state-loc state)
  (car (cdr state)))

(define (state-pills state)
  (cdr (cdr state)))


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

(define (loc= loc-a loc-b)
  (and (== (car loc-a) (car loc-b))
       (== (cdr loc-a) (cdr loc-b))))

(define (dir-to-target from to up-loc-free? down-loc-free? left-loc-free? right-loc-free?)
  (let* ((dx (- (car to) (car from)))
         (dy (- (cdr to) (cdr from)))
         (ydir (if (> dy 0)
                   (cons +down+ down-loc-free?)
                   (cons +up+ up-loc-free?)))
         (xdir (if (> dx 0)
                   (cons +right+ right-loc-free?)
                   (cons +left+ left-loc-free?))))
    (if (and (== dx 0) (== dy 0))
        +nil+
        (if (> (abs dy) (abs dx))
            ;; up or down
            (if (cdr ydir)
                (car ydir)
                (car xdir))
            ;; left or right
            (if (cdr xdir)
                (car xdir)
                (car ydir))))))

(define (step state world)
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         ;; (ghosts (ghost-status world))
         ;; (ghost-locations (map gh-location ghosts))
         (curr-loc (lm-location lman))
         (up-loc (get-up curr-loc))
         (down-loc (get-down curr-loc))
         (left-loc (get-left curr-loc))
         (right-loc (get-right curr-loc))
         (up-loc-free? (non-blocked? wmap up-loc))
         (down-loc-free? (non-blocked? wmap down-loc))
         (left-loc-free? (non-blocked? wmap left-loc))
         (right-loc-free? (non-blocked? wmap right-loc))
         (prev-direction (state-dir state))
         (prev-loc (state-loc state))
         (pills (state-pills state))
         (closest-pill (minimum-by (lambda (x) (manhattan-distance curr-loc x))
                                   pills))
         (pill-direction (if (nil? closest-pill)
                             +nil+
                             (dir-to-target curr-loc
                                            closest-pill
                                            up-loc-free?
                                            down-loc-free?
                                            left-loc-free?
                                            right-loc-free?)))
         (closest-pill-loc (get-loc-in-direction curr-loc
                                                 pill-direction))
         (dir (cond ((useful? wmap up-loc) +up+)
                    ((useful? wmap down-loc) +down+)
                    ((useful? wmap left-loc) +left+)
                    ((useful? wmap right-loc) +right+)
                    ((and (not (nil? pill-direction))
                          (non-blocked? wmap closest-pill-loc))
                     (debug-it pill-direction))
                    ((non-blocked? wmap
                                   (get-loc-in-direction curr-loc
                                                         prev-direction))
                     prev-direction)
                    ((and up-loc-free?
                          (and (not (loc= up-loc prev-loc))
                               #t
                               ;; (not (member-by loc= up-loc ghost-locations))
                               ))
                     +up+)
                    ((and left-loc-free?
                          (and (not (loc= left-loc prev-loc))
                               #t
                               ;; (not (member-by loc= left-loc ghost-locations))
                               ))
                     +left+)
                    ((and right-loc-free?
                          (and (not (loc= right-loc prev-loc))
                               #t
                               ;; (not (member-by loc= right-loc ghost-locations))
                               ))
                     +right+)
                    ((and down-loc-free?
                          (and (not (loc= down-loc prev-loc))
                               #t
                               ;; (not (member-by loc= down-loc ghost-locations))
                               ))
                     +down+)
                    (#t +left+))))
    (cons (make-state dir curr-loc (filter (lambda (p) (not (loc= p curr-loc))) pills)) dir)))

