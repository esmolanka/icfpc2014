(define (main world undocumented)
  (cons (cons +up+ +nil+) step))

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

(define (loc= loc-a loc-b)
  (and (== (car loc-a) (car loc-b))
       (== (cdr loc-a) (cdr loc-b))))

(define (step state world)
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         (ghosts (ghost-status world))
         (ghost-locations (map gh-location ghosts))
         (loc (swap (lm-location lman)))
         (up-loc (get-up loc))
         (down-loc (get-down loc))
         (left-loc (get-left loc))
         (right-loc (get-right loc))
         (prev-direction (car state))
         (prev-loc (cdr state))
         (dir (cond ((useful? wmap up-loc) +up+)
                    ((useful? wmap down-loc) +down+)
                    ((useful? wmap left-loc) +left+)
                    ((useful? wmap right-loc) +right+)
                    ((non-blocked? wmap
                                   (get-loc-in-direction loc
                                                         prev-direction))
                     prev-direction)
                    ((and (non-blocked? wmap up-loc)
                          (and (not (loc= up-loc prev-loc))
                               (not (member-by loc= up-loc ghost-locations))))
                     +up+)
                    ((and (non-blocked? wmap left-loc)
                          (and (not (loc= left-loc prev-loc))
                               (not (member-by loc= left-loc ghost-locations))))
                     +left+)
                    ((and (non-blocked? wmap right-loc)
                          (and (not (loc= right-loc prev-loc))
                               (not (member-by loc= right-loc ghost-locations))))
                     +right+)
                    ((and (non-blocked? wmap down-loc)
                          (and (not (loc= down-loc prev-loc))
                               (not (member-by loc= down-loc ghost-locations))))
                     +down+)
                    (#t +left+))))
    (cons (cons dir loc) dir)))
