(define (main world undocumented)
  (cons 0 step))

(define (get-up xy)
  (cons (car xy) (- (cdr xy) 1)))

(define (get-down xy)
  (cons (car xy) (+ (cdr xy) 1)))

(define (get-left xy)
  (cons (- (car xy) 1) (cdr xy)))

(define (get-right xy)
  (cons (+ (car xy) 1) (cdr xy)))

(define (step state world)
  (let* ((wmap (world-map world))
         (lman (lm-status world))
         (loc (lm-location lman))
         (up (get-up loc))
         (down (get-down loc))
         (left (get-left loc))
         (right (get-right loc))
         (dir (cond ((useful? wmap up) +up+)
                    ((useful? wmap down) +down+)
                    ((useful? wmap left) +left+)
                    ((useful? wmap right) +right+)
                    ((non-blocked? wmap up) +up+)
                    ((non-blocked? wmap right) +right+)
                    ((non-blocked? wmap down) +down+)
                    ((non-blocked? wmap left) +left+)
                    (#t 42))))
    (cons 0 dir)))
