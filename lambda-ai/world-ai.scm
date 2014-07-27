(define (main world undocumented)
  (cons 0 step)) ;; define initial state here

(define (step state world)
  (let ((wmap (get-world-map world)))
    (let ((wsize (map-size wmap)))
      (let ((width (car wsize))
            (height (cdr wsize)))
        (cons (+ (debug-it width) (debug-it height)) 1)))))

