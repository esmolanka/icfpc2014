(define (main world undocumented)
  (parse-world world))

(define (parse-world world)
  (let ((world_map (first world))
        (lman_status (second world))
        (ghosts_status (third world))
        (fruit_status (fourth world)))
    (rows_number world_map)))

(define (rows_number xs) (length xs))
