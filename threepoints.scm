;Nate Salima

(define (make-point x-cor y-cor)
  (cond
    (
     (and
      (number? x-cor)
      (number? y-cor)
      )
     (cons x-cor y-cor)
     )
    (else (display `("Not a Point"))
          (newline)
          )
    )
  )

(define (get-x point)
  (car point))

(define (get-y point)
  (cdr point))

(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)
    )
  )

(define (area point1 point2 point3)
  (abs
   (/
    (+
     (*
      (get-x point1)
      (-
       (get-y point2)
       (get-y point3)
       )
      )
     (*
      (get-x point2)
      (-
       (get-y point3)
       (get-y point1)
       )
      )
     (*
      (get-x point3)
      (-
       (get-y point1)
       (get-y point2)
       )
      )
     )
    2)
   )
  )

(define (is-line point1 point2 point3)
  (eq?
   (area point1 point2 point3)
   0)
  )

(define (distance point1 point2)
  (sqrt
   (+
    (expt
     (-
      (get-x point1)
      (get-x point2)
      )
     2)
    (expt
     (-
      (get-y point1)
      (get-y point2)
      )
     2)
    )
   )
  )

(define (perimeter point1 point2 point3)
  (+
   (distance point1 point2)
   (distance point2 point3)
   (distance point3 point1)
   )
  )
(define (angles-rad point1 point2 point3)
  (acos
   (/
    (-
     (+
      (expt (distance point2 point3) 2)
      (expt (distance point3 point1) 2)
      )
     (expt (distance point1 point2) 2)
     )
    (*
     2
     (distance point2 point3)
     (distance point3 point1)
     )
    )
   )
  )

(define (angles-deg point1 point2 point3)
  (round-off
   (/
    (*
     (angles-rad point1 point2 point3)
     180
     )
    3.14159265358979
    )
   5)
  )
   
(define (calculate-triangle point1 point2 point3)
  (cond ((> (area point1 point2 point3) 0)
         (display `("Side 1 = ", (round-off (distance point1 point2) 3)))
         (newline)
         (display `("Side 2 = ", (round-off (distance point2 point3) 3)))
         (newline)
         (display `("Side 3 = ", (round-off (distance point3 point1) 3)))
         (newline)
         (display `("Perimeter = ", (round-off (perimeter point1 point2 point3) 3)))
         (newline)
         (display `("Area = ", (round-off(area point1 point2 point3) 3)))
         (newline)
         (display `("Angle 1 = ", (round-off(angles-rad point1 point2 point3) 5), " ", (angles-deg point1 point2 point3)))
         (newline)
         (display `("Angle 2 = ", (round-off(angles-rad point2 point3 point1) 5), " ", (angles-deg point2 point3 point1)))
         (newline)
         (display `("Angle 3 = ", (round-off(angles-rad point3 point1 point2) 5), " ", (angles-deg point3 point1 point2)))
         (newline)
         )
        (else (display `("Not A Triangle"))
              (newline)
              )
        )
  )