;;;; Benchmark utilities
;;;; --------------------
;;;; Dump -- needs fixing! @mck- Oct 1, 2013

(defun distance-coords (x1 y1 x2 y2)
  "Calculates pythagoras distance"
  (flet ((square (x)
           (* x x)))
    (sqrt (+ (square (- x1 x2)) (square (- y1 y2))))))

(defun distance-coord-pair (n1 n2)
  "Calculates distance given two coord pairs. Returns NIL if both coords are the same."
  (if (eql n1 n2)
      NIL
      (distance-coords (car n1) (cdr n1)
                       (car n2) (cdr n2))))

(defun generate-dist-array (coord-list)
  "Given a list of coord pairs, generate an array of distances."
  (let* ((size (length coord-list))
         (dist-array (eval `(make-array '(,size ,size) :initial-element nil))))
    (map0-n #'(lambda (x)
                (map0-n #'(lambda (y)
                            (setf (aref dist-array x y)
                                  (distance-coord-pair (nth x coord-list)
                                                       (nth y coord-list))))
                        (1- size)))
            (1- size))
    dist-array))
