;;; Thu 29 Dec, 2011 (c) Marc Kuo
;;; Output functions
;;; --------------------------
(in-package :open-vrp.output)

;; Print solution
(defgeneric print-routes (solution)
  (:method (solution) "Expects <fleet> or <problem> object!")
  (:documentation "Prints solution given a <fleet>/<problem>/<algo> object. Also prints the total distance when the input is a <problem>/<algo> object."))

;; ignore empty vehicles
(defmethod print-routes ((f fleet))
  (format t "Solution:~%~{Route: ~A~^~%~}"
	  (remove-if #'(lambda (route) (null (cdr route))) (route-indices f))))

(defmethod print-routes ((prob problem))
  (print (fitness prob))
  (print-routes (problem-fleet prob)))

(defmethod print-routes ((a algo))
  (print-routes (algo-best-sol a)))