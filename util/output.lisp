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
  (format t "~&~{Route: ~A~^~%~}"
	  (remove-if #'(lambda (route) (null (cdr route))) (route-indices f)))
  (format t "~&---------------"))

(defmethod print-routes ((prob problem))
  (format t "~&---------------")
  (format t "~&Fitness: ~A" (fitness prob))
  (format t "~&---------------")
  (print-routes (problem-fleet prob)))

(defmethod print-routes ((a algo))
  (print-routes (algo-best-sol a)))