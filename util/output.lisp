;;; Thu 29 Dec, 2011 (c) Marc Kuo
;;; Output functions
;;; --------------------------
(in-package :open-vrp.output)

;; Print solution
(defmethod route-indices ((f fleet))
  (mapcar #'route-indices (fleet-vehicles f)))

(defmethod route-indices ((prob problem))
  (route-indices (problem-fleet prob)))

(defgeneric print-routes (solution)
  (:method (solution) "Expects <fleet> or <problem> object!")
  (:documentation "Prints solution given a <fleet> object. Also prints the total distance when the input is a <problem> object."))

(defmethod print-routes ((f fleet))
  (format t "Solution:~%~{Route: ~A~^~%~}" (route-indices f)))

(defmethod print-routes ((prob problem))
  (print (fitness prob))
  (print-routes (problem-fleet prob)))

