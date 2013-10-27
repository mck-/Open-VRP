;;; Fitness functions for each type of problem
;;; -----------------------------------------
(in-package :open-vrp.util)

(defparameter *unserved-penalty* 1000)

(defun total-penalty (prob)
  "Given a problem, calculate total fitness penalty"
  (* *unserved-penalty* (length (problem-unserved prob))))

(defgeneric fitness (problem)
  (:method (problem) "Parameter is not a <problem> object!")
  (:documentation "The generic fitness function. To be defined for each class of <problem> specifically. This function allows for custom fitness-functions for your own defined <problem> classess. The default fitness function is total distance."))

(defmethod fitness ((prob problem))
  (if (problem-allow-unserved prob)
      (values
       (+ (total-dist prob) (total-penalty prob))
       (constraints-p prob))
      (values
       (total-dist prob)
       (constraints-p prob))))
