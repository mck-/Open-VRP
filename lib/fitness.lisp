;;; Fitness functions for each type of problem
;;; -----------------------------------------
(in-package :open-vrp.util)

(defgeneric fitness (problem)
  (:method (problem) "Parameter is not a <problem> object!")
  (:documentation "The generic fitness function. To be defined for each class of <problem> specifically. This function allows for custom fitness-functions for your own defined <problem> classess. The default fitness function is total distance."))

(defmethod fitness ((prob problem))
  (values
   (total-dist prob)
   (constraintsp prob)))
