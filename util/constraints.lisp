;;; Thu 12 Jan, 2012
;;; Constraint checking functions
;;; --------------
(in-package :open-vrp.util)

(defgeneric in-capacityp (veh/fleet/problem)
  (:method (obj) "Expects a <Vehicle>/<Fleet>/<Problem> object!")
  (:documentation "Tests weather the route on <vehicle> is complying with the capacity constraint. When <Fleet> is provided, test all vehicles."))

(defmethod in-capacityp ((v vehicle))
  (labels ((iter (route cap)
	     (if (null route) T
		 (let ((demand (node-demand (car route))))
		   (and
		    (<= demand cap)
		    (iter (cdr route) (- cap demand)))))))
    (iter (vehicle-route v)
	  (vehicle-capacity v))))

(defmethod in-capacityp ((f fleet))
  (labels ((iter (veh)
	     (if (null veh) T
		 (and
		  (in-capacityp (car veh))
		  (iter (cdr veh))))))
    (iter (fleet-vehicles f))))

(defmethod in-capacityp ((pr problem))
  (in-capacityp (problem-fleet pr)))
	   
		       
		  