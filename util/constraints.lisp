;;; Constraint checking functions
;;; --------------
;;; 0. General Constraints Checker tools
;;; 1. Capacity Constraints
;;; 2. Time-window Constraints
;;; ---------------
(in-package :open-vrp.util)

;; 0. General tools/definitions
;; ----------------------------

(defgeneric constraintsp (prob)
  (:documentation "Tests weather the solution in the <problem> is complying with the constraints. If the problem is a CVRP, check for capacity. If it is a VRPTW, check for time-windows. For CVRPTW, that inherits from both classes, check both constraints.")
  (:method-combination and))

(defmethod constraintsp and ((prob problem)) T)

(defmethod constraintsp and ((sol CVRP)) (in-capacityp sol))

(defmethod constraintsp and ((sol VRPTW)) (in-timep (problem-fleet sol)))

;; Helper macro for defining constraints-checking methods below
;; Returns NIL as soon as it finds out that a constraint is violated
(defmacro constraints-check (arglist init-forms next-forms testform &optional endtest)
  (let ((iter (gensym)))
    `(labels ((,iter ,arglist
		(if ,@(if endtest `(,endtest) `((null ,(car arglist))))
		    (values T ,@(cdr arglist))
		    (and
		     ,testform
		     (,iter ,@next-forms)))))
       (,iter ,@init-forms))))
;; -------------------------

;; 1. Capacity Constraints
;; ------------------------

(defgeneric in-capacityp (veh/cvrp)
  (:method (obj) "Expects a <Vehicle>/<CVRP> object!")
  (:documentation "Tests weather the route on <vehicle> is complying with the capacity constraint. Returns T and the remaining capacity if it does. When <CVRP> is provided, test all vehicles."))

(defmethod in-capacityp ((v vehicle))
  (unless (vehicle-capacity v) (error 'no-capacities-vehicle :veh v))
  (constraints-check
   (route cap)
   ((vehicle-route v) (vehicle-capacity v))
   ((cdr route) (- cap (node-demand (car route))))
   (<= (node-demand (car route)) cap)))

(defmethod in-capacityp ((pr CVRP))
  (constraints-check
   (flt)
   ((problem-fleet pr))
   ((cdr flt))
   (in-capacityp (car flt))))
	   
;; ------------------------------

;; 2. Time-window constraints
;; -------------------------

(defun travel-time (n1 n2 &optional (speed 1))
  "Given two <nodes> and optional speed, return the travel-time."
  (handler-case
      (/ (node-distance n1 n2) speed)
    (same-origin-destination () 0)))
	 
(defun time-after-serving-node (node arrival-time)
  "Given a node to serve and the current time, return the new time (if on-time to begin with). When arrival-time is too early, wait till earliest start time."
  (cond ((> arrival-time (node-end node)) (error 'infeasible-solution :sol node :func arrival-time :msg "Arrival time is later than latest start-time of node"))
	((< arrival-time (node-start node)) (+ (node-start node) (node-duration node))) ;wait
	(t (+ arrival-time (node-duration node)))))

(defgeneric in-timep (veh/vrptw)
  (:method (veh/vrptw) "in-timep: Expects <Vehicle-TW>/<Problem>!")
  (:documentation "Tests weather the route on <Vehicle> is complying with the time-window constraints. Returns T and the time of finishing its last task. When <Fleet> is provided, test all vehicles."))

(defmethod in-timep ((v vehicle))
  (unless (vehicle-speed v) (error 'no-speed-vehicle :veh v))
  (symbol-macrolet ((to (car route))
		    (arr-time (+ time (travel-time loc to))))
    (constraints-check
     (route time loc)
     ((cdr (vehicle-route v)) 0 (car (vehicle-route v)))
     ((cdr route) (time-after-serving-node to arr-time) to)
     (<= arr-time (node-end to)))))
    
(defmethod in-timep ((pr VRPTW))
  (constraints-check
   (veh)
   ((problem-fleet pr))
   ((cdr veh))
   (in-timep (car veh))))
;; -------------------------

;; Feasibility check of insertion
;; ----------------------------

(defun node-fit-in-vehiclep (sol node-id vehicle-id)
  "Helper function for assessing constraints. Used by assess-move :around."
  (multiple-value-bind (comply cap-left) (in-capacityp (vehicle sol vehicle-id))
    (if (not comply) (error 'infeasible-solution :sol sol :func #'in-capacityp)
	(<= (node-demand (node sol node-id)) cap-left))))

