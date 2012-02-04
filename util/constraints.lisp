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
  (:method (obj) "constraintsp: Expects <Problem> object!")
  (:documentation "Tests weather the solution in the <problem> is complying with the constraints. If the problem is a CVRP, check for capacity. If it is a VRPTW, check for capacity and time-windows."))

(defmethod constraintsp ((prob problem))
  T)

(defmethod constraintsp :around ((sol CVRP))
  (if (in-capacityp sol)
      (call-next-method)
      NIL))

(defmethod constraintsp :around ((sol VRPTW))
  (if (in-timep (problem-fleet sol))
      (call-next-method)
      NIL))

;; Helper macro for defining constraints-checking methods below
;; Returns NIL as soon as it finds out that a constraint is violated
(defmacro constraints-check (arglist init-forms next-forms testform endtest)
  (let ((iter (gensym)))
    `(labels ((,iter ,arglist
		(if ,endtest (values T ,@(cdr arglist))
		    (and
		     ,testform
		     (,iter ,@next-forms)))))
       (,iter ,@init-forms))))
;; -------------------------

;; 1. Capacity Constraints
;; ------------------------

(defgeneric in-capacityp (veh/problem)
  (:method (obj) "Expects a <Vehicle>/<Problem> object!")
  (:documentation "Tests weather the route on <vehicle> is complying with the capacity constraint. Returns T and the remaining capacity if it does. When <Problem> is provided, test all vehicles."))

 (defmethod in-capacityp ((v vehicle))
   (constraints-check
    (route cap)
    ((vehicle-route v) (vehicle-capacity v))
    ((cdr route) (- cap (node-demand (car route))))
    (<= (node-demand (car route)))
    (null route)))

(defmethod in-capacityp ((pr problem))
  (constraints-check
   (flt)
   ((problem-fleet pr))
   ((cdr flt))
   (in-capacityp (car flt))
   (null flt)))
	   
(defun node-fit-in-vehiclep (sol node-id vehicle-id)
  "Helper function for assessing constraints. Used by assess-move :around."
  (multiple-value-bind (comply cap-left) (in-capacityp (vehicle sol vehicle-id))
    (if (not comply) (error 'infeasible-solution :sol sol :func #'in-capacityp)
	(<= (node-demand (node sol node-id)) cap-left))))

;; ------------------------------

;; 2. Time-window constraints
;; -------------------------

(defun travel-time (n1 n2 &optional (speed 1))
  "Given two <nodes> and optional speed, return the travel-time."
  (if (= (node-id n1) (node-id n2)) 0
      (/ (node-distance n1 n2)
	 speed)))

(defun time-after-serving-node (node arrival-time)
  "Given a node to serve and the current time, return the new time (if on-time to begin with). When arrival-time is too early, wait till earliest start time."
  (cond ((> arrival-time (node-end node)) (error 'infeasible-solution :sol node :func arrival-time :msg "Arrival time is later than latest start-time of node"))
	((< arrival-time (node-start node)) (+ (node-start node) (node-duration node))) ;wait
	(t (+ arrival-time (node-duration node)))))

(defgeneric in-timep (veh/vrptw)
  (:method (veh/vrptw) "in-timep: Expects <Vehicle-TW>/<Problem>!")
  (:documentation "Tests weather the route on <Vehicle> is complying with the time-window constraints. Returns T and the time of finishing its last task. When <Fleet> is provided, test all vehicles."))

;; check for each node
;; arrival time is current time + travel time
;; if arrival time < end-time, on time (if not on time, return NIL - no need to check more)
;;   if arrival time < begin-time, wait until begin time -> set time to begin + duration
;;   if arrival after begin-time -> set time to arrival + duration

;; check for one vehicle
(defmethod in-timep ((v vehicle-TW))
  (symbol-macrolet ((to (car route))
		    (arr-time (+ time (travel-time loc to))))
    (constraints-check
     (route time loc)
     ((cdr (vehicle-route v)) 0 (car (vehicle-route v)))
     ((cdr route) (time-after-serving-node to arr-time) to)
     (<= arr-time (node-end to))
     (null route))))
    
;; check for whole fleet
(defmethod in-timep ((pr VRPTW))
  (constraints-check
   (veh)
   ((problem-fleet pr))
   ((cdr veh))
   (in-timep (car veh))
   (null veh)))
		  
		 
;; -------------------------