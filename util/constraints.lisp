;;; Thu 12 Jan, 2012
;;; Constraint checking functions
;;; --------------
(in-package :open-vrp.util)

;; General Constraints Checker
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
(defmacro constraints-check (arglist init-forms next-forms testform)
  `(labels ((iter ,arglist
	      (if (null ,(car arglist)) (values T ,@(cdr arglist))
		  (and
		   ,testform
		   (iter ,@next-forms)))))
     (iter ,@init-forms)))
;; -------------------------

;; Capacity Constraints
;; ------------------------

(defgeneric in-capacityp (veh/fleet/problem)
  (:method (obj) "Expects a <Vehicle>/<Fleet>/<Problem> object!")
  (:documentation "Tests weather the route on <vehicle> is complying with the capacity constraint. Returns T and the remaining capacity if it does. When <Fleet> is provided, test all vehicles."))

 (defmethod in-capacityp ((v vehicle))
   (constraints-check
    (route cap)
    ((vehicle-route v) (vehicle-capacity v))
    ((cdr route) (- cap (node-demand (car route))))
    (<= (node-demand (car route)))))   

(defmethod in-capacityp ((f fleet))
  (constraints-check
   (flt)
   ((fleet-vehicles f))
   ((cdr flt))
   (in-capacityp (car flt))))
		  
(defmethod in-capacityp ((pr problem))
  (in-capacityp (problem-fleet pr)))
	   
(defun node-fit-in-vehiclep (sol node-id vehicle-id)
  "Helper function for assessing constraints. Used by assess-move :around."
  (multiple-value-bind (comply cap-left) (in-capacityp (vehicle sol vehicle-id))
    (if (not comply) (error "node-fit-in-vehiclep: The solution was infeasible to begin with!")
	(<= (node-demand (node sol node-id)) cap-left))))

;; ------------------------------

;; Time-window constraints
;; -------------------------

(defmethod travel-time ((n1 node) (n2 node) &optional (speed 1))
  (if (= (node-id n1) (node-id n2)) 0
      (/ (node-distance n1 n2)
	 speed)))

(defun time-after-serving-node (node arrival-time)
  "Given a node to serve and the current time, return the new time (if on-time to begin with). When arrival-time is too early, wait till earliest start time."
  (cond ((> arrival-time (node-end node)) (error "time-after-serving-node - too late!"))
	((< arrival-time (node-start node)) (+ (node-start node) (node-duration node))) ;wait
	(t (+ arrival-time (node-duration node)))))

(defgeneric in-timep (veh/fleet)
  (:method (veh/fleet) "in-timep: Expects <Vehicle-TW>/<Fleet>!")
  (:documentation "Tests weather the route on <Vehicle> is complying with the time-window constraints. Returns T and the time of finishing its last task. When <Fleet> is provided, test all vehicles."))

;; check for each node
;; arrival time is current time + travel time
;; if arrival time < end-time, on time (if not on time, return NIL - no need to check more)
;;   if arrival time < begin-time, wait until begin time -> set time to begin + duration
;;   if arrival after begin-time -> set time to arrival + duration

;; check for one vehicle
(defmethod in-timep ((v vehicle-TW))
  (labels ((iter (loc route time)	     
	     (if (null route) (values T time) ;also returns time of finishing all tasks
		 (let* ((to (car route))
			(arr-time (+ time (travel-time loc to))))
		   (and
		    (<= arr-time (node-end to)) ;arrive before end-time
		    (iter to
			  (cdr route)
			  (time-after-serving-node to arr-time)))))))
    (iter (car (vehicle-route v))
	  (cdr (vehicle-route v))
	  0)))

;; check for whole fleet
(defmethod in-timep ((f fleet))
  (labels ((iter (veh)
	     (if (null veh) T
		 (and
		  (in-timep (car veh))
		  (iter (cdr veh))))))
    (iter (fleet-vehicles f))))			       
		  
		 
;; check if node fits in route
;; given node-id, vehicle-route and insertion index
;; 1. check if on time. If so, check if routes afterward still on time.

(defmethod feasible-insertionp ((m insertion-move) (sol VRPTW))
  "Tests if insertion move is feasible in route at index. Makes sure the remaining nodes on the route are still on time."
  (let* ((v (vehicle sol (move-vehicle-ID m)))
	 (route (vehicle-route v))
	 (node-id (move-node-ID m))
	 (index (move-index m)))
    (labels ((iter (loc route time i)
	       (if (and (null route) (< i 1)) T
		   (let ((to (car route)))
		     (when (= i 1) (setf to (node sol node-id))) ; this is the place to insert
		     (let ((arr-time (+ time (travel-time loc to))))
		       (and (<= arr-time (node-end to))
			    (iter to
				  (if (= 1 i) route (cdr route)) ;don't skip after detour
				  (time-after-serving-node to arr-time)
				  (1- i))))))))
      (iter (car route) (cdr route) 0 index))))
		    