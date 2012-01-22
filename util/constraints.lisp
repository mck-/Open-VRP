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
  (if (in-timep (problem-fleet sol) (problem-network sol))
      (call-next-method)
      NIL))
;; -------------------------

;; Capacity Constraints
;; ------------------------

(defgeneric in-capacityp (veh/fleet/problem)
  (:method (obj) "Expects a <Vehicle>/<Fleet>/<Problem> object!")
  (:documentation "Tests weather the route on <vehicle> is complying with the capacity constraint. Returns T and the remaining capacity if it does. When <Fleet> is provided, test all vehicles."))

(defmethod in-capacityp ((v vehicle))
  (labels ((iter (route cap)
	     (if (null route) (values T cap)
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
	   
(defun node-fit-in-vehiclep (sol node-id vehicle-id)
  "Helper function for assessing constraints. Used by assess-move :around."
  (multiple-value-bind (comply cap-left) (in-capacityp (vehicle sol vehicle-id))
    (if (not comply) (error "node-fit-in-vehiclep: The solution was infeasible to begin with!")
	(<= (node-demand (node sol node-id)) cap-left))))

;; ------------------------------

;; Time-window constraints
;; -------------------------

(defmethod travel-time ((v vehicle-TW) (n network) node-id1 node-id2)
  (if (= node-id1 node-id2) 0 ; travel-time from base to base is 0
      (let ((dist (distance node-id1 node-id2 (network-dist-table n))))
	(/ dist (vehicle-speed v)))))

(defgeneric in-timep (veh/fleet network)
  (:method (obj net) "in-timep: Expects <Vehicle-TW>/<Fleet> and a <Network> object!")
  (:documentation "Tests weather the route on <Vehicle> is complying with the time-window constraints. Returns T and the time of finishing its last task. When <Fleet> is provided, test all vehicles."))

;; check for each node
;; arrival time is current time + travel time
;; if arrival time < end-time, on time (if not on time, return NIL - no need to check more)
;;   if arrival time < begin-time, wait until begin time -> set time to begin + duration
;;   if arrival after begin-time -> set time to arrival + duration

(defmethod in-timep ((v vehicle-TW) (n network))
  (labels ((iter (loc route time)	     
	     (if (null route) (values T time) ;also returns time of finishing all tasks
		 (let* ((to (car route))
			(arr-time (+ time
				     (travel-time v n (node-id loc) (node-id to)))))
		   (and
		    (<= arr-time (node-end to)) ;arrive before end-time
		    (iter to
			  (cdr route)
			  (if (< arr-time (node-start to))
			      (+ (node-start to) (node-duration to)) ;wait for start-time
			      (+ arr-time (node-duration to)))))))))
    (iter (car (vehicle-route v))
	  (cdr (vehicle-route v))
	  0)))

(defmethod in-timep ((f fleet) (n network))
  (labels ((iter (veh)
	     (if (null veh) T
		 (and
		  (in-timep (car veh) n)
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
		     (when (= i 1) (setf to (node sol node-id)))
		     (let ((arr-time (+ time (travel-time v
							  (problem-network sol)
							  (node-id loc)
							  (node-id to)))))
		       (and (<= arr-time (node-end to))
			    (iter to
				  (if (= 1 i) route (cdr route)) ;don't skip after detour
				  (if (< arr-time (node-start to))
				      (+ (node-start to) (node-duration to)) ;wait
				      (+ arr-time (node-duration to)))
				  (1- i))))))))
      (iter (car route) (cdr route) 0 index))))
		    