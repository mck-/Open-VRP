;;; Tools to be shared among algorithms
;;; ---------------------------
;;; - get-closest-node
;;; - get-closest-vehicle
;;; - get-closest-feasible-vehicle
;;; - optimal-insertion
;;; - init-algo

(in-package :open-vrp.algo)

;; Closest node
;; ---------------------------

(defun get-min-index-with-tabu (distances tabu)
  "Returns index of the first next closest, that is not in chosen (which is a list)."
  (with-tabu-indices tabu #'get-min-index distances))

(defun get-closest-node (prob veh-id &optional tabu)
  "Returns the closest node from the last location of vehicle. Requires <problem> and vehicle-ID. A tabu list of node-IDs is optional to exclude consideration of some nodes."
  (let* ((loc (last-node (vehicle prob veh-id)))
	 (dists (get-array-row (problem-dist-array prob) (node-id loc))))
    (aif (get-min-index-with-tabu dists tabu)
	 (node prob it)
	 nil)))
;; --------------------------

;; Closest Vehicle
;; ---------------------------
(defun dists-to-vehicles (node prob)
  "Given a <Node> and a <Problem>, return the list of all the distances from the <Node> to the current positions of the fleet. Used by get-closest-(feasible)-vehicle."
  (mapcar #'(lambda (x) (node-distance (last-node x) node)) (problem-fleet prob)))

;; challenge: what if the vehicle is located on the node n - use only for initial insertion?
(defun get-closest-vehicle (n prob)
  "Returns the closest <vehicle> to <node>. Used by insertion heuristic. When multiple <vehicle> are on equal distance, choose first one (i.e. lowest ID)."
  (vehicle prob (get-min-index (dists-to-vehicles n prob))))
;; -------------------------

;; Closest Feasible Vehicle
;; ----------------------------
(defmethod get-closest-feasible-vehicle ((n node) (prob problem))
  (get-closest-vehicle n prob))

;; Capacity check
(defun capacities-left (prob)
  "Returns a list of all capacities left on the vehicles given the present solution."
  (mapcar #'(lambda (x) (multiple-value-bind (c cap)
			    (in-capacityp x) (when c cap)))
	  (problem-fleet prob)))

(defmethod get-closest-feasible-vehicle ((n node) (prob CVRP))
  "Returns the vehicle closest to the node and has enough capacity." 
  (vehicle prob (get-min-index
		 (mapcar #'(lambda (dist cap)
			     (unless (> (node-demand n) cap) dist))
			 (dists-to-vehicles n prob)
			 (capacities-left prob)))))

;; Time-window check
(defun times-of-arriving (node prob)
  "Returns a list of arrival times of the vehicles to node given the present solution."
  (mapcar #'(lambda (x)
	      (multiple-value-bind (c time)
		  (in-timep x) (when c (+ time (travel-time (last-node x) node)))))
	  (problem-fleet prob)))

;; for VRPTW, consider both capacity and time. Feasiblility of appending at the end only.
;; use get-best-insertion instead for inserting feasibly into routes.
(defmethod get-closest-feasible-vehicle ((n node) (prob VRPTW))
  "Returns the vehicle closest to the node and has enough capacity and time."
  (vehicle prob (get-min-index 
		 (mapcar #'(lambda (dist arr-time cap)
			     (unless (or (> (node-demand n) cap)
					 (> arr-time (node-end n)))
			       dist))
			 (dists-to-vehicles n prob)
			 (times-of-arriving n prob)
			 (capacities-left prob)))))

;; ----------------------

;; Optimal insertion
;; ---------------------

(defgeneric optimal-insertion (prob node)
  (:method (prob node) "optimal-insertion: Expects <Problem> and <Node>.")
  (:documentation "Given a node and a solution (that does not have this node yet), insert the node in the best possible and feasible location. DESTRUCTIVE."))

(defmethod optimal-insertion ((sol problem) (n node))
  (labels ((iter (flt best-move)
	     (if (null flt) best-move
		 (iter (cdr flt)
		       (let ((new (get-best-insertion-move sol
							   (vehicle-id (car flt))
							   (node-id n))))
			 (if (and (move-fitness new) ;check if new move is feasible
				  (or (null best-move) ;first move
				      (< (move-fitness new) (move-fitness best-move)))) ;better?
			     new
			     best-move))))))
    (iter (problem-fleet sol) nil)))

;; -------------------------

;; Algo object initializing macro
;; -------------------------
(defmacro init-algo (prob algo)
  `(setf (algo-current-sol ,algo) ,prob
	 (algo-best-sol ,algo) (copy-object ,prob)
	 (algo-best-fitness ,algo) (fitness ,prob)))