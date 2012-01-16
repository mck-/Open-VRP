;;; Thu 29 Dec, 2011 (c) Marc Kuo
;;; Tools to be shared among algorithms
;;; ---------------------------
;;; - get-closest-vehicle
;;; - get-closest-feasible-vehicle
;;; - optimal-insertion

(in-package :open-vrp.algo)

;; Closest Vehicle
;; ---------------------------
;; Thu Dec 8, 2011
;; challenge: what if the vehicle is located on the node n - use only for initial insertion?
(defgeneric get-closest-vehicle (node problem)
  (:method (node problem) "get-closest-vehicle: Expects <node> and <problem> inputs!")
  (:documentation "Returns the closest <vehicle> to <node>. Used by insertion heuristic. When multiple <vehicle> are on equal distance, choose first one (i.e. lowest ID)."))
 
(defmethod get-closest-vehicle ((n node) (prob TSP))
  (let ((distances (mapcar #'(lambda (x) (node-distance (last-node x) n))
			   (fleet-vehicles (problem-fleet prob))))) ;all distances
    (vehicle prob (get-min-index distances))))


;; allow only feasible vehicles to be selected
(defmethod get-closest-feasible-vehicle ((n node) (prob TSP))
  (get-closest-vehicle n prob))

(defmethod get-closest-feasible-vehicle ((n node) (prob CVRP))
  (let* ((vehicles (fleet-vehicles (problem-fleet prob)))
	 (dists (mapcar #'(lambda (x) (node-distance (last-node x) n)) vehicles))
	 (caps (mapcar #'(lambda (x)
			   (multiple-value-bind (c cap)
			       (in-capacityp x) (when c cap)))
		       vehicles))
	 (filtered (mapcar #'(lambda (dist cap)
			       (if (> (node-demand n) cap) nil dist))
			   dists caps)))
    (vehicle prob (get-min-index filtered))))	   

;; for VRPTW, consider both capacity and time. Feasiblility of appending at the end only.
(defmethod get-closest-feasible-vehicle ((n node) (prob VRPTW))
  (let* ((vehicles (fleet-vehicles (problem-fleet prob)))
	 (dists (mapcar #'(lambda (x) (node-distance (last-node x) n)) vehicles))
	 (times (mapcar #'(lambda (x)
			    (multiple-value-bind (c time)
				(in-timep x (problem-network prob)) (when c time)))
			vehicles))
	 (caps (mapcar #'(lambda (x)
			   (multiple-value-bind (c cap)
			       (in-capacityp x) (when c cap)))
		       vehicles))
	 (filtered (mapcar #'(lambda (veh dist time cap)
			       (if (or (> (node-demand n) cap)
				       (> (+ time (travel-time veh
							       (problem-network prob)
							       (node-id (last-node veh))
							       (node-id n)))
					  (node-end n)))
				   nil
				   dist))
			   vehicles dists times caps)))
    (vehicle prob (get-min-index filtered))))	   

;; ----------------------

;; Optimal insertion
;; ---------------------

(defgeneric optimal-insertion (prob node)
  (:method (prob node) "optimal-insertionL: Expects <Problem> and <Node>.")
  (:documentation "Given a node and a solution (that does not have this node yet), insert the node in the best possible and feasible location."))

(defmethod optimal-insertion ((sol tsp) (n node))
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
    (iter (fleet-vehicles (problem-fleet sol)) nil)))