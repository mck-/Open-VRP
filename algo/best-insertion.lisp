;; Tue 3 Jan, 2011
;; Best Insertion (step towards Tabu Search)
;; The following functions are defined to generate-assess-choose the best insertion of a node into the current route of a vehicle.
;; -----------------------------------------------
(in-package :open-vrp.algo)

(defun generate-insertion-moves (sol vehicle-id node-id)
  "Given the <solution> object, vehicle-id and node-id (integers), create all possible insertion-moves, and return them in a list. Avoid generating moves that won't do anything (when doing intra-route insertion)."
  (let* ((route (vehicle-route (vehicle sol vehicle-id)))
	 (pos (position node-id route :key #'node-id)) ;check if we do intra-route insertion
	 (moves '()))
    (do ((index 1 (1+ index)))
	((> index (if (problem-to-depot sol) (1- (length route)) (length route))))
      (unless (and pos (or (= index pos) (= index (1+ pos)))) ;useless moves avoided
	(push (make-instance 'insertion-move
			     :index index
			     :vehicle-id vehicle-id
			     :node-id node-id)
	      moves)))
    (nreverse moves)))

;; calculates the added distance of performing the insertion-move
;; when appending to the end, it's just the distance from last location to the node
;; otherwise it is the distance to the nodes before and after, minus their direct connection
(defmethod assess-move ((sol problem) (m insertion-move))
  (let* ((route (vehicle-route (vehicle sol (move-vehicle-id m))))
	 (dist-array (problem-dist-array sol))
	 (index (move-index m))
	 (node (move-node-id m))
	 (node-before (node-id (nth (1- index) route))))
    (setf (move-fitness m)
	  (if (= index (length route)) ;if appending to end of route
	      (distance node (node-id (last-node route)) dist-array)
	      (let ((node-after (node-id (nth index route))))
		(-
		 (+ (distance node node-before dist-array)
		    (distance node node-after dist-array)) 
		 (or (distance node-before node-after dist-array) 0))))))) ; NIL -> 0

;; around method for checking constraints. If move is infeasible, return NIL.
(defmethod assess-move :around ((sol CVRP) (m insertion-move))
  (if (node-fit-in-vehiclep sol (move-node-id m) (move-vehicle-id m))      
      (call-next-method)
      (setf (move-fitness m) nil)))

(defmethod assess-move :around ((sol VRPTW) (m insertion-move))
  (if (feasible-insertionp m sol)
      (call-next-method)
      (setf (move-fitness m) nil)))
					     
(defmethod perform-move ((sol problem) (m insertion-move))
  "Performs the <move> on <problem>."
  (insert-node (vehicle sol (move-vehicle-id m))
	       (node sol (move-node-id m))
	       (move-index m))
  sol)

(defun get-best-insertion-move (sol vehicle-id node-id)
  "Given the <solution> object, vehicle-id and node-id (integers), return the best <insertion-move> (i.e. with the lowest fitness)."
  (let* ((moves (assess-moves sol (generate-insertion-moves sol vehicle-id node-id)))
	 (sorted (sort-moves moves)))
    (car sorted)))

;; -------------------------------------------------
