;; Best Insertion (used by Tabu Search)
;; The following functions are defined to generate-assess-choose the best insertion of a node into the current route of a vehicle.
;; -----------------------------------------------
(in-package :open-vrp.algo)

(defclass insertion-move (move) 
  ((node-ID :accessor move-node-ID :initarg :node-ID)
   (vehicle-ID :accessor move-vehicle-ID :initarg :vehicle-ID)
   (index :accessor move-index :initarg :index)))

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
  (with-slots (node-ID vehicle-ID index) m
    (let* ((route (vehicle-route (vehicle sol vehicle-ID)))
	   (dist-array (problem-dist-array sol))
	   (node-before (node-id (nth (1- index) route))))
      (setf (move-fitness m)
	    (if (= index (length route)) ;if appending to end of route
		(distance node-ID (node-id (last-node route)) dist-array)
		(let ((node-after (node-id (nth index route))))
		  (-
		   (+ (distance node-ID node-before dist-array)
		      (distance node-ID node-after dist-array)) 
		   (handler-case (distance node-before node-after dist-array)
		     (same-origin-destination () 0)))))))))

;; around method for checking constraints. If move is infeasible, return NIL.
(defmethod assess-move :around ((sol CVRP) (m insertion-move))
  (with-slots (node-ID vehicle-ID fitness) m
    (if (node-fit-in-vehiclep sol node-ID vehicle-ID)
	(call-next-method)
	(setf fitness nil))))

(defmethod assess-move :around ((sol VRPTW) (m insertion-move))
  (if (feasible-insertionp m sol)
      (call-next-method)
      (setf (move-fitness m) nil)))
					     
(defmethod perform-move ((sol problem) (m insertion-move))
  "Performs the <move> on <problem>."
  (with-slots (node-ID vehicle-ID index) m
    (insert-node (vehicle sol vehicle-ID)
		 (node sol node-ID)
		 index)
    sol))

;; logging
(defmethod perform-move :after ((prob problem) (mv insertion-move))
  (format t "~&Performing ~A with Node ~A and Vehicle ~A and Index ~A" (type-of mv) (move-node-ID mv) (move-vehicle-ID mv) (move-index mv)))

(defun get-best-insertion-move (sol vehicle-id node-id)
  "Given the <solution> object, vehicle-id and node-id (integers), return the best <insertion-move> (i.e. with the lowest fitness)."
  (let* ((moves (assess-moves sol (generate-insertion-moves sol vehicle-id node-id)))
	 (sorted (sort-moves moves)))
    (car sorted)))

;; -------------------------------------------------

;; Move feasibility check
;; ------------------------

;; check if move is feasible
;; check if on time. If so, check if routes afterward still on time.
(defmethod feasible-insertionp ((m insertion-move) (sol VRPTW))
  (with-slots (node-ID vehicle-ID index) m
    (symbol-macrolet ((full-route (vehicle-route (vehicle sol vehicle-ID)))
		      (ins-node (node sol node-ID))
		      (to (if (= 1 i) ins-node (car route)))
		      (arr-time (+ time (travel-time loc to))))
      (constraints-check
       (route time loc i)
       ((cdr full-route) 0 (car full-route) index)
       ((if (= 1 i) route (cdr route)) ;don't skip after inserting new node
	(time-after-serving-node to arr-time) ;set time after new node
	to (1- i))   
       (<= arr-time (node-end to))
       (and (null route) (< i 1)))))) ; case of append, need to check once more

;; for debugging
;       (format t "Route: ~A~% Loc: ~A~% To: ~A~% Time: ~A~% Arr-time: ~A~% Node-start: ~A~% Node-end: ~A~% Duration: ~A~% ins-node-end: ~A~% i: ~A~%" (mapcar #'node-id route) (node-id loc) (node-id to) time arr-time (node-start to) (node-end to) (node-duration to) (node-end ins-node) i)
;; -----------------------------

