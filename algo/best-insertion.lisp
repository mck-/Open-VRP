;; Best Insertion (used by Tabu Search)
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
					     
(defmethod perform-move ((sol problem) (m insertion-move))
  "Performs the <move> on <problem>."
  (with-slots (node-ID vehicle-ID index) m
    (insert-node (vehicle sol vehicle-ID) (node sol node-ID) index)
    sol))

;; logging
(defmethod perform-move :after ((prob problem) (mv insertion-move))
  (format t "~&Performing ~A with Node ~A and Vehicle ~A and Index ~A" (type-of mv) (move-node-ID mv) (move-vehicle-ID mv) (move-index mv)))

(defun get-best-insertion-move (sol vehicle-id node-id)
  "Given the <solution> object, vehicle-id and node-id (integers), return the best <insertion-move> (i.e. with the lowest fitness). When no move is feasible, throws error."
  (let* ((moves (assess-moves sol (generate-insertion-moves sol vehicle-id node-id)))
	 (sorted (sort-moves moves)))
    (unless (move-fitness (car sorted)) (error 'no-feasible-move :moves sorted))
    (car sorted)))

;; -------------------------------------------------
