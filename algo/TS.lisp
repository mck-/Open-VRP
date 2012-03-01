;;; Tabu Search implementation
;;; ----------------
;;; 0. initialize algo object
;;; 1. generate moves
;;; 2. perform move
;;; 3. select move
;;; 4. iterate

(in-package :open-vrp.algo)

(defmethod run-algo ((prob problem) (ts tabu-search))
  "Initialize (if necessary), iterate till finished. Returns <Algo> object."
  (unless (algo-current-sol ts) (initialize prob ts))
  (while (< 0 (algo-iterations ts))
    (iterate ts))
  ts)

(defmethod initialize ((prob problem) (ts tabu-search))
  "Creates inital solution and sets it to :algo-current-sol. Returns the <tabu-search> object. For Tabu Search, the default heuristic for generating an initial solution is 'greedy-best-insertion, which is read from the slot :init-heur."
  (init-algo (algo-current-sol
	      (run-algo (copy-object prob) (make-instance (ts-init-heur ts))))
	     ts)
  ts)

;; Original attempt was to make generate-moves a general method - using the move-type slot of ts - which can be used to generate all sorts of moves e.g. swap moves.. but the method below enumerates only along node-id (excluding 0) and vehicle-id. This may only be useful for TS-best-insertion-move?? For other moves, we need to define other defmethods?

(defmacro for-node-ID ((node-ID prob) &body body)
  "Map over all node-IDs, except for base."
  `(map1-n #'(lambda (,node-ID)
	       ,@body)
	   (1- (num-nodes ,prob))))

(defmacro for-veh-ID ((veh-ID prob) &body body)
  "Map over all veh-IDs capped at fleet-size. Will consider only busy vehicles and one extra idle vehicle."
  `(map0-n #'(lambda (,veh-ID)
	       ,@body)
	   (min (1+ (vehicle-id (car (last (get-busy-vehicles ,prob)))))
		(1- (num-veh ,prob)))))

(defun useless-move (mv prob)
  "Returns T if move is useless. Two options: 1. move concerns node and vehicle that has the node as it's only destination, e.g. (0 2 0). 2. Moving node from one-destination vehicle to empty-vehicle, which becomes another one-destination vehicle."
  (let ((route (route-to mv prob)))
    (or (and (one-destinationp route)			  
	     (= (node-id (cadr route)) (move-node-ID mv)))
	(and (empty-routep route) (one-destinationp (route-from mv prob))))))

(defmethod generate-moves ((ts tabu-search))
  "Generates a list of <move> instances (depending on what was defined in the ts slot) for all nodes and vehicles."
  (let ((prob (algo-current-sol ts)))
    (remove-if #'(lambda (mv) (useless-move mv prob))
	       (flatten
		(for-node-ID (node-ID prob)
		  (for-veh-ID (veh-ID prob)
		    (make-instance (ts-move-type ts)
				   :node-ID node-ID
				   :vehicle-ID veh-ID)))))))		  

;; the difference between cost (inserting) and saving (removing)
;; cost of inserting is calculated by (get-best-insertion-move)
;; saving by removing the connecting arcs before and after, and connecting them directly
(defmethod assess-move ((sol problem) (mv TS-best-insertion-move))
  (with-slots (node-id vehicle-id fitness) mv
    (handler-case
	(let* ((dist-array (problem-dist-array sol))
	       (route (route-from mv sol))
	       (pos (position node-id route :key #'node-id))
	       (node-before (node-id (nth (1- pos) route)))
	       (dist-before (distance node-before node-id dist-array)))
	  (setf fitness
		   ;cost of insertion
		(- (move-fitness (get-best-insertion-move-in-vehicle sol vehicle-id node-id))
		   ;save by removing:
		   (if (= pos (1- (length route))) ;if the node is at end of route
		       dist-before
		       (let ((node-after (node-id (nth (1+ pos) route))))
			 (- (+ dist-before
			       (distance node-id node-after dist-array)) ;dist to next node
		;minus direct route, which is 0 if the node-before and node-after are the same.
			    (handler-case (distance node-before node-after dist-array)
			      (same-origin-destination () 0))))))))
      (no-feasible-move () (setf fitness nil))))) ;when no feasible-moves exist, set fitness nil
					
	  
(defmethod perform-move ((sol problem) (mv TS-best-insertion-move))
  "Takes <Node> with node-ID and uses get-best-insertion to insert in vehicle-ID. DESTRUCTIVE."
  (with-slots (node-ID vehicle-ID) mv
    (let ((best-move (get-best-insertion-move-in-vehicle sol vehicle-ID node-ID)))
    ;if the move of node is intra-route, AND the node is being moved forward
      (if (and (= (vehicle-with-node-ID sol node-ID) vehicle-ID)
	       (> (move-index best-move)
		  (position node-id (route-to mv sol) :key #'node-id)))
	;then perform insertion first, afterward remove the old node, positioned before the new
	(progn (perform-move sol best-move) (remove-node-ID sol node-ID))
	;in all other cases, it's okay to remove the node first, then reinsert
	(progn (remove-node-ID sol node-ID) (perform-move sol best-move))))
    sol))

(defmethod select-move ((ts tabu-search) all-moves)
  "This function selects best non-tabu move from a list of assessed moves. When aspiration criteria is set to T, then if by performing the move we get a new best solution, circumvent the tabu-list."
  (let ((sorted-moves (sort-moves all-moves)))
    (if (and (ts-aspirationp ts)
	     (< (+ (fitness (algo-current-sol ts)) (move-fitness (car sorted-moves)))
		(algo-best-fitness ts)))
	(car sorted-moves)
	(restart-case
	    (aif (find-if-not #'(lambda (mv) (is-tabu-movep ts mv)) sorted-moves) it
		 (error 'all-moves-tabu :moves all-moves :tabu-list (ts-tabu-list ts)))
	  (select-best-tabu-move ()
	    :report "Choost the best move, you'll need to move somehow, right?"
	    (car sorted-moves))
	  (flush-tabu-list ()
	    :report "Erase everything on the tabu-list and resume."
	    (clear-tabu-list ts)
	    (car sorted-moves))))))

;; --------------------
;; If there is no candidate-list
;;   generate-assess-sort moves
;;   create a candidate list and perform top move
;; Perform top move from candidate-list

(defmethod iterate ((ts tabu-search))
  (let ((sol (algo-current-sol ts)))
    (labels ((perform-add-tabu (move)
	       "add move to tabu-list if unimproving move and perform it"
	       (when (<= 0 (move-fitness move)) (add-move-to-tabu ts move))
	       (perform-move sol move))
	     (select-perform-from-cand (ts)
	       "select best move from candidate-list, remove all related moves and perform"
	       (let ((best-move (car (ts-candidate-list ts))))
		 (remove-affected-moves ts best-move) 
		 (perform-add-tabu best-move))))
      (if (ts-elite-listp ts)
	  (if (ts-candidate-list ts) 
	      (select-perform-from-cand ts)
	      (let ((sorted-moves (sort-moves (assess-moves sol (generate-moves ts)))))
		(setf (ts-candidate-list ts) (create-candidate-list ts sorted-moves))
		(select-perform-from-cand ts)))
	  (perform-add-tabu (select-move ts (assess-moves sol (generate-moves ts))))))
    ts))
					  
;; --------------------------

;; Stopping condition
;; --------------------------
;; When there is a stopping-condition, check it on the algo. If met, set iterations to 0.

(defmethod iterate :around ((ts tabu-search))
  (let ((sc (ts-stopping-condition ts)))
    (when (and sc (funcall sc ts))
      (setf (algo-iterations ts) 0)
      (with-log-or-print (stream (algo-current-sol ts))
	(format stream "~&Stopping condition met.~%"))
      (unless (= (log-mode ts) 2)
	(format t "~&Stopping condition met.~%"))))
    (call-next-method))
	
  