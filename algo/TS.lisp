;;; Thu Dec 15, 2011 (c) Marc Kuo
;;; ----------------
;;; Tabu Search implementation (uses algo/tools.lisp)
;;; 0. initialize algo object
;;; 1. generate moves
;;; 2. perform move
;;; 3. select move
;;; 4. iterate

(in-package :open-vrp.algo)

(defmethod run-algo ((prob problem) (ts tabu-search))
  "Initialize (if necessary), iterate till finished."
  (when (null (algo-current-sol ts)) (initialize prob ts))
  (while (typep (iterate ts) 'problem))
  ts)

(defmethod initialize ((prob problem) (ts tabu-search))
  "Creates inital solution and sets it to :algo-current-sol. Returns the <tabu-search> object."
  (let ((init-sol (algo-current-sol
		   (solve-prob prob (make-instance (tabu-search-init-heur ts))))))
    (setf (algo-current-sol ts) init-sol
	  (algo-best-sol ts) (copy-object init-sol)
	  (algo-best-fitness ts) (fitness init-sol)))	
  ts)

;; Original attempt was to make generate-moves a general method - using the move-type slot of ts - which can be used to generate all sorts of moves e.g. swap moves.. but the method below enumerates only along node-id (excluding 0) and vehicle-id. This may only be useful for TS-best-insertion-move?? For other moves, we need to define other defmethods?

(defmethod generate-moves ((ts tabu-search))
  "Generates a list of <move> instances (depending on what was defined in the ts slot) for all nodes and vehicles."
  (let* ((prob (algo-current-sol ts))
	 (num-nodes (1- (length (network-nodes (problem-network prob))))) ;1- for base
	 ;ignore empty vehicles, except for one (if available! capped at fleet-size)
	 (num-vehicles (min
			(1+ (vehicle-id (car (last (get-busy-vehicles (problem-fleet prob))))))
			(1- (length (fleet-vehicles (problem-fleet prob))))))
	 (move-type (tabu-search-moves ts)))
    ;remove unnecessary moves that don't do anything, e.g. when vehicle 2's route is (0 1), then the move of best-inserting node 1 into vehicle 2 has no meaning (but causes trouble!) 
    (remove-if #'(lambda (mv)
		   (let ((route (vehicle-route (vehicle prob (move-vehicle-ID mv)))))
			  ;route has one destination only
		     (and (one-destinationp route)			  
			  (= (node-id (cadr route)) (move-node-ID mv))))) ;and same node
	       (flatten
		(map1-n #'(lambda (node-id)
			    (map0-n #'(lambda (veh-ID)
					(make-instance move-type
						       :node-ID node-id
						       :vehicle-ID veh-ID))
				    num-vehicles))
			num-nodes)))))

;; the difference between cost (inserting) and saving (removing)
;; cost of inserting is calculated by (get-best-insertion-move)
;; saving by removing the connecting arcs before and after, and connecting them directly
(defmethod assess-move ((sol problem) (mv TS-best-insertion-move))
  (let* ((dist-array (network-dist-table (problem-network sol)))
	 (node (move-node-id mv))
	 (best-move (get-best-insertion-move sol (move-vehicle-ID mv) node))
	 (route (vehicle-route (vehicle sol (vehicle-with-node sol node))))
	 (pos (position node route :key #'node-id))
	 (node-before (node-id (nth (1- pos) route)))
	 (dist1 (distance node-before node dist-array)))
    (setf (move-fitness mv)
	  (- (move-fitness best-move)
					;how much you save by removing:
	     (if (= pos (1- (length route))) ;if the node is at end of route
		 dist1
		 (let ((node-after (node-id (nth (1+ pos) route))))
		   (- (+ dist1
			 (distance node node-after dist-array)) ;dist to next node
		      (distance node-before node-after dist-array)))))))) ;minus direct route

;; around method for checking constraints. If move is infeasible, return NIL.
(defmethod assess-move :around ((sol CVRP) (m TS-best-insertion-move))
  (if (node-fit-in-vehiclep sol (move-node-id m) (move-vehicle-ID m))
      (call-next-method)
      (setf (move-fitness m) nil)))

(defmethod assess-move :around ((sol VRPTW) (m TS-best-insertion-move))
  (if (feasible-insertionp (get-best-insertion-move sol (move-vehicle-ID m) (move-node-id m))
			   sol)
      (call-next-method)
      (setf (move-fitness m) nil)))
	  
(defmethod perform-move ((prob problem) (mv TS-best-insertion-move))
  "Takes <Node> with node-ID and uses get-best-insertion to insert in vehicle-ID. DESTRUCTIVE."
  (let* ((node-ID (move-node-id mv))
	 (veh-ID (move-vehicle-ID mv))
	 (best-move (get-best-insertion-move prob
					     veh-ID
					     node-ID)))
    ;if the move of node is intra-route, AND the node is being moved forward
    (if (and (= (vehicle-with-node prob node-ID) veh-ID)
	     (> (move-index best-move)
		(position node-id (vehicle-route (vehicle prob veh-ID)) :key #'node-id)))
	;then perform insertion first, afterward remove the old node, positioned before the new)
	(progn (perform-move prob best-move) (remove-node-ID prob node-ID))
	;in all other cases, it's okay to remove the node first, then reinsert
	(progn (remove-node-ID prob node-ID) (perform-move prob best-move))))
  prob)

(defmethod select-move ((ts tabu-search) moves)
  "This function selects a move from a sorted list of moves, while considering the tabu-list. If by performing the move we get a new best solution, circumvent the tabu-list."
  (if (<
       (+ (fitness (algo-current-sol ts)) (move-fitness (car moves)))
       (algo-best-fitness ts))
      (car moves)
      (labels ((iter (moves)
		 (cond ((null moves) (error "No more moves! All are tabu! Reduce tenure!"))
		       ((is-tabup ts (car moves)) (iter (cdr moves)))
		       (t (car moves)))))
	(iter moves))))

;; --------------------
;; If there is no candidate-list
;;   generate-assess-sort moves
;;   create a candidate list and perform top move
;; Perform top move from candidate-list

(defmethod iterate ((ts tabu-search))
  (let ((sol (algo-current-sol ts)))
    (labels ((perform-add-tabu (move)
	       "add move to tabu-list and perform it"
	       (add-to-tabu ts move)
	       (perform-move sol move))
	     (select-perform-from-cand (ts)
	       "select best move from candidate-list, remove all related moves and perform"
	       (let ((best-move (car (tabu-search-candidate-list ts))))
		 (remove-affected-moves ts best-move) 
		 (perform-add-tabu best-move))))
      (aif (tabu-search-candidate-list ts) 
	   (select-perform-from-cand ts)
	   (let ((sorted-moves (sort-moves (assess-moves sol (generate-moves ts)))))
	     (setf (tabu-search-candidate-list ts) (create-candidate-list ts sorted-moves))
	     (select-perform-from-cand ts))))))

;; Tabu Search animate
;; -------------------------
(defmethod iterate :after ((ts tabu-search))
  (when (tabu-search-animate ts)
    (plot-solution (algo-current-sol ts) (with-output-to-string (s)
					   (princ "run-frames/Iteration " s)
					   (princ (algo-iterations ts) s)
					   (princ ".png" s)))))
;; --------------------------