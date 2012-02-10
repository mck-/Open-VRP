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
  "Initialize (if necessary), iterate till finished. Prints run stats and the best solution. Returns (best) <Algo> object."
  (when (null (algo-current-sol ts)) (initialize prob ts))
  (while (< 0 (algo-iterations ts))
    (iterate ts))
  ts)

(defmethod initialize ((prob problem) (ts tabu-search))
  "Creates inital solution and sets it to :algo-current-sol. Returns the <tabu-search> object."
  (let ((init-sol (algo-current-sol
		   (solve-prob prob (make-instance (tabu-search-init-heur ts))))))
    (init-algo init-sol ts))
  ts)

;; Original attempt was to make generate-moves a general method - using the move-type slot of ts - which can be used to generate all sorts of moves e.g. swap moves.. but the method below enumerates only along node-id (excluding 0) and vehicle-id. This may only be useful for TS-best-insertion-move?? For other moves, we need to define other defmethods?

(defmethod generate-moves ((ts tabu-search))
  "Generates a list of <move> instances (depending on what was defined in the ts slot) for all nodes and vehicles."
  (let* ((prob (algo-current-sol ts))
	 (num-nodes (1- (length (problem-network prob)))) ;1- for base
	 ;ignore empty vehicles, except for one (if available! capped at fleet-size)
	 (num-vehicles (min
			(1+ (vehicle-id (car (last (get-busy-vehicles prob)))))
			(1- (length (problem-fleet prob)))))
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
  (with-slots (node-id vehicle-id fitness) mv
    (handler-case
	(let* ((dist-array (problem-dist-array sol))
	       (best-move (get-best-insertion-move sol vehicle-id node-id))
	       (route (vehicle-route (vehicle sol (vehicle-with-node-ID sol node-id))))
	       (pos (position node-id route :key #'node-id))
	       (node-before (node-id (nth (1- pos) route)))
	       (dist1 (distance node-before node-id dist-array)))
	  (setf fitness
		(- (move-fitness best-move)
		   ;how much you save by removing:
		   (if (= pos (1- (length route))) ;if the node is at end of route
		       dist1
		       (let ((node-after (node-id (nth (1+ pos) route))))
			 (- (+ dist1
			       (distance node-id node-after dist-array)) ;dist to next node
		;minus direct route, which is 0 if the node-before and node-after are the same.
			    (handler-case (distance node-before node-after dist-array)
			      (same-origin-destination () 0))))))))
      (no-feasible-move () (setf fitness nil))))) ;when no feasible-moves exist, set fitness nil
					
	  
(defmethod perform-move ((prob problem) (mv TS-best-insertion-move))
  "Takes <Node> with node-ID and uses get-best-insertion to insert in vehicle-ID. DESTRUCTIVE."
  (let* ((node-ID (move-node-id mv))
	 (veh-ID (move-vehicle-ID mv))
	 (best-move (get-best-insertion-move prob
					     veh-ID
					     node-ID)))
    ;if the move of node is intra-route, AND the node is being moved forward
    (if (and (= (vehicle-with-node-ID prob node-ID) veh-ID)
	     (> (move-index best-move)
		(position node-id (vehicle-route (vehicle prob veh-ID)) :key #'node-id)))
	;then perform insertion first, afterward remove the old node, positioned before the new)
	(progn (perform-move prob best-move) (remove-node-ID prob node-ID))
	;in all other cases, it's okay to remove the node first, then reinsert
	(progn (remove-node-ID prob node-ID) (perform-move prob best-move))))
  prob)

(defmethod select-move ((ts tabu-search) all-moves)
  "This function selects a move from a sorted list of moves, while considering the tabu-list. If by performing the move we get a new best solution, circumvent the tabu-list."
  (if (<
       (+ (fitness (algo-current-sol ts)) (move-fitness (car all-moves)))
       (algo-best-fitness ts))
      (car all-moves)
      (labels ((iter (moves)
		 (cond ((null moves)
			(error 'all-moves-tabu :moves all-moves :tabu-list (tabu-list ts)))
		       ((is-tabup ts (car moves)) (iter (cdr moves)))
		       (t (car moves)))))
	(iter all-moves))))

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