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
  "Initialize, iterate till finished."
  (initialize prob ts)
  (while (typep (iterate ts) 'problem))
  ts)

(defmethod initialize ((prob problem) (ts tabu-search))
  "Creates inital solution and sets it to :algo-current-sol. Returns the <tabu-search> object."
  (setf (algo-current-sol ts)
	(algo-current-sol 
	 (solve-prob prob (make-instance (tabu-search-init-heur ts)))))
  ts)

;; Original attempt was to make generate-moves a general method - using the move-type slot of ts - which can be used to generate all sorts of moves e.g. swap moves.. but the method below enumerates only along node-id (excluding 0) and vehicle-id. This may only be useful for TS-best-insertion-move?? For other moves, we need to define other defmethods?

(defmethod generate-moves ((ts tabu-search))
  "Generates a list of <move> instances (depending on what was defined in the ts slot) for all nodes and vehicles."
  (let* ((prob (algo-current-sol ts))
	 (num-nodes (1- (length (coords prob)))) ;1- to account for base
	 ;ignore empty vehicles, except for one (if available! capped at fleet-size)
	 (num-vehicles (min
			(length (remove-if #'(lambda (v) (single (vehicle-route v)))
					   (fleet-vehicles (problem-fleet prob))))
			(1- (length (fleet-vehicles (problem-fleet prob))))))
	 (move-type (tabu-search-moves ts)))
    ;remove unnecessary moves that don't do anything, e.g. when vehicle 2's route is (0 1), then the move of best-inserting node 1 into vehicle 2 has no meaning (but causes trouble!) 
    (remove-if #'(lambda (mv)
		   (let ((route (vehicle-route (vehicle prob (move-vehicle-ID mv))))) 
		     (and (null (cddr route)) ;route is of length 2 (has one destination only)
			  (cadr route) ;it's not an empty one either
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
	  
(defmethod perform-move ((prob problem) (mv TS-best-insertion-move))
  "Takes <Node> with node-ID and uses get-best-insertion to insert in vehicle-ID. DESTRUCTIVE."
  (let* ((node-ID (move-node-id mv))
	 (best-move (get-best-insertion-move prob
					     (move-vehicle-ID mv)
					     node-ID)))
    (remove-node-ID prob node-ID)
    (perform-move prob best-move))
  prob)

;; could not be a generic function, because the way a TS selects a move is different from
;; say GA, but the argument is a list, not an object.
(defun select-move (moves tabu-list)
  "This function selects a move from a sorted list of moves, while considering the tabu-list."
  (if (is-tabup tabu-list (car moves))
      (select-move (cdr moves) tabu-list)
      (car moves)))

;; --------------------
;; perhaps all the logging in an output file as well
;; perhaps all the logging/animation in :before/:after methods?
(defmethod iterate ((ts tabu-search))
  (let* ((sol (algo-current-sol ts))
	 (moves (assess-moves sol (generate-moves ts)))
 	 (sorted (sort-moves moves))
	 (selected-move (select-move sorted (tabu-search-tabu-list ts))))
    ;; add move to tabu-list
    (add-to-tabu ts selected-move)
    ;; logging
    (princ "Performing ")
    (princ (tabu-search-moves ts))
    (princ " with Node ")
    (princ (move-node-ID selected-move))
    (princ " and Vehicle ")
    (princ (move-vehicle-ID selected-move))
    ;; ---------------
    (perform-move sol selected-move)))



