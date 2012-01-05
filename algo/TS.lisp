;;; Thu Dec 15, 2011 (c) Marc Kuo
;;; ----------------
;;; Tabu Search implementation (uses iterate (util-algo.lisp))
;;; 0. initialize algo object
;;; 1. generate moves
;;; 2. assess moves
;;; 3. select move
;;; 4. perform move

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
	 (num-vehicles (1- (length (fleet-vehicles (problem-fleet prob)))))
	 (move-type (tabu-search-moves ts)))
    (flatten
     (map1-n #'(lambda (node-id)
		 (map0-n #'(lambda (veh-ID)
			     (make-instance move-type :node-ID node-id :vehicle-ID veh-ID))
			 num-vehicles))
	     num-nodes))))

(defmethod perform-move ((prob problem) (mv TS-best-insertion-move))
  "Takes <Node> with node-ID and uses get-best-insertion to insert in vehicle-ID. DESTRUCTIVE."
  (let ((node-ID (move-node-id mv)))
    (remove-node-ID prob node-ID)
    (let ((best-move (get-best-insertion-move prob
					      (move-vehicle-ID mv)
					      node-ID)))
      (perform-move prob best-move)))
  prob)

;; WIP - currently just selects the best move; no tabu-list implementation yet!
(defun select-move (moves)
  "This function selects a move from a sorted list of moves, while considering the tabu-list."
  (car moves))

;; --------------------
;; perhaps all the logging in an output file as well
;; perhaps all the logging/animation in :before/:after methods?
(defmethod iterate ((ts tabu-search))
  (let* ((sol (algo-current-sol ts))
	 (moves (assess-moves sol (generate-moves ts)))
 	 (sorted (sort-moves moves))
	 (selected-move (select-move sorted)))
    ;; for testing
    (when (tabu-search-animate ts)
      (plot-solution sol (with-output-to-string (s)
			   (princ "run-frames/Iteration " s)
			   (princ (algo-iterations ts) s)
			   (princ ".png" s))))
    (princ "Performing ")
    (princ (tabu-search-moves ts))
    (princ " with Node ")
    (princ (move-node-ID selected-move))
    (princ " and Vehicle ")
    (princ (move-vehicle-ID selected-move))
    ;; ------------
    (perform-move sol selected-move)))



