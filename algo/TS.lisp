;;; Thu Dec 15, 2011 (c) Marc Kuo
;;; ----------------
;;; Tabu Search implementation (uses iterate (util-algo.lisp))
;;; 1. generate moves
;;; 2. assess moves
;;; 3. select move
;;; 4. perform move

(in-package :vrp.util.algo)

;; needs a lot of work!!
(defmethod iterate ((ts tabu-search))
  (let* ((moves (generate-moves ts))
	 (assessments (mapcar #'assess-best-insertion moves))
	 (selected-move (select-move assessments)))
    (perform-move selected-move ts)))


;; Original attempt was to make generate-moves a general method - using the move-type slot of ts - which can be used to generate all sorts of moves e.g. swap moves.. but the method below enumerates only along node-id (excluding 0) and vehicle-id. This may only be useful for best-insertion-move?? For other moves, we need to define other defmethods?

(defmethod generate-moves ((ts tabu-search))
  "Generates a list of <move> instances (depending on what was defined in the ts slot) for all nodes and vehicles."
  (let* ((prob (algo-current-sol ts))
	 (num-nodes (1- (length (coords prob)))) ;1- to account for base
	 (num-vehicles (length (fleet-vehicles (problem-fleet prob))))
	 (move-type (tabu-search-moves ts)))
    (flatten
     (map1-n #'(lambda (node-id)
		 (map0-n #'(lambda (veh-ID)
			     (make-instance move-type :node-ID node-id :vehicle-ID veh-ID))
			 num-vehicles))
	     num-nodes))))

;; (defmethod perform-move ((prob tsp) (m best-insertion-move))
  
  
  

;; (defmethod assess-move ((prob tsp) (m move))
;;   (let ((before (fitness prob))
;; 	(after (fitness (perform-move prob m))))
;;     (setf (move-score m) (- after before))))

;; (defun assess-moves (list-of-moves)
;;   "Given a list of moves, assess each one of them and 