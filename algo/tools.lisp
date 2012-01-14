;;; Thu 29 Dec, 2011 (c) Marc Kuo
;;; Tools to be shared among algorithms
;;; ---------------------------
(in-package :open-vrp.algo)

;; Thu Dec 8, 2011
;; challenge: what if the vehicle is located on the node n - use only for initial insertion?
(defgeneric get-closest-vehicle (node problem)
  (:method (node problem) "get-closest-vehicle: Expects <node> and <problem> inputs!")
  (:documentation "Returns the closest <vehicle> to <node>. Used by insertion heuristic. When multiple <vehicle> are on equal distance, choose first one (i.e. lowest ID)."))
 
(defmethod get-closest-vehicle ((n node) (prob problem))
  (let* ((locations (mapcar #'last-node (fleet-vehicles (problem-fleet prob)))) ;list of <Vehicle> locations
	 (distances (mapcar #'(lambda (x) (node-distance x n)) locations)) ;all distances
	 (id (get-min-index distances)))
    (vehicle prob id)))

;; Mon Dec 12, 2011 - TODO
;; Attempt for Iterator on <Algo> object. Used by TS (or even GA or other metaheuristics).
;; --------------------------------------------

;; initializer
;; -----------
(defgeneric initialize (problem algo)
  (:method (problem algo)
    "initialize: Requires <Problem> and <Algo> as inputs.")
  (:documentation "Initializes the initial solution for the algo object. For Tabu Search, the default heuristic for generating an initial solution is 'greedy-insertion, which is read from the slot :init-heur."))

;; iterator
;; ------------
(defgeneric iterate (algo)
  (:method (algo)
    "iterate: This algo is not defined.")
  (:documentation "Runs the algo one iteration. Uses the algo's slot current-sol as current solution on which the algo runs one iteration. When algo's slot iterations is 0, then print the best solution found by this algo object. Returns the <algo> object when finished, otherwise returns <problem> solution object."))

(defmethod iterate :around ((a algo))
  (if (< (algo-iterations a) 1)
      (progn
	(print-routes (algo-best-sol a))
	a)
      (call-next-method))) ; otherwise iterate

(defmethod iterate :after ((a algo))
  (setf (algo-iterations a) (1- (algo-iterations a)))
  (let* ((sol (algo-current-sol a))
	 (new-fitness (fitness sol))
	 (best-fitness (algo-best-fitness a)))
    (print-routes sol)
    (when (or (null best-fitness)
	      (< new-fitness best-fitness))
      (setf (algo-best-fitness a) new-fitness)
      (setf (algo-best-sol a) (copy-object sol)))))

;; Resume run - add some more iterations
;; ------------------------
(defgeneric iterate-more (algo int)
  (:method (algo int) "iterate-more: expects <Algo> and int as inputs")
  (:documentation "When an algo finished (i.e. iterations = 0) using iterate-more allows you to keep running it x more iterations."))

(defmethod iterate-more ((a algo) int)
  (setf (algo-iterations a) int)
  (run-algo (algo-current-sol a) a))

;; Tabu Search animate
;; -------------------------
(defmethod iterate :after ((ts tabu-search))
  (when (tabu-search-animate ts)
    (plot-solution (algo-current-sol ts) (with-output-to-string (s)
					   (princ "run-frames/Iteration " s)
					   (princ (algo-iterations ts) s)
					   (princ ".png" s)))))
;; --------------------------


;; Generate-moves
;; -------------------
(defgeneric generate-moves (algo)
  (:method (algo)
    "generate-moves: This algo is not defined; cannot generate moves.")
  (:documentation "Given the algo object, that contains the current solution, generate potential <moves> for next iteration as defined by the algo. e.g. insertion moves for TS and chromosome pool for GA."))

;; Perform move
;; ---------------------
(defgeneric perform-move (sol move)
  (:method (sol move)
    "perform-move: This move is not defined.")
  (:documentation "Performs the move defined in <move> on the solution. Returns the new solution (which is a class of <Problem>)"))

;; logging
(defmethod perform-move :after ((prob problem) (mv move))
  (print "Performing ")
  (princ (type-of mv))
  (princ " with Node ")
  (princ (move-node-ID mv))
  (princ " and Vehicle ")
  (princ (move-vehicle-ID mv))
  (when (eq (type-of mv) 'insertion-move)
    (princ " and Index ")
    (princ (move-index mv))))

;; Assess move(s)
;; ------------------------
(defgeneric assess-move (sol move)
  (:method (sol move)
    "assess-move: This move is not defined.")
  (:documentation "The <Move> is assessed by calculating the fitness of solution before, and after. The fitness is the difference and is stored in the :fitness slot of the <Move> object."))

(defun fitness-before-after (sol operation)
  "Given <Problem> object and an #'operation function that takes the <problem> as input, return the difference of fitness between after and before."
  (let* ((before (fitness sol))
	 (clone (copy-object sol)))
    (funcall operation clone)
    (- (fitness clone) before)))
    
(defmethod assess-move ((sol problem) (m move))
  "Assesses the effect on fitness when <move> is performed on <problem> (on a clone - undestructive)."
  (setf (move-fitness m)
	(fitness-before-after sol #'(lambda (x) (perform-move x m)))))

(defun assess-moves (solution moves)
  "Given a list of <Move> objects, assess them all on the solution (uses assess-move), and setf the move's :fitness slots. Returns the list of moves."
  (mapcar #'(lambda (move) (assess-move solution move)) moves)
  moves)

;; Select move
;; -------------------
(defgeneric select-move (algo moves)
  (:method (algo moves) "select-move: not defined for <Algo>")
  (:documentation "Given an <Algo> object and the list of <Moves>, select a move. By default, sort the moves and select the best one, but e.g. for tabu-search, check first of the <Move> is tabu."))

(defun sort-moves (moves)
  "Given a list of <Move>s, sort them according to fitness (ascending). Undestructive."
  (sort-ignore-nil moves #'< :key #'move-fitness))

(defmethod select-move ((a algo) moves)
  (car (sort-moves moves)))

;; ------------------------------------------------

;; Tue 3 Jan, 2011
;; Best Insertion (step towards Tabu Search)
;; The following functions are defined to generate-assess-choose the best insertion of a node into the current route of a vehicle.
;; -----------------------------------------------

(defun generate-insertion-moves (sol vehicle-id node-id)
  "Given the <solution> object, vehicle-id and node-id (integers), create all possible insertion-moves, and return them in a list. Avoid generating moves that won't do anything (when doing intra-route insertion)."
  (let* ((route (vehicle-route (vehicle sol vehicle-id)))
	 (pos (position node-id route :key #'node-id)) ;check if we do intra-route insertion
	 (moves '()))
    (do ((index 1 (1+ index)))
	((> index (length route)))
      (unless (and pos (or (= index pos) (= index (1+ pos))))
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
	 (dist-array (network-dist-table (problem-network sol)))
	 (index (move-index m))
	 (node (move-node-id m))
	 (node-before (node-id (nth (1- index) route))))
    (setf (move-fitness m)
	  (if (= index (length route)) ;if appending to end of route
	      (distance node (node-id (car (last route))) dist-array)
	      (let ((node-after (node-id (nth index route))))
		(-
		 (+ (distance node node-before dist-array)
		    (distance node node-after dist-array)) 
		 (distance node-before node-after dist-array)))))))

;; around method for checking constraints. If move is infeasible, return NIL.
(defmethod assess-move :around ((sol CVRP) (m insertion-move))
  (multiple-value-bind (comply cap-left) (in-capacityp (vehicle sol (move-vehicle-id m)))
    (if (not comply) (error "assess-move: The solution was infeasible to beign with!")
	(if (> (node-demand (node sol (move-node-ID m))) cap-left)
	    (setf (move-fitness m) nil)
	    (call-next-method)))))
					     
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
