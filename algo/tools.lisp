;;; Thu 29 Dec, 2011 (c) Marc Kuo
;;; Tools to be shared among algorithms
;;; ---------------------------
(in-package :open-vrp.algo)

;; OBSOLETE - USE closest-node in util/route.lisp
;; ;; Thu Dec 8, 2011 - perhaps change to get-closest-node, and include &optional for exclude?     
;; (defun get-closest-unchosen-node (from-node network chosen-nodes)
;;   "Returns the closest node from current location, not in the chosen-nodes list. Input: <Node> object, <Network> object and a list of node IDs to exclude. Uses get-closest-unchosen, which works only on number lists. This is a wrapper function for CLOS. Used by greedy algo. Returns NIL if no more nodes are available."
;;   (aif (get-min-index-with-tabu (nth (node-id from-node) ;Node ID to retrieve from.. 
;; 				     (dist-table network)) ;..dist-table the right row.
;; 				chosen-nodes)
;;        (node network it)
;;        nil)) ;If no more nodes available, return nil.

;; Thu Dec 8, 2011
;; challenge: what if the vehicle is located on the node n - use only for initial insertion?
(defgeneric get-closest-vehicle (node problem)
  (:method (node problem) "Expects <node> and <problem> inputs!")
  (:documentation "Returns the closest <vehicle> to <node>. Used by insertion heuristic. When multiple <vehicle> are on equal distance, choose first one (i.e. lowest ID)."))
 
(defmethod get-closest-vehicle ((n node) (prob problem))
  (let* ((locations (mapcar #'last-node (fleet-vehicles (problem-fleet prob)))) ;list of <Vehicle> locations
	 (distances (mapcar #'(lambda (x) (node-distance x n)) locations)) ;all distances
	 (id (get-min-index distances)))
    (vehicle prob id)))

;; ;;; Mon Dec 12, 2011 - TODO
;; ;;; Attempt for Iterator on <Algo> object. Used by TS (or even GA).
;; ;;; --------------------------------------------
;; ;;; perhaps use :before for iterations check?

;; (defgeneric iterate (algo)
;;   (:method (algo)
;;     "This algo is not defined.")
;;   (:documentation "Runs the algo one iteration. Uses the algo's slot current-sol as current solution on which the algo runs one iteration. When algo's slot iterations is 0, then just return the best solution found by this algo object."))

;; (defmethod iterate :around ((a algo))
;;   (if (< (algo-iterations a) 1)
;;       (algo-best-sol a) ; when done, return best solution
;;       (call-next-method))) ; otherwise iterate

;; (defgeneric generate-moves (algo)
;;   (:method (algo)
;;     "This algo is not defined; cannot generate moves.")
;;   (:documentation "Given the algo object, that contains the current solution, generate potential <moves> for next iteration as defined by the algo. e.g. insertion moves for TS and chromosome pool for GA."))

;; (defgeneric perform-move (sol move)
;;   (:method (sol move)
;;     "This move is not defined.")
;;   (:documentation "Performs the move defined in <move> on the solution. Returns the new solution (which is a class of TSP)"))

;; (defgeneric assess-move (sol move)
;;   (:method (sol move)
;;     "This move is not defined.")
;;   (:documentation "The <Move> is assessed by calculating the fitness of solution before, and after. The score is the difference and is stored in the score slot of the <Move> object."))
