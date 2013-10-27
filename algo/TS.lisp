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

(defmethod generate-moves ((ts tabu-search))
  "Generates a list of <move> instances (depending on what was defined in the ts slot) for all nodes and vehicles."
  (flet ((symb (a b)
           (intern (format nil "~a-~a" (symbol-name a) (symbol-name b)) :open-vrp.algo)))
    (let ((prob (algo-current-sol ts)))
      (loop for veh in (problem-fleet prob)
         for veh-id = (vehicle-id veh) append
           (loop for node-id being the hash-keys of (problem-visits prob)
                for veh-w-node-id = (vehicle-with-node-id prob node-id)
              unless (or;; Skip UNSERVED moves for later
                      (eq :UNSERVED veh-w-node-id)
                        ;; Do not generate moves that are not changing anything
                      (and (one-destination-p (vehicle-route veh))
                              (eq (cadr (route-indices veh)) node-id))
                         ;; Avoid moves that transfers a single node to an other empty vehicle
                      (let ((vehicle-to (vehicle prob veh-w-node-id)))
                        (and (no-visits-p (vehicle-route veh))
                             (one-destination-p (vehicle-route vehicle-to))
                             (eq (vehicle-start-location veh) (vehicle-start-location vehicle-to))
                             (eq (vehicle-end-location veh) (vehicle-end-location vehicle-to)))))
              collect
                (funcall
                 (symb 'make (ts-move-type ts))
                 :node-ID node-id
                 :vehicle-ID veh-id))
              into moves
              finally
                ;; Create UNSERVED related moves (if applicable)
                (if (problem-allow-unserved prob)
                    (return (append
                     (loop for node-id being the hash-keys of (problem-visits prob)
                          unless (member node-id (problem-unserved prob))
                          collect (funcall (symb 'make (ts-move-type ts)) :node-ID node-id :vehicle-ID :UNSERVED))
                     moves))
                    (return moves))))))

;; the difference between cost (inserting) and saving (removing)
;; cost of inserting is calculated by (get-best-insertion-move)
;; saving by removing the connecting arcs before and after, and connecting them directly
(defmethod assess-move ((sol problem) (mv TS-best-insertion-move))
  (with-slots (node-id vehicle-id fitness) mv
    (handler-case
        (let* ((dist-matrix (problem-dist-matrix sol))
               (veh (vehicle sol (vehicle-with-node-id sol node-id)))
               (route (route-indices veh))
               (pos (position node-id route))
               (node-before (if (= pos 0) (vehicle-start-location veh) (nth (1- pos) route)))
               (dist-before (get-distance node-before node-id dist-matrix)))
          (setf fitness
                ;cost of insertion
                (- (move-fitness (get-best-insertion-move-in-vehicle sol vehicle-id node-id))
                   ;save by removing:
                   (let ((node-after (nth (1+ pos) route)))
                     (- (+ dist-before
                           (get-distance node-id node-after dist-matrix)) ;dist to next node
                     ;minus direct route, which is 0 if the node-before and node-after are the same.
                        (handler-case (get-distance node-before node-after dist-matrix)
                          (same-origin-destination () 0)))))))
    (no-feasible-move () (setf fitness nil))))) ;when no feasible-moves exist, set fitness nil


(defmethod perform-move ((sol problem) (mv TS-best-insertion-move))
  "Takes <Node> with node-ID and uses get-best-insertion to insert in vehicle-ID. DESTRUCTIVE."
  (let* ((node-id (move-node-id mv))
         (veh-id (move-vehicle-id mv))
         (best-move (get-best-insertion-move-in-vehicle sol veh-id node-id)))
         ;if the move of node is intra-route, AND the node is being moved forward
    (if (and (eq (vehicle-with-node-ID sol node-ID) veh-ID)
             (> (move-index best-move)
                (position node-id (route-to mv sol) :key #'visit-node-id)))
        ;then perform insertion first, afterward remove the old node, positioned before the new
        (progn (perform-move sol best-move) (remove-node-id sol node-ID))
        ;in all other cases, it's okay to remove the node first, then reinsert
        (progn (remove-node-ID sol node-ID) (perform-move sol best-move))))
  sol)

(defmethod select-move ((ts tabu-search) all-moves)
  "This function selects best non-tabu move from a list of assessed moves. When aspiration criteria is set to T, then if by performing the move we get a new best solution, circumvent the tabu-list."
  (let* ((sorted-moves (sort-moves all-moves))
         (top-move (car sorted-moves)))
    (unless sorted-moves (error 'no-feasible-move :moves all-moves))
    (if (and (ts-aspiration-p ts)
             (best-solution-found-p ts top-move))
        top-move
        (restart-case
            (aif (find-if-not #'(lambda (mv) (is-tabu-move-p ts mv)) sorted-moves) it
                 (error 'all-moves-tabu :moves all-moves :tabu-list (ts-tabu-list ts)))
          (select-best-tabu-move ()
            :report "Choost the best move, you'll need to move somehow, right?"
            top-move)
          (flush-tabu-list ()
            :report "Erase everything on the tabu-list and resume."
            (clear-tabu-list ts)
            top-move)))))

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
      (if (ts-elite-list-p ts)
          (if (ts-candidate-list ts)
              (select-perform-from-cand ts)
              (let ((sorted-moves (sort-moves (assess-moves sol (generate-moves ts)))))
                (setf (ts-candidate-list ts) (create-candidate-list ts sorted-moves))
                (select-perform-from-cand ts)))
          (perform-add-tabu
           (handler-bind ((all-moves-tabu #'select-best-tabu))
             (select-move ts (assess-moves sol (generate-moves ts)))))))
    ts))

;; --------------------------

;; Stopping condition
;; --------------------------
;; When there is a stopping-condition, check it on the algo. If met, set iterations to 0.

(defmethod iterate :around ((ts tabu-search))
  (let ((sc (ts-stopping-condition ts)))
    (when (and sc (funcall sc ts))
      (setf (algo-iterations ts) 0)
      (with-log-or-print (stream (algo-current-sol ts) *start-time*)
        (format stream "~&Stopping condition met.~%"))
      (unless (log-to-repl-p ts)
        (format t "~&Stopping condition met.~%"))))
  (call-next-method))
