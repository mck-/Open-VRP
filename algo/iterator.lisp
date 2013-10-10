;; Iterator framework -- Search-heuristic framework used by Tabu-Search
;; --------------------------------------------
(in-package :open-vrp.algo)


;; initializer
;; -----------------
(defgeneric initialize (problem algo)
  (:method (problem algo) "initialize: Requires <Problem> and <Algo> as inputs.")
  (:documentation "Initializes the initial solution for the algo object."))
;; ----------------

;; Generate-moves
;; -------------------
(defgeneric generate-moves (algo)
  (:method (algo)
    "generate-moves: This algo is not defined; cannot generate moves.")
  (:documentation "Given the algo object, that contains the current solution, generate potential <moves> for next iteration as defined by the algo. e.g. insertion moves for TS and chromosome pool for GA."))

;; --------------------

;; Perform move
;; ---------------------
(defgeneric perform-move (sol move)
  (:method (sol move)
    "perform-move: This move is not defined.")
  (:documentation "Performs the move defined in <move> on the solution. Returns the new solution (which is a class of <Problem>)"))

;; -----------------------

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

;; feasibility check
(defmethod assess-move :around ((sol problem) (m move))
  (if (or (typep m 'TS-best-insertion-move) (feasible-move-p sol m))
      (call-next-method)
      (setf (move-fitness m) nil)))

(defun assess-moves (solution moves)
  "Given a list of <Move> objects, assess them all on the solution (uses assess-move), and setf the move's :fitness slots. Returns the list of moves."
  (mapcar #'(lambda (move) (assess-move solution move)) moves)
  moves)

;; -------------------

;; Select move
;; -------------------
(defgeneric select-move (algo moves)
  (:method (algo moves) "select-move: not defined for <Algo>")
  (:documentation "Given an <Algo> object and the list of <Moves>, select a move. By default, sort the moves and select the best one, but e.g. for tabu-search, check first of the <Move> is tabu."))

(defun sort-moves (moves)
  "Given a list of <Move>s, sort them according to fitness (ascending). Undestructive. Removes all infeasible moves."
  (remove-if #'null (sort-ignore-nil moves #'< :key #'move-fitness) :key #'move-fitness))

(defmethod select-move ((a algo) moves)
  (unless (move-fitness (car moves)) (error 'no-feasible-move :moves moves))
  (car (sort-moves moves)))

;; ------------------------------------------------
