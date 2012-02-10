;; Algo related conditions
(in-package :open-vrp.algo)

;; algo/TS.lisp
(define-condition all-moves-tabu (error)
  ((moves :initarg :moves :reader moves)
   (tabu-list :initarg :tabu-list :reader tabu-list))
  (:report "All possible moves are on the Tabu-list! Consider reducing tabu-tenure, or override the select-move procedure."))

;; algo/best-insertion.lisp
(define-condition no-feasible-move (error)
  ((moves :initarg :moves :reader moves))
  (:report "All moves are infeasible!"))