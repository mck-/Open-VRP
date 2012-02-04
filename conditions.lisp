;; Error condition definitions
;; -------------------------

(in-package :open-vrp.util)

;; util/lists.lisp
(define-condition unaccepted-predicate (error)
  ((pred :initarg :pred :reader pred)))

(define-condition index-out-of-bounds (error)
  ((index :initarg :index :reader index)
   (ls :initarg :ls :reader ls)))

;; constraints.lisp
(define-condition infeasible-solution (error)
  ((sol :initarg :sol :reader sol)
   (func :initarg :func :reader func)
   (msg :initarg :msg :reader msg)))

;; algo/TS.lisp
(define-condition all-moves-tabu (error)
  ((moves :initarg :moves :reader moves)
   (tabu-list :initarg :tabu-list :reader tabu-list)))