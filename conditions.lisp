;; Error condition definitions
;; -------------------------

(in-package :open-vrp.util)

;; util/lists.lisp
(define-condition unaccepted-predicate (error)
  ((pred :initarg :pred :reader pred))
  (:report "Accepts only #'> or #'<."))

(define-condition index-out-of-bounds (error)
  ((index :initarg :index :reader index)
   (ls :initarg :ls :reader ls)))

;; util/network.lisp
(define-condition same-origin-destination (error)
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to))
  (:report "Trying to lookup distance for same origin and destination - NIL"))

;; constraints.lisp
(define-condition infeasible-solution (error)
  ((sol :initarg :sol :reader sol)
   (func :initarg :func :reader func))
  (:report "The provided solution is already infeasible, cannot check for feasibility of the move."))

(define-condition no-capacities-vehicle (error)
  ((veh :initarg :veh :reader veh))
  (:report "Trying to check capacities for a vehicle that has no defined capacity."))

(define-condition no-speed-vehicle (error)
  ((veh :initarg :veh :reader veh))
  (:report "Trying to check TW constraints for a vehicle that has no defined speed."))
   

;; algo/TS.lisp
(define-condition all-moves-tabu (error)
  ((moves :initarg :moves :reader moves)
   (tabu-list :initarg :tabu-list :reader tabu-list)))