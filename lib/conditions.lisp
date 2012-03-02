;; Error condition definitions
;; -------------------------

(in-package :open-vrp.util)

;; lib/lists.lisp
(define-condition unaccepted-predicate (error)
  ((pred :initarg :pred :reader pred))
  (:report "Accepts only #'> or #'<."))

(define-condition index-out-of-bounds (error)
  ((index :initarg :index :reader index)
   (ls :initarg :ls :reader ls)))

;; lib/network.lisp
(define-condition same-origin-destination (error)
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to))
  (:report "Trying to lookup distance for same origin and destination - NIL"))

;; lib/constraints.lisp
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

;; lib/network.lisp lib/fleet.lisp
(define-condition not-equal-length (error)
  ((list1 :initarg :list1 :reader list1)
   (list2 :initarg :list2 :reader list2))
  (:report "Trying to create objects where input lists are of unequal length!"))  

;; lib/init-macros.lisp
(define-condition empty-network (error)()
  (:report "Network is empty! To create a network requires at least one parameter!"))