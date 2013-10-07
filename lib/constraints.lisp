;;; Constraint checking functions
;;; --------------
;;; 0. General Constraints Checker tools
;;; 1. Capacity Constraints
;;; 2. Time-window Constraints
;;; ---------------
(in-package :open-vrp.util)

;; 0. General tools/definitions
;; ----------------------------

(defgeneric constraints-p (prob)
  (:documentation "Tests weather the solution in the <problem> is complying with the constraints. If the problem is a CVRP, check for capacity. If it is a VRPTW, check for time-windows. For CVRPTW, that inherits from both classes, check both constraints.")
  (:method-combination and))

(defmethod constraints-p and ((prob problem)) T)

(defmethod constraints-p and ((sol CVRP)) (in-capacity-p sol))

(defmethod constraints-p and ((sol VRPTW)) (in-timep sol))

;; Helper macro for defining constraints-checking methods below
;; Returns NIL as soon as it finds out that a constraint is violated
(defmacro constraints-check (arglist init-forms next-forms testform &optional endtest)
  (let ((iter (gensym)))
    `(labels ((,iter ,arglist
                (if ,@(if endtest `(,endtest) `((null ,(car arglist))))
                    (values T ,@(cdr arglist))
                    (and
                     ,testform
                     (,iter ,@next-forms)))))
       (,iter ,@init-forms))))
;; -------------------------

;; 1. Capacity Constraints
;; ------------------------

(defgeneric in-capacity-p (veh/cvrp)
  (:method (obj) "Expects a <Vehicle>/<CVRP> object!")
  (:documentation "Tests weather the route on <vehicle> is complying with the capacity constraint. Returns T and the remaining capacity if it does. When <CVRP> is provided, test all vehicles."))

(defmethod in-capacity-p ((v vehicle))
  (unless (vehicle-capacity v) (error 'no-capacities-vehicle :veh v))
  (constraints-check
   (route cap)
   ((vehicle-route v) (vehicle-capacity v))
   ((cdr route) (- cap (order-demand (car route))))
   (<= (order-demand (car route)) cap)))

(defmethod in-capacity-p ((pr CVRP))
  (reduce
   (lambda (x y)
     (and x (in-capacity-p y)))
   (problem-fleet pr)
   :initial-value T))

;; ------------------------------

;; 2. Time-window constraints
;; -------------------------

(defun travel-time (n1 n2 dist-matrix &key (speed 1))
  "Given two node-ids and optional speed, return the travel-time. By default, speed is 1, hence distance-matrix can also be the time-matrix."
  (check-type speed number)
  (/ (distance n1 n2 dist-matrix) speed))

(defun time-after-visit (visit arrival-time)
  "Given a visit to serve and the current time, return the new time (if on-time to begin with). When arrival-time is too early, wait till earliest start time. Time is given in minutes since midnight."
  (check-type visit visit)
  (check-type arrival-time (integer 0 1439))
  (cond ((> arrival-time (visit-end visit)) (error 'too-late-arrival :visit visit))
        ((< arrival-time (visit-start visit)) (+ (visit-start visit) (visit-duration visit))) ;wait
        (t (+ arrival-time (visit-duration visit)))))

(defun veh-in-timep (v &optional dist-array)
  "Tests weather the route on <Vehicle> is complying with the time-window constraints. Returns T and the time of finishing its last task."
  (unless (vehicle-speed v) (error 'no-speed-vehicle :veh v))
  (symbol-macrolet ((to (car route))
                    (arr-time (+ time (travel-time loc to :dist-array dist-array :speed (vehicle-speed v)))))
    (constraints-check
     (route time loc)
     ((cdr (vehicle-route v)) 0 (car (vehicle-route v)))
     ((cdr route) (time-after-visit to arr-time) to)
     (<= arr-time (node-end to)))))

(defmethod in-timep ((pr VRPTW))
  (constraints-check
   (veh)
   ((problem-fleet pr))
   ((cdr veh))
   (veh-in-timep (car veh) (aif (problem-dist-array pr) it))))
;; -------------------------
