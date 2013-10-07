;;; Fleet related functions
;;; ---------------------------
;;; - route-indices (<vehicle>/<problem>) - returns list of node IDs
;;; - vehicle-with-node-ID	(<Problem> int)   - returns <vehicle> that has the node-ID
;;; - total-dist (<vehicle>/<problem>)    - returns the total distance
;;; - vehicle (<problem> int)             - returns <Vehicle> with id
;;; - new-vehicle                         - macro that creates a <Vehicle> according to input

(in-package :open-vrp.util)

(defgeneric route-indices (obj)
  (:method (vehicle) "Input is not a <vehicle>/<problem> object!")
  (:documentation "When input is a <vehicle>, returns its route as a list of node IDs. When input is <fleet>/<problem>, list all routes."))

(defmethod route-indices ((v vehicle))
  (if (or (eq :nil (vehicle-start-location v))
          (eq :nil (vehicle-end-location v)))
      (mapcar #'visit-node-id (vehicle-route v))
      (nconc (list (vehicle-start-location v))
             (mapcar #'visit-node-id (vehicle-route v))
             (list (vehicle-end-location v)))))

(defmethod route-indices ((p problem))
  (mapcar #'route-indices (problem-fleet p)))

(defun node-on-route-p (node-id vehicle)
  "Returns NIL if <vehicle> does not have the node on its route."
  (check-type node-id symbol)
  (check-type vehicle vehicle)
  (find node-id (vehicle-route vehicle) :key #'visit-node-id))

(defun vehicle-with-node-id (prob node-id)
  "Given a node-id, return the vehicle-id that has the node in its route. Returns NIL if node-id cannot be found. Assumes only 1 presence of a node in the problem."
  (check-type prob problem)
  (check-type node-id symbol)
  (reduce
   (lambda (x y)
     (or x (when (node-on-route-p node-id y)
             (vehicle-id y))))
   (problem-fleet prob)
   :initial-value nil))

(defun route-dist (veh dist-matrix)
  "Returns total distance of the route(s) given a vehicle. Takes into account the start and end locations of the vehicle."
  (check-type veh vehicle)
  (check-type dist-matrix hash-table)
  (labels ((iter (togo sum)
             (if (null (cdr togo)) sum
                 (iter (cdr togo)
                       (+ sum
                          (distance (car togo)
                                    (cadr togo)
                                    dist-matrix))))))
    ;; Insert start and end-locations into route
    (iter (route-indices veh) 0)))


(defun total-dist (problem)
  "Returns total distance of all routes combined. Includes to and from start and end locations."
  (sum
   (mapcar #'(lambda (v)
               (route-dist v (problem-dist-matrix problem)))
           (get-busy-vehicles problem))))

;; Accessor functions
;; ------------------
(defmethod vehicle ((p problem) id)
  (find id (problem-fleet p) :key #'vehicle-id))
;; ------------------

;; Create Vehicle macro
;; ------------------
(defmacro new-vehicle (id base-node to-depot &key capacity speed)
  `(make-vehicle
    :id ,id
    :route ,(if to-depot `(list ,base-node ,base-node) `(list ,base-node))
    ,@(when capacity `(:capacity ,capacity))
    ,@(when speed `(:speed ,speed))))
