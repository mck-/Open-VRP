;;; Fleet related functions
;;; ---------------------------
;;; - route-indices (<vehicle>/<problem>) - returns list of node IDs
;;; - vehicle-with-node	(<Problem> int)   - returns <vehicle> that has the node-ID
;;; - total-dist (<vehicle>/<problem>)    - returns the total distance
;;; - vehicle (<problem> int)             - returns <Vehicle> with id
;;; - new-vehicle                         - macro that creates a <Vehicle> according to input

(in-package :open-vrp.util)

(defgeneric route-indices (obj)
  (:method (vehicle) "Input is not a <vehicle>/<problem> object!")
  (:documentation "When input is a <vehicle>, returns its route as a list of node IDs. When input is <fleet>/<problem>, list all routes."))

(defmethod route-indices ((v vehicle))
  (mapcar #'node-ID (vehicle-route v)))

(defmethod route-indices ((p problem))
  (mapcar #'route-indices (problem-fleet p)))

(defun node-on-routep (node-id vehicle)
  "Returns NIL if <vehicle> does not have the node on its route."
  (member node-id (vehicle-route vehicle) :key #'node-id))

(defun vehicle-with-node-ID (prob node-ID)
  "Given a node-ID, return the vehicle-ID that has the node in its route. The function for the input of the base-node 0 is undefined. Returns NIL if node-ID cannot be found."
  (position-if #'(lambda (veh) (node-on-routep node-ID veh)) (problem-fleet prob)))

(defgeneric total-dist (veh/prob dist-array)
  (:method (veh/prob dist-array) "Expects <problem> as input!")
  (:documentation "Returns total distance of the route(s) given a vehicle or a fleet."))

(defmethod total-dist ((v vehicle) dist-array)
  (let ((route (vehicle-route v)))
    (labels ((iter (togo sum)
               (if (null (cdr togo)) sum
                   (iter (cdr togo)
                         (+ sum
                            (handler-case (distance (node-id (car togo))
                                                    (node-id (cadr togo))
                                                    dist-array)
                              (same-origin-destination () 0)))))))
      (iter route 0))))


(defmethod total-dist ((p problem) dist-array)
  (sum (mapcar #'(lambda (v) (total-dist v dist-array)) (get-busy-vehicles p))))

;; Accessor functions
;; ------------------
(defmethod vehicle ((p problem) id)
  (nth id (problem-fleet p)))
;; ------------------

;; Create Vehicle macro
;; ------------------
(defmacro new-vehicle (id base-node to-depot &key capacity speed)
  `(make-vehicle
    :id ,id
    :route ,(if to-depot `(list ,base-node ,base-node) `(list ,base-node))
    ,@(when capacity `(:capacity ,capacity))
    ,@(when speed `(:speed ,speed))))
