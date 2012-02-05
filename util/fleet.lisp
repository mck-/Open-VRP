;;; Fleet related functions
;;; ---------------------------
;;; - route-indices returns list of node IDs, given <vehicle>/<problem>
;;; - vehicle-with-node returns <vehicle> that has the node-ID
;;; - create-vehicles generates a list of <Vehicle> objects
;;; - total-dist returns the total distance given a <vehicle>/<problem> object

(in-package :open-vrp.util)

(defgeneric route-indices (obj)
  (:method (vehicle) "Input is not a <vehicle>/<problem> object!")
  (:documentation "When input is a <vehicle>, returns its route as a list of node IDs. When input is <fleet>/<problem>, list all routes."))

(defmethod route-indices ((v vehicle))
  (mapcar #'node-ID (vehicle-route v)))

(defmethod route-indices ((p problem))
  (mapcar #'route-indices (problem-fleet p)))

(defgeneric vehicle-with-node-ID (obj node-id)
  (:method (obj node) "Expects <problem> and int as inputs!")
  (:documentation "Given a node-ID, return the vehicle-ID that has the node in its route. The function for the input of the base-node 0 is undefined. Returns NIL if node-ID cannot be found."))

(defmethod vehicle-with-node-ID ((p problem) node-ID)
  (position-if #'(lambda (route) (member node-ID route)) (route-indices p)))

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

;; Initializing functions 
;; (added Wed Nov 9,2011)
;; --------------------
;; Requires a network object to initialize the base locations

(defmacro create-vehicles (fleet-size network to-depot &optional capacities speeds)
  "Returns a list of vehicles, starting with ID 0. The starting location of their routes are all initialized at 0. When to-depot is set to T, initialize their routes with 2 base nodes (departure and destination)."
  (with-gensyms (route id)
    `(let* ((base (aref ,network 0))
	    (,route ,(if to-depot `(list base base) `(list base))))
       (loop for ,id from 0 to (1- ,fleet-size) collect
	    (make-instance 'vehicle
			   :id ,id
			   :route ,route
			   ,@(when capacities `(:capacity ,capacities))
			   ,@(when speeds `(:speed ,speeds)))))))