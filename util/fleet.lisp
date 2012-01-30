;;; for vehicles/fleets
;;; ---------------------------
;;; 1. create-fleet generates/initializes the <fleet> object
;;; 2. total-dist returns the total distance given the fleet/vehicle object

(in-package :open-vrp.util)

(defgeneric route-indices (obj)
  (:method (vehicle) "Input is not a <vehicle>/<problem> object!")
  (:documentation "When input is a <vehicle>, returns its route as a list of node IDs. When input is <fleet>/<problem>, list all routes."))

(defmethod route-indices ((v vehicle))
  (mapcar #'node-ID (vehicle-route v)))

(defmethod route-indices ((p problem))
  (mapcar #'route-indices (problem-fleet p)))

(defgeneric vehicle-with-node (obj node-id)
  (:method (obj node) "Expects <problem> and int as inputs!")
  (:documentation "Given a node-ID, return the vehicle-ID that has the node in its route. The function for the input of the base-node 0 is undefined. Returns NIL if node-ID cannot be found."))

(defmethod vehicle-with-node ((p problem) node-ID)
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
			    (distance (node-id (car togo))
				      (node-id (cadr togo))
				      dist-array))))))
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
  (with-gensyms (fleet i route)
    `(let ((,fleet nil)
	   (,route ,(if to-depot `(list (aref ,network 0) (aref ,network 0))
			`(list (aref ,network 0)))))
       (do ((,i 0 (1+ ,i)))
	   ((= ,i ,fleet-size) (nreverse ,fleet))
	 (push (make-instance ,(cond (speeds ''vehicle-tw)
				     (capacities ''vehicle-c)
				     (t ''vehicle))
			      :id ,i
			      :route ,route
			      ,@(when capacities `(:capacity ,capacities))
			      ,@(when speeds `(:speed ,speeds)))
	       ,fleet)))))