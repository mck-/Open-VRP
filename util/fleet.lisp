;;; Thu Oct 27, 2011 (YYZ to AMS) (c) Marc Kuo
;;; Utilities for vehicles/fleets
;;; ---------------------------
;;; 1. create-fleet generates/initializes the <fleet> object
;;; 2. total-dist returns the total distance given the fleet/vehicle object

(in-package :open-vrp.util)

(defgeneric route-indices (obj)
  (:method (vehicle) "Input is not a <vehicle>/<fleet>/<problem> object!")
  (:documentation "When input is a <vehicle>, returns its route as a list of node IDs. When input is <fleet>/<problem>, list all routes."))

(defmethod route-indices ((v vehicle))
  (mapcar #'node-ID (vehicle-route v)))

(defmethod route-indices ((f fleet))
  (mapcar #'route-indices (fleet-vehicles f)))

(defmethod route-indices ((p problem))
  (route-indices (problem-fleet p)))

(defgeneric vehicle-with-node (obj node-id)
  (:method (obj node) "Expects <problem>/<fleet> and int as inputs!")
  (:documentation "Given a node-ID, return the vehicle-ID that has the node in its route. The function for the input of the base-node 0 is undefined. Returns NIL if node-ID cannot be found."))

(defmethod vehicle-with-node ((f fleet) node-ID)
  (position-if #'(lambda (route) (member node-ID route)) (route-indices f)))

(defmethod vehicle-with-node ((p problem) node-ID)
  (vehicle-with-node (problem-fleet p) node-ID))

(defgeneric total-dist (obj net)
  (:method (obj net) "Expects <vehicle>/<fleet> and <network> as inputs!")
  (:documentation "Returns total distance of the route(s) given a vehicle or a fleet."))

(defmethod total-dist ((v vehicle) (net network))
  (let ((route (vehicle-route v)) ;A list of node ID's..
	(dist-table (network-dist-table net)))       ;..to be used for quick lookup
    (labels ((iter (togo sum)
	       (if (null (cdr togo)) sum
		   (iter (cdr togo)
			 (+ sum
			    (distance (node-id (car togo))
				      (node-id (cadr togo))
				      dist-table))))))
      (iter route 0))))
	       

(defmethod total-dist ((f fleet) (net network))
  (sum (mapcar #'(lambda (v) (total-dist v net)) (fleet-vehicles f))))

;; Accessor functions
;; ------------------
(defgeneric vehicle (obj id)
  (:method (obj id) "Expects <fleet>/<problem> and int as inputs!")
  (:documentation "A simple accessor method, for quick id lookup of a <vehicle>"))

(defmethod vehicle ((f fleet) id)
  (nth id (fleet-vehicles f)))

(defmethod vehicle ((p problem) id)
  (vehicle (problem-fleet p) id))
;; ------------------

;; Initializing functions 
;; (added Wed Nov 9,2011)
;; --------------------
;; Requires a network object to initialize the base locations

(defmacro create-vehicles (fleet-size network &optional capacities)
  "Returns a list of vehicles, starting with ID 0. The starting location of their routs are all initialized at 0."
  (let ((fleet (gensym))
	(i (gensym))
	(route (gensym)))
    `(let ((,fleet nil)
	   (,route (list (node ,network 0))))
       (do ((,i 0 (1+ ,i)))
	   ((= ,i ,fleet-size) (nreverse ,fleet))
	 (push (make-instance ,(if capacities ''vehicle-c ''vehicle)
			      :id ,i
			      :route ,route
			      ,@(when capacities `(:capacity ,capacities)))
	       ,fleet)))))
  
(defmacro create-fleet (fleet-size network &optional capacities)
  "Returns a fleet object, with the <vehicle> objects of type initialised in the vehicles slot. Requires a <network> object for base node initialisation. When capacities is provided, capacitated <vehicles-c> will be createed."
  `(let ((fleet (create-vehicles ,fleet-size ,network ,@(when capacities `(,capacities)))))
     (make-instance 'fleet :vehicles fleet)))