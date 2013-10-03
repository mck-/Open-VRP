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
  (mapcar #'visit-node-id (vehicle-route v)))

(defmethod route-indices ((p problem))
  (mapcar #'route-indices (problem-fleet p)))

(defun node-on-route-p (node-id vehicle)
  "Returns NIL if <vehicle> does not have the node on its route."
  (unless (eq (type-of node-id) 'keyword)
    (error 'expect-keyword-arguments :arg node-id))
  (unless (vehicle-p vehicle)
    (error 'expect-vehicle :arg vehicle))
  (member node-id (vehicle-route vehicle) :key #'visit-node-id))

(defun vehicle-with-node-id (prob node-id)
  "Given a node-id, return the vehicle-id that has the node in its route. Returns NIL if node-id cannot be found. Assumes only 1 presence of a node in the problem."
  (unless (typep prob 'problem) (error 'expect-problem :arg prob))
  (labels ((iter (fleet)
             (cond ((null fleet) nil)
                   ((node-on-route-p node-id (car fleet)) (vehicle-id (car fleet)))
                   (t (iter (cdr fleet))))))
    (iter (problem-fleet prob))))

(defun route-dist (veh dist-matrix)
  "Returns total distance of the route(s) given a vehicle"
  (let ((route (vehicle-route veh)))
    (labels ((iter (togo sum)
               (if (null (cdr togo)) sum
                   (iter (cdr togo)
                         (+ sum
                            (handler-case (distance (visit-node-id (car togo))
                                                    (visit-node-id (cadr togo))
                                                    dist-matrix)
                              (same-origin-destination () 0)))))))
      (iter route 0))))


(defun total-dist (problem)
  (sum
   (mapcar #'(lambda (v)
               (route-dist v (problem-dist-matrix problem)))
           (get-busy-vehicles problem))))

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
