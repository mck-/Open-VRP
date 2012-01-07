;;; Wed Nov 2, 2011 (c) Marc Kuo
;;; Object-oriented representation of routes and its operations
;;; Uses simple list utilities (util-list.lisp)
(in-package :open-vrp.util)
;;; -------
;;; A route is a list of <Node> objects (util-geometry.lisp)
;;; This list is to be held in a <vehicle>'s route slot (util-vehicle.lisp)
;;; Operations:
;;; 1. Insert node into the route
;;; 2. Remove node from the route
;;; 3. Last location
;;; 4. Closest node
;;; TABU SEARCH UTILS ---
;;; 5. Move Node with nodeID from v1 to v2 at v2loc

;; change-route macro binds the route to r and sets it to the result of &body
(defmacro change-route (vehicle &body body)
  "Expands into binding the vehicles route to r and setting it to result of body"
  `(let ((r (vehicle-route ,vehicle)))
     (setf (vehicle-route ,vehicle) ,@body)))

;; 1. Insert Node
(defgeneric insert-node (vehicle node index)
  (:method (vehicle node index) "Expecting <vehicle>, <node> and int as input!")
  (:documentation "Adds the <Node> object before the index of the route of <vehicle>. An index of 0 implies inserting in front, length of list implies at the end."))

(defmethod insert-node ((v vehicle) (n node) index)
  (change-route v
    (insert-before n index r)))

(defgeneric append-node (vehicle node)
  (:method (vehicle node) "Expects <vehicle> and <node>!")
  (:documentation "Appends <Node> to the end of the route of <vehicle>. Wrapper of insert-node."))

(defmethod append-node ((v vehicle) (n node))
  (change-route v
    (insert-at-end n r)))

;; 2. Remove Node
;; adjusted Sun Dec 11, 2011

(defgeneric remove-node-at (vehicle index)
  (:method (vehicle index) "Expects <vehicle> and int as inputs!")
  (:documentation "Removes the <node> from the route of <vehicle> at index"))

(defmethod remove-node-at ((v vehicle) index)
  (change-route v
    (remove-index index r)))

;; added Thu 29 Dec, 2011
(defgeneric remove-node-ID (veh/prob node-ID)
  (:method (vehicle node-ID) "Expects <vehicle>/<problem> and int as inputs!")
  (:documentation "Removes the <node> with node-ID from the route of <vehicle> which has it. Returns NIL if failed to find node-ID."))

(defmethod remove-node-ID ((v vehicle) node-ID)
  (if (member node-ID (route-indices v))
      (change-route v
	(remove node-ID r :key #'node-id))
      nil))
  
(defmethod remove-node-ID ((prob problem) node-ID)
  (aif (vehicle-with-node prob node-ID)
       (remove-node-ID (vehicle prob it) node-ID)
       nil))

;; 3. Last location
(defgeneric last-node (vehicle)
  (:method (vehicle) "Expects <vehicle>")
  (:documentation "Returns the last <node> in its route. Depicts the current location."))

(defmethod last-node ((v vehicle))
  "Returns the last node in the route."
  (car (last (vehicle-route v))))

;; 4. Closest node
;; Using the ID of the last node on the route of the vehicle, to lookup the dist-matrix for quick lookup, instead of recalculating the distances. After getting the closest node ID, retrieve the <Node> object from the network.
(defun get-min-index-with-tabu (distances tabu)
  "Returns index of the first next closest, that is not in chosen (which is a list)."
  (with-tabu-indices tabu #'get-min-index distances))

(defgeneric closest-node (vehicle network &optional tabu)
; It's not allowing &optional parameter in :method expression below.
;  (:method (vehicle network &optional tabu) "Expects <vehicle>, <network> or <problem> veh-id.")
  (:documentation "Returns the closest node from the current location of vehicle v. Requires <vehicle> and <network>. A tabu list of node-IDs is optional. Also accepts <problem> and veh-id as first two arguments."))

(defmethod closest-node ((v vehicle) (net network) &optional tabu)
  (let* ((loc (last-node v))                                  ;binds list of distances..
	 (dists (get-array-row (network-dist-table net) (node-id loc))));..from the location..
    (aif (get-min-index-with-tabu dists tabu)
	 (node net it)
	 nil)))

(defmethod closest-node ((prob problem) veh-id &optional tabu)
  (let ((veh (vehicle prob veh-id))
	(net (problem-network prob)))
    (closest-node veh net tabu)))

  