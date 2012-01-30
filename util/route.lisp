;;; Functions to operate on routes, which are a list of <node> objects
;;; contained in a <vehicle>'s :route slot.
;;; Uses simple list utilities (list.lisp)
(in-package :open-vrp.util)
;;; -------
;;; 0. Route utils
;;; 1. Insert node into the route
;;; 2. Remove node from the route
;;; 3. Last location
;;; --------------------

;; 0. Route utils
;; ---------------------
(defun empty-routep (veh)
  "Given a <vehicle>, return T if the route only has base-nodes."
  (not (member 0 (vehicle-route veh) :key #'node-id :test-not #'=)))

(defun get-busy-vehicles (problem)
  "Returns a list of <Vehicles> that are not empty, given a <TSP> object."
  (remove-if #'empty-routep (problem-fleet problem)))

(defun one-destinationp (route)
  "Return T if there is only one destination on route, excluding base nodes. Used by generate-moves in TS.lisp."
  (= 1 (length (remove 0 route :key #'node-id))))

(defmacro change-route (vehicle &body body)
  "Expands into binding the vehicles route to r and setting it to result of body"
  `(let ((r (vehicle-route ,vehicle)))
     (setf (vehicle-route ,vehicle) ,@body)))
;; ------------------

;; 1. Insert Node
;; -------------------
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
    (if (= 0 (node-id (car (last (vehicle-route v))))) ; if last node is 0, insert before that
	(reverse (insert-before n 1 (reverse r)))
	(insert-at-end n r))))

;; -------------------------

;; 2. Remove Node
;; -------------------------
(defgeneric remove-node-at (vehicle index)
  (:method (vehicle index) "Expects <vehicle> and int as inputs!")
  (:documentation "Removes the <node> from the route of <vehicle> at index"))

(defmethod remove-node-at ((v vehicle) index)
  (change-route v
    (remove-index index r)))

(defgeneric remove-node-ID (veh/prob node-ID)
  (:method (vehicle node-ID) "Expects <vehicle>/<problem> and int as inputs!")
  (:documentation "Removes the <node> with node-ID from the route of <vehicle>. Returns NIL if failed to find node-ID."))

(defmethod remove-node-ID ((v vehicle) node-ID)
  (if (member node-ID (vehicle-route v) :key #'node-id)
      (change-route v
	(remove node-ID r :key #'node-id :count 1)) ;count 1 for perform-move in TS.lisp.
      nil))
  
(defmethod remove-node-ID ((prob problem) node-ID)
  (aif (vehicle-with-node-ID prob node-ID)
       (remove-node-ID (vehicle prob it) node-ID)
       nil))
;; ----------------------------

;; 3. Last location
;; ----------------------------
(defgeneric last-node (vehicle)
  (:method (vehicle) "Expects <vehicle>")
  (:documentation "Returns the last <node> in its route. Depicts the current location (before returning to base)."))

(defmethod last-node (route)
  (let ((r (reverse route)))
    (if (= 0 (node-id (car r)))
	(or (cadr r) (car r)) ;in case route has only one base-node.
	(car r))))

(defmethod last-node ((v vehicle))
  (last-node (vehicle-route v)))

;; ---------------------------
      

  