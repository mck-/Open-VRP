;;; Functions to operate on routes, which are a list of <node> objects
;;; contained in a <vehicle>'s :route slot.
(in-package :open-vrp.util)
;;; -------
;;; 0. Route utils
;;; 1. Insert node into the route
;;; 2. Remove node from the route
;;; 3. Last location
;;; --------------------

;; 0. Route utils
;; ---------------------
(defun no-visits-p (route)
  "Given a route, return T if the route only has base-nodes."
  (not (some #'order-p route)))

(defun get-busy-vehicles (problem)
  "Returns a list of <Vehicles> that are not empty, given a <Problem> object."
  (remove-if #'no-visits-p (problem-fleet problem) :key #'vehicle-route))

(defun one-destinationp (route)
  "Return T if there is only one order on route."
  (= 1 (count-if #'order-p route)))

(defmacro change-route (vehicle &body body)
  "Expands into binding the vehicles route to r and setting it to result of body."
  `(let ((r (vehicle-route ,vehicle)))
     (setf (vehicle-route ,vehicle) ,@body)))
;; ------------------

;; 1. Insert Node
;; -------------------

(defun insert-node (veh node index)
  "Adds the <Node> object before the index of the route of <vehicle>. An index of 0 implies inserting in front, length of list implies at the end."
  (change-route veh
    (insert-before node index r)))

(defun append-node (veh node)
  "Appends <Node> to the end of the route of <vehicle>. Wrapper of insert-node. If the route includes returning to-depot, then append before the last return to depot."
  (change-route veh
    (if (and (cdr (vehicle-route veh))
             (= 0 (node-id (car (last (vehicle-route veh))))))
        (reverse (insert-before node 1 (reverse r)))
        (insert-at-end node r))))

;; -------------------------

;; 2. Remove Node
;; -------------------------
(defun remove-node-at (veh index)
  "Removes the <node> from the route of <vehicle> at index"
  (change-route veh
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
