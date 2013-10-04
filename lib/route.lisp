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
  "Given a route, return T if the route does not contain any orders (pitstops do not count)."
  (check-type route sequence)
  (notany #'order-p route)))

(defun get-busy-vehicles (problem)
  "Returns a list of <Vehicles> that are not empty, given a <Problem> object."
  (check-type problem problem)
  (remove-if #'no-visits-p (problem-fleet problem) :key #'vehicle-route))

(defun one-destination-p (route)
  "Return T if there is only one order on route."
  (check-type route sequence)
  (= 1 (count-if #'order-p route)))

(defmacro with-changing-route ((var vehicle) &body body)
  "Expands into binding the vehicles route to var and setting it to result of body."
  `(let ((,var (vehicle-route ,vehicle)))
     (setf (vehicle-route ,vehicle) ,@body)))
;; ------------------

;; 1. Insert Node
;; -------------------

(defun insert-node (veh node index)
  "Adds the <Node> object before the index of the route of <vehicle>. An index of 0 implies inserting in front, length of list implies at the end."
  (with-changing-route (r veh)
    (insert-before node index r)))

(defun append-node (veh node)
  "Appends <Node> to the end of the route of <vehicle>. Wrapper of insert-node."
  (with-changing-route (r veh)
    (insert-before node (length r) r)))

;; -------------------------

;; 2. Remove Node
;; -------------------------
(defun remove-node-at (veh index)
  "Removes the <node> from the route of <vehicle> at index"
  (with-changing-route (r veh)
    (remove-index index r)))

(defgeneric remove-node-id (veh/prob node-id)
  (:method (vehicle node-id) "Expects <vehicle>/<problem> and int as inputs!")
  (:documentation "Removes the <node> with node-id from the route of <vehicle>. Returns NIL if failed to find node-id."))

(defmethod remove-node-id ((v vehicle) node-id)
  (if (member node-id (vehicle-route v) :key #'visit-node-id)
      (with-changing-route (r v)
        (remove node-id r :key #'node-id :count 1)) ;count 1 for perform-move in TS.lisp.
      nil))

(defmethod remove-node-id ((prob problem) node-id)
  (aif (vehicle-with-node-id prob node-id)
       (remove-node-id (vehicle prob it) node-id)
       nil))
;; ----------------------------

;; 3. Last location
;; ----------------------------
(defgeneric last-node (vehicle)
  (:method (vehicle) "Expects <vehicle>")
  (:documentation "Returns the last <node> in its route. Depicts the current location (before returning to base)."))

;; (defmethod last-node (route)
;;   (let ((r (reverse route)))
;;     (if (= 0 (node-id (car r)))
;;         (or (cadr r) (car r))   ;in case route has only one base-node.
;;         (car r))))

(defmethod last-node ((v vehicle))
  (last-node (vehicle-route v)))

;; ---------------------------
