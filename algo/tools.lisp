;;; Tools to be shared among algorithms
;;; ---------------------------
;;; 0. Miscellaneous
;;; 1. Move feasibility checks
;;; 2. Heuristical tools

(in-package :open-vrp.algo)

;; 0. Misc
;; -------------------------
(defstruct move fitness)

(defstruct (insertion-move (:include move) (:conc-name move-)) node-id vehicle-id index)

(defun route-from (ins-move sol)
  "Returns the route that contains the node that will be moved. Returns NIL if it cannot be found in any vehicle."
  (aif (vehicle-with-node-id sol (move-node-id ins-move))
       (vehicle-route (vehicle sol it))
       nil))

(defun route-to (ins-move sol)
  "Returns the route that will be affected by the insertion-move."
  (vehicle-route (vehicle sol (move-vehicle-ID ins-move))))

;; --------------------------

;; 1. Feasibility check of moves
;; ---------------------------

(defgeneric feasible-move-p (sol move)
  (:documentation "Given a current solution, assess feasibility of the <Move>. For CVRP, just check if it fits in the total vehicle capacity. For VRPTW, check for TW feasibility of the whole route. For CVRPTW, checks both by means of multiple-inheritance and method-combination.")
  (:method-combination and))

(defmethod feasible-move-p and ((sol problem) (m move)) T)

(defmethod feasible-move-p and ((sol CVRP) (m insertion-move))
  (with-slots (node-id vehicle-id) m
    (let ((veh (vehicle sol vehicle-id)))
      ; if node is already on the route, moving intra-route is feasible
      (if (node-on-route-p node-id veh) T
          (multiple-value-bind (comply cap-left) (in-capacity-p veh)
            (unless comply (error 'infeasible-solution :sol sol :func #'in-capacity-p))
            (<= (order-demand (visit-node sol node-id)) cap-left))))))

(defmethod feasible-move-p and ((sol VRPTW) (m insertion-move))
  (with-slots (node-id vehicle-id index) m
    (let* ((v (vehicle sol vehicle-id))
           (route (vehicle-route v))
           (ins-node (visit-node sol node-id)))
      (labels ((iter (route time loc i)
                 (if (and (null route) (< i 0))
                     (values (<= (+ time (travel-time loc (vehicle-end-location v) (problem-dist-matrix sol) :speed (vehicle-speed v)))
                                 (vehicle-shift-end v))
                             time loc)
                     (let* ((to (if (= 0 i) ins-node (car route)))
                            (arr-time (+ time (travel-time loc (visit-node-id to) (problem-dist-matrix sol)))))
                       (and
                        (<= arr-time (visit-end to))
                        (iter (if (= 0 i) route (cdr route))
                              (time-after-visit to arr-time)
                              (visit-node-id to)
                              (1- i)))))))
        (iter route (vehicle-shift-start v) (vehicle-start-location v) index)))))

;; for debugging (insert in test-form with progn)
                       ;; (progn
                       ;;   (format t "Route: ~A~% Loc: ~A~% To: ~A~% Time: ~A~% Arr-time: ~A~% Node-start: ~A~% Node-end: ~A~% Duration: ~A~% ins-node-end: ~A~% i: ~A~%" (mapcar #'visit-node-id route) loc (visit-node-id to) time arr-time (visit-start to) (visit-end to) (visit-duration to) (visit-end ins-node) i)

;; for debugging arrival at home (insert in return form with progn)
;;                      (progn
                       ;; (format t "OK! Time: ~A~% Arrival at home: ~A~% Shift-end: ~A~% Return: ~A~%" time (+ time (travel-time loc (vehicle-end-location v) (problem-dist-matrix sol) :speed (vehicle-speed v))) (vehicle-shift-end v) (<= (+ time (travel-time loc (vehicle-end-location v) (problem-dist-matrix sol) :speed (vehicle-speed v))) (vehicle-shift-end v)))
