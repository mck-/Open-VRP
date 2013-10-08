;;; Greedy Appending heuristic
;;; --------------
;;; Using a (random) sequence, append the <Nodes> one by one in the nearest <Vehicle>
;;; May cause error for VRPTW or CVRP, when the number of vehicles available are too low
;;; to append all the nodes. Use greedy-best-insertion instead!

(in-package :open-vrp.algo)

(defclass greedy-append (algo)
  ((name :initform "Greedy Appending heuristic")
   (desc :initform "Random greedy insertion heuristic; append nodes to closest vehicle successively. Used as initial solution for search algos.")))

(defmethod run-algo ((p problem) (a greedy-append))
  "Randomly append <Nodes> one by one to the closest <Vehicle>. Returns <Algo> object when done. Also prints the fitness and solution."
  (loop for id in (random-list-permutation (1- (length (problem-network p))))
     do (append-node (get-closest-feasible-vehicle (node p id) p) ; closest vehicle
                     (node p id))
     finally (init-algo p a)
       (return a)))

;; -----------------------------

;; Closest Vehicle (used by Greedy Append)
;; ---------------------------
(defun dists-to-vehicles (node prob)
  "Given a <Node> and a <Problem>, return the list of all the distances from the <Node> to the current positions of the fleet. Used by get-closest-(feasible)-vehicle."
  (mapcar #'(lambda (x) (distance (node-id (last-node x))
                                  (node-id node)
                                  (problem-dist-array prob)))
          (problem-fleet prob)))

;; challenge: what if the vehicle is located on the node n - use only for initial insertion?
(defun get-closest-vehicle (n prob)
  "Returns the closest <vehicle> to <node>. Used by insertion heuristic. When multiple <vehicle> are on equal distance, choose first one (i.e. lowest ID)."
  (vehicle prob (get-min-index (dists-to-vehicles n prob))))
;; -------------------------

;; Closest Feasible Vehicle
;; ----------------------------
(defmethod get-closest-feasible-vehicle ((n node) (prob problem))
  (get-closest-vehicle n prob))

;; Capacity check
(defun capacities-left (prob)
  "Returns a list of all capacities left on the vehicles given the present solution."
  (mapcar #'(lambda (x) (multiple-value-bind (c cap)
                            (in-capacity-p x) (when c cap)))
          (problem-fleet prob)))

(defmethod get-closest-feasible-vehicle ((n node) (prob CVRP))
  "Returns the vehicle closest to the node and has enough capacity."
  (handler-case
      (vehicle prob (get-min-index
                     (mapcar #'(lambda (dist cap)
                                 (unless (> (node-demand n) cap) dist))
                             (dists-to-vehicles n prob)
                             (capacities-left prob))))
    (list-of-nils () (error 'no-feasible-move :moves n))))


;; Time-window check
(defun times-of-arriving (node prob)
  "Returns a list of arrival times of the vehicles to node given the present solution."
  (mapcar #'(lambda (x)
              (multiple-value-bind (c time)
                  (veh-in-time-p x) (when c (+ time (travel-time (last-node x) node :dist-array (problem-dist-array prob))))))
          (problem-fleet prob)))

;; Feasiblility of appending at the end only.
(defmethod get-closest-feasible-vehicle ((n node) (prob VRPTW))
  "Returns the vehicle closest to the node that has enough time at the end of its route. Used for appending nodes. Use get-optimal-insertion instead for inserting feasibly into routes."
  (handler-case
      (vehicle prob (get-min-index
                     (mapcar #'(lambda (dist arr-time cap)
                                 (unless (or (> (node-demand n) cap)
                                             (> arr-time (node-end n)))
                                   dist))
                             (dists-to-vehicles n prob)
                             (times-of-arriving n prob)
                             (capacities-left prob))))
    (list-of-nils () (error 'no-feasible-move :moves n))))
