;;; Nearest Neighborhood algorithm (greedy) for TSP (NOT defined for (C)VRP(TW)!)
;;; -----
;;; Start at base, and keep choosing the next closest one
(in-package :open-vrp.algo)

(defclass greedy-NN (algo)
  ((name :initform "Greedy NN-algo")
   (desc :initform "Nearest Neighborhood algo; from base/random, select next closest one")))

;; Greedy NN algo for TSP problem (no fleet)

(defmethod run-algo ((p problem) (a greedy-NN))
  "While there exists unchosen nodes, keep appending it. Returns the <Algo> object when done. Also prints the fitness and solution (run-algo :after method)."
  (let ((v (vehicle p 0)))
    (awhile (handler-case
                (get-closest-node p 0 (route-indices v))
              (list-of-nils () nil))
      (append-node v it)))
  (init-algo p a)
  a)

;; Closest node (used by Greedy Nearest Neighborhood)
;; ---------------------------

(defun get-min-index-with-tabu (distances tabu)
  "Returns index of the first next closest, that is not in chosen (which is a list)."
  (with-tabu-indices tabu #'get-min-index distances))

(defun get-closest-node (prob veh-id &optional tabu)
  "Returns the closest node from the last location of vehicle. Requires <problem> and vehicle-ID. A tabu list of node-IDs is optional to exclude consideration of some nodes."
  (let* ((loc (last-node (vehicle prob veh-id)))
         (dists (get-array-row (problem-dist-array prob) (node-id loc))))
    (aif (get-min-index-with-tabu dists tabu)
         (node prob it)
         nil)))
