;;; Thu Dec 8, 2011 (c) Marc Kuo
;;; ------------------------
;;; Greedy Appending heuristic
;;; Using a (random) sequence, append the <Nodes> one by one in the nearest <Vehicle>
;;; May cause error for VRPTW or CVRP, when the number of vehicles available are too low
;;; to append all the nodes. Use greedy-best-insertion instead!

(in-package :open-vrp.algo)

;; 1. Generate random insertion sequence
;; 2. For each node, append to closest vessel feasible

(defmethod run-algo ((p problem) (a greedy-append))
  "Randomly append <Nodes> one by one to the closest <Vehicle>. Returns <Algo> object when done. Also prints the fitness and solution."
  (loop for id in (random-list-permutation (1- (length (problem-network p))))
     do (append-node (get-closest-feasible-vehicle (node p id) p) ; closest vehicle
		     (node p id))
     finally (setf (algo-best-sol a) p
		   (algo-current-sol a) p
		   (algo-best-fitness a) (fitness p))
       (return a)))
       
