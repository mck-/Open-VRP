;;; Thu Dec 8, 2011 (c) Marc Kuo
;;; ------------------------
;;; Greedy Appending heuristic
;;; Using a (random) sequence, append the <Nodes> one by one in the nearest <Vehicle>
(in-package :open-vrp.algo)

;; 1. Generate random insertion sequence
;; (random-list-permutation #nodes)
;; (node <tsp> int)

;; 2. For each node, append to closest vessel feasible
;; (get-closest-vehicle <node> <tsp>)
;; (append-node <vehicle> <node>)

(defmethod run-algo ((p tsp) (a greedy-append))
  "Randomly append <Nodes> one by one to the closest <Vehicle>. Returns <Algo> object when done. Also prints the fitness and solution."
  (loop for id in (random-list-permutation (1- (length (network-nodes (problem-network p)))))
     do (append-node (get-closest-feasible-vehicle (node p id) p) ; closest vehicle
		     (node p id))
       finally (setf (algo-best-sol a) p
		     (algo-current-sol a) p
		     (algo-best-fitness a) (fitness p)))
       
  a)
       
