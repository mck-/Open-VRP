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
       
