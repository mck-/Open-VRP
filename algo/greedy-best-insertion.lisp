;;; Thu Jan 16, 2012 (c) Marc Kuo
;;; ------------------------
;;; Greedy Best Insertion heuristic
;;; Using a (random) sequence, insert the <Nodes> one by one in the best feasible <vehicle>
;;; and at the optimal location in its route. Used as a feasible initial solution to the 
;;; Tabu Search. Randomizing the sequence assures a broad search space when using multi-run

(in-package :open-vrp.algo)

(defmethod run-algo ((p tsp) (a greedy-best-insertion))
  "Randomly insert <Nodes> one by one to best <vehicle> in best location. Returns <Algo> object when done."
  )
  