;;; Greedy Best Insertion heuristic
;;; -------
;;; Using a (random) sequence, insert the <Nodes> one by one in the best feasible <vehicle>
;;; and at the optimal location in its route. Used as a feasible initial solution to the
;;; Tabu Search. Randomizing the sequence assures a broad search space when using multi-run

(in-package :open-vrp.algo)

(defclass greedy-best-insertion (algo)
  ((name :initform "Greedy Best Insertion heuristic")
   (desc :initform "Randomly insert nodes one by one to best vehicle at best location. Used as initial solution for search algos.")
   (iterations :initform 1)))

(defmethod run-algo ((p problem) (a greedy-best-insertion))
  "Randomly insert <Nodes> one by one to best <vehicle> in best location. Returns <Algo> object when done."
  (let ((insertion-order (shuffle (loop for visits being the hash-values of (problem-visits p) collect (visit-node-id visits)))))
    (loop for node in insertion-order
       do (handler-case
              (perform-move p (get-best-insertion-move p node))
            (no-feasible-move ()
              (if (problem-allow-unserved p)
                  (add-to-unserved p node)
                  (error 'no-initial-feasible-solution :data p))))
       finally (init-algo p a)
         (return a))))
