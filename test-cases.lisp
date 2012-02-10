;; Some test instances
;; -----------------------
(in-package :open-vrp)

(defparameter *node-coords*
  (list (cons 0 0)
	(cons 1 2)
	(cons 1 3)
	(cons 2 4)
	(cons 3 5)
	(cons 4 4)
	(cons 4 -2)
	(cons 3 -5)
	(cons 2 -3)
	(cons -2 -2)))

;; Initialization of objects
(defvar test-tsp (define-problem "test-case-TSP" *node-coords* 1 :to-depot nil))
(defvar test-vrp (define-problem "test-case-VRP" *node-coords* 2))
(defvar solomon25 (load-testcase-solomon "test-cases/25-cust.txt"))
(defvar solomon100 (load-testcase-solomon "test-cases/100-cust.txt"))