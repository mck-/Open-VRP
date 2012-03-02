;; Some test instances
;; -----------------------
(in-package :open-vrp.test)

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
(defvar solomon25 
  (load-testcase-solomon (merge-pathnames "test-cases/25-cust.txt" 
					  (asdf:system-source-directory 'open-vrp))))
(defvar solomon100 
  (load-testcase-solomon (merge-pathnames "test-cases/100-cust.txt" 
					  (asdf:system-source-directory 'open-vrp))))
(defvar christofides-1 
  (load-tsplib-vrp-file (merge-pathnames "test-cases/Christofides_01.vrp" 
					 (asdf:system-source-directory 'open-vrp))))
(defvar christofides-2 
  (load-tsplib-vrp-file (merge-pathnames "test-cases/Christofides_02.vrp" 
					 (asdf:system-source-directory 'open-vrp))))


