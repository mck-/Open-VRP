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
(defvar test-tsp (define-problem "test-case-TSP" 1 :node-coords-list *node-coords* :to-depot nil :plotp nil))
(defvar test-vrp (define-problem "test-case-VRP" 2 :node-coords-list *node-coords*))
(defvar solomon25 
  (load-test-case-file (merge-pathnames "test-cases/25-cust.txt" 
					  (asdf:system-source-directory 'open-vrp))))
(defvar solomon100 
  (load-test-case-file (merge-pathnames "test-cases/100-cust.txt" 
					  (asdf:system-source-directory 'open-vrp))))
(defvar christofides-1 
  (load-test-case-file (merge-pathnames "test-cases/Christofides_01.vrp" 
					 (asdf:system-source-directory 'open-vrp))))
(defvar christofides-2 
  (load-test-case-file (merge-pathnames "test-cases/Christofides_02.vrp" 
					 (asdf:system-source-directory 'open-vrp))))


