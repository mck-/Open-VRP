;; Thu Nov 10, 2011 (c) Marc Kuo
;; Some test instances
;; -----------------------
(in-package :open-vrp)
;; Initialization of objects
(defvar test-tsp (define-problem 'tsp *node-coords* 1 "test-tsp.png"))
(defvar test-vrp (define-problem 'vrp *node-coords* 2 "test-vrp.png"))