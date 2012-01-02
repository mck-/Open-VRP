;;; Wed Nov 9, 2011 (c) Marc Kuo
;;; Test case for CLOS
;;; --------------
(in-package :open-vrp.data)
;; list of x,y coord pairs. index 0 is base. 

(defvar *node-coords*
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

;; Parameters for problem formulation
(defparameter *fleet-size* 1)
(defparameter *plot-filename* "testing.png")