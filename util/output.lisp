;;; Thu 29 Dec, 2011 (c) Marc Kuo
;;; Output functions
;;; --------------------------
(in-package :open-vrp.output)

;; Print solution
(defgeneric print-routes (solution)
  (:method (solution) "Expects <fleet> or <problem> object!")
  (:documentation "Prints solution given a <fleet>/<problem>/<algo> object. Also prints the total distance when the input is a <problem>/<algo> object."))

(defmethod print-routes ((prob problem))
  (format t "~&---------------")
  (format t "~&Fitness: ~A" (fitness prob))
  (format t "~&---------------")
  (format t "~&~{Route: ~A~^~%~}" (mapcar #'route-indices (get-busy-vehicles prob)))
  (format t "~&---------------"))

(defmethod print-routes ((a algo))
  (print-routes (algo-best-sol a)))

;; multi-run/batch run stats
(defun print-multi-run-stats (solutions)
  (let ((results (mapcar #'algo-best-fitness solutions)))    
    (format t "~&Runs: ~8a~%Max: ~8a~%Min: ~8a~%Avg: ~8a~%Std: ~8a~%"
	    (length results) (get-max results) (get-min results) (mean results) (standard-deviation results))))
