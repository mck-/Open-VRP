;;; Thu 29 Dec, 2011 (c) Marc Kuo
;;; Output functions
;;; --------------------------
(in-package :open-vrp.output)

;; Print solution
(defgeneric print-routes (solution)
  (:method (solution) "Expects <fleet> or <problem> object!")
  (:documentation "Prints solution given a <fleet>/<problem>/<algo> object. Also prints the total distance when the input is a <problem>/<algo> object."))

;; ignore empty vehicles
(defmethod print-routes ((f fleet))
  (format t "~&~{Route: ~A~^~%~}"
	  (remove-if #'(lambda (route) (null (cdr route))) (route-indices f)))
  (format t "~&---------------"))

(defmethod print-routes ((prob problem))
  (format t "~&---------------")
  (format t "~&Fitness: ~A" (fitness prob))
  (format t "~&---------------")
  (print-routes (problem-fleet prob)))

(defmethod print-routes ((a algo))
  (print-routes (algo-best-sol a)))

;; multi-run/batch run stats
(defun print-multi-run-stats (solutions)
  (let ((results (mapcar #'algo-best-fitness solutions)))
    (labels ((average (list)
	       (/ (apply #'+ list) (length list)))
	     (square (x) (* x x))
	     (stdv (list)
	       (sqrt
		(sum
		 (mapcar #'(lambda (x) (square (- x (average list)))) list)))))
      (format t "~&Runs: ~8a~%Max: ~8a~%Min: ~8a~%Avg: ~8a~%Std: ~8a~%"
	      (length results) (get-max results) (get-min results) (average results) (stdv results)))))
