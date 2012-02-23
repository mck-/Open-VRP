;;; Output functions
;;; --------------------------
(in-package :open-vrp.util)

(defgeneric print-routes (solution)
  (:method (solution) "Expects <fleet>/<problem>/<algo> object!")
  (:documentation "Prints solution given a <fleet>/<problem>/<algo> object. Also prints the total distance when the input is a <problem>/<algo> object."))

(defmethod print-routes ((prob problem))
  (format t "~&---------------")
  (format t "~&Fitness: ~A" (fitness prob))
  (format t "~&---------------")
  (dolist (busy-veh (get-busy-vehicles prob))    
    (format t "~&[~2D]: ~A~%" (vehicle-ID busy-veh) (route-indices busy-veh)))
  (format t "~&---------------"))

(defmethod print-routes ((a algo))
  (print-routes (algo-best-sol a)))

(defun print-multi-run-stats (algo-objects)
  "Given a list of algo-objects returned by multi-run, print run-stats."
  (let ((results (mapcar #'algo-best-fitness algo-objects)))    
    (format t "~&Runs: ~8a~%Max: ~8a~%Min: ~8a~%Avg: ~8a~%Std: ~8a~%"
	    (length results) (get-max results) (get-min results) (mean results) (standard-deviation results))))
