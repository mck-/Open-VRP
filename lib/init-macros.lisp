;;; Define problem macro

(in-package :open-vrp)

;; Initialising Drawer object functions
;; ---------------------------
(defun get-min-coord (node-coords)
  (reduce #'min (flatten node-coords)))

(defun get-max-coord (node-coords)
  (reduce #'max (flatten node-coords)))
;; ---------------------------
  
;; 
(defmacro define-problem (name node-coords-list fleet-size to-depot &key demands capacities time-windows-list durations speeds plot-filename)
  "Creates the appropriate <Problem> object from the inputs. Extra key attributes only accept lists that are of equal length to node-coords-list or fleet-size (depending on what attributes it sets). With only the demands-list and capacities, creates a CVRP problem. With time-windows, creates a VRPTW problem. When durations and speeds are not provided, defaults to 0 and 1.  When plot-filename is not given, it will plot in \"plots/name.png\"."
  (with-gensyms (network fleet drawer)
    `(let* ((,network (create-nodes ,node-coords-list
				    ,@(when demands `(:demands ,demands))
				    ,@(when time-windows-list `(:time-windows ,time-windows-list))
				    ,@(when durations `(:durations ,durations))))
	    (,fleet (create-vehicles ,fleet-size (aref ,network 0) ,to-depot
				     ,@(when capacities `(:capacities ,capacities))
				     ,@(when speeds `(:speeds ,speeds)))) 
	    (,drawer (make-instance 'drawer
				    :min-coord (get-min-coord ,node-coords-list)
				    :max-coord (get-max-coord ,node-coords-list)
				    :filename ,(if plot-filename plot-filename
						   (concatenate 'string "plots/" (string name) ".png")))))
       (make-instance ,@(cond ((and time-windows-list capacities) '('cvrptw))
			      (time-windows-list '('vrptw))
			      ((and demands capacities) '('cvrp))
			      (t '('problem)))
		      :name ,name :fleet ,fleet :network ,network :dist-array (generate-dist-array ,node-coords-list) :to-depot ,to-depot :drawer ,drawer))))