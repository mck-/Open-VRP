;;; Sat 31, 2011 (c) Marc Kuo
;;; -------------
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
(defmacro define-problem (name node-coords-list fleet-size plot-filename to-depot &optional demands-list capacity time-windows-list duration-list speeds)
  "Creates a <Problem> object from the inputs. When fleet-size is 1 (and no optional arguments), creates a TSP problem. When fleet-size is more than 1, creates a VRP problem. With only the demands-list and capacity, creates a CVRP problem. With time-windows and durations, creates a VRPTW problem."
  (with-gensyms (network fleet drawer)
    `(let* ((,network (create-nodes ,node-coords-list
				   ,@(when demands-list `(,demands-list))
				   ,@(when time-windows-list `(,time-windows-list))
				   ,@(when duration-list `(,duration-list))))
	    (,fleet (create-vehicles ,fleet-size ,network ,to-depot
				 ,@(when capacity `(,capacity))
				 ,@(when duration-list `((or ,speeds 1))))) ;default speed of 1
	    (,drawer (make-instance 'drawer
				   :min-coord (get-min-coord ,node-coords-list)
				   :max-coord (get-max-coord ,node-coords-list)
				   :filename ,plot-filename)))
       (make-instance ,@(cond (time-windows-list '('vrptw))
			      (capacity '('cvrp))
			      (t '('problem)))
		      :name ,name :fleet ,fleet :network ,network :dist-array (generate-dist-array ,node-coords-list) :to-depot ,to-depot :drawer ,drawer))))
	  
	   
