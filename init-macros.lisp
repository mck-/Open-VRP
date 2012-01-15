;;; Sat 31, 2011 (c) Marc Kuo
;;; -------------
;;; Problem building macros
;;; Used for quickly defining problems, requiring only a list of node-coords
;;; Needs adjustment when extended with advanced nodes! (e.g. time-windows, load)
;;; Think of a different approach!
(in-package :open-vrp)

;; Initialising Drawer object functions
;; ---------------------------
(defun get-min-coord (node-coords)
  (reduce #'min (flatten node-coords)))

(defun get-max-coord (node-coords)
  (reduce #'max (flatten node-coords)))
;; ---------------------------
  
;; 
(defmacro define-problem (name node-coords-list fleet-size plot-filename &optional demands-list capacity time-windows-list duration-list speeds)
  "Creates a <Problem> object from the inputs. When fleet-size is 1 (and no optional arguments), creates a TSP problem. When fleet-size is more than 1, creates a VRP problem. With only the demands-list and capacity, creates a CVRP problem. With time-windows and durations, creates a VRPTW problem."
  `(let* ((network (create-network ,node-coords-list
				   ,@(when demands-list `(,demands-list))
				   ,@(when time-windows-list `(,time-windows-list))
				   ,@(when duration-list `(,duration-list))))
	  (fleet (create-fleet ,fleet-size network
			       ,@(when capacity `(,capacity))
			       ,@(when duration-list `((or ,speeds 1))))) ;default speed of 1
	  (drawer (make-instance 'drawer
				 :min-coord (get-min-coord ,node-coords-list)
				 :max-coord (get-max-coord ,node-coords-list)
				 :filename ,plot-filename)))
     (make-instance ,@(cond (time-windows-list '('vrptw))
			    (capacity '('cvrp))
			    ((> fleet-size 1) '('vrp))
			    (t '('tsp)))
		    :name ,name :fleet fleet :network network :drawer drawer)))
	  
	   
