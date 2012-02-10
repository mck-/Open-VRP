;;; Creating objects macros
;;; - create-nodes
;;; - create-vehicles
;;; - define-problem

(in-package :open-vrp.util)

;; Initialising Drawer object functions
;; ---------------------------
(defun get-min-coord (node-coords)
  (reduce #'min (flatten node-coords)))

(defun get-max-coord (node-coords)
  (reduce #'max (flatten node-coords)))
;; ---------------------------

;; Create network
;; ----------------------------
(defmacro create-nodes (node-coords &key demands time-windows durations)
  "Given a coord-list, return a vector of nodes. The nodes are created and numbered starting from 0, which is the base node. For additional parameters accept a list with the same length as node-coords, in which each element specifies the node attributes."
  (with-gensyms (nodes id coords demand tw dur ln)
    `(let ((,ln (length ,node-coords)))
       ;; Checking if input attributes' length is equal to node-coords' length
       (when (and ,demands (not (= (length ,demands) ,ln)))
	 (error 'not-equal-length :list1 ,node-coords :list2 ,demands))
       (when (and ,time-windows (not (= (length ,time-windows) ,ln)))
	 (error 'not-equal-length :list1 ,node-coords :list2 ,time-windows))
       (when (and ,durations (not (= (length ,durations) ,ln)))
	 (error 'not-equal-length :list1 ,node-coords :list2 ,durations))
       
       (loop with ,nodes = (make-array ,ln :fill-pointer 0) ;vector of nodes
	for
	  ,coords in ,node-coords
	and ,id from 0
	  ,@(when demands `(and ,demand in ,demands))
	  ,@(when time-windows `(and ,tw in ,time-windows))
	  ,@(when durations `(and ,dur in ,durations))
	do
	  (vector-push
	   (make-instance 'node
			  :id ,id
			  :xcor (car ,coords)
			  :ycor (cdr ,coords)
			  ,@(when demands `(:demand ,demand))
			  ,@(when time-windows `(:start (car ,tw) :end (cdr ,tw)))
			  ,@(when durations `(:duration ,dur)))
	   ,nodes)
	finally (return ,nodes)))))
  
;; Create fleet
;; --------------------------
(defmacro create-vehicles (fleet-size base-node to-depot &key capacities speeds)
  "Returns a list of vehicles, starting with ID 0. The starting location of their routes are all initialized at base-node. When to-depot is set to T, initialize their routes with 2 base nodes (departure and destination). For capacities and speeds, accepts a list that is of equal lenght to fleet-size. A shorter list will return a smaller fleet."
  (with-gensyms (id capacity speed)
    `(progn
       ;; Checking if input attributes' length is equal to fleet-size
       (when (and ,capacities (not (= (length ,capacities) ,fleet-size)))
	 (error 'not-equal-length :list1 ,fleet-size :list2 ,capacities))
       (when (and ,speeds (not (= (length ,speeds) ,fleet-size)))
	 (error 'not-equal-length :list1 ,fleet-size :list2 ,speeds))

       (loop for ,id from 0 to (1- ,fleet-size)
	    ,@(when capacities `(and ,capacity in ,capacities))
	    ,@(when speeds `(and ,speed in ,speeds))
	  collect
	    (new-vehicle ,id ,base-node ,to-depot
			 ,@(when capacities `(:capacity ,capacity))
			 ,@(when speeds `(:speed ,speed)))))))
;; -----------------------------

;; Create Problem macro
;; ----------------------------

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