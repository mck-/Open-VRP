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
(defun same-lengthp(&rest lists)
  "Returns NIL if lists are not of equal length. Returns their length if all are equal. Accepts NIL arguments or single numbers, which will be ignored. Also accepts 2-dimensional arrays."
  (let ((pruned-list (remove-if-not #'consp
				    (mapcar #'(lambda (x)
						(if (arrayp x) (2d-array-to-list x) x))
					    lists))))
    (labels ((iter (ls len)
	       (if (null ls) len 
		   (and (= len (length (car ls)))
			(iter (cdr ls) len)))))
      (iter (cdr pruned-list) (length (car pruned-list))))))

(defmacro create-nodes (&key node-coords demands time-windows durations dist-matrix)
  "Given a coord-list, return a vector of nodes. The nodes are created and numbered starting from 0, which is the base node. For additional parameters accept a list with the same length as node-coords, in which each element specifies the node attributes. Note that dist-matrix parameter is not used, but only to create nodes when no other parameter is passed (called from define-problem)."
  (with-gensyms (nodes id coords demand tw dur ln)
    ;; Checking if input attributes' length is equal to node-coords' length
    `(let ((,ln (same-lengthp ,node-coords ,demands ,time-windows ,durations ,dist-matrix)))
       (unless ,ln (error 'not-equal-length
			  :lists (list ,node-coords ,demands ,time-windows ,durations ,dist-matrix)))
       (loop with ,nodes = (make-array ,ln :fill-pointer 0) ;vector of nodes
	  for ,id from 0 to ,ln
	  ,@(when node-coords `(and ,coords in ,node-coords))
	  ,@(when demands `(and ,demand in ,demands))
	  ,@(when time-windows `(and ,tw in ,time-windows))
	  ,@(when durations `(and ,dur in ,durations))
	  do
	  (vector-push
	   (make-node
	    :id ,id
	    ,@(when node-coords `(:xcor (car ,coords) :ycor (cdr ,coords)))
	    ,@(when demands `(:demand ,demand))
	    ,@(when time-windows `(:start (car ,tw) :end (cdr ,tw)))
	    ,@(when durations `(:duration ,dur)))
	   ,nodes)
	  finally (return ,nodes)))))
  
;; Create fleet
;; --------------------------
(defmacro create-vehicles (fleet-size base-node to-depot &key capacities speeds)
  "Returns a list of vehicles, starting with ID 0. The starting location of their routes are all initialized at base-node. When to-depot is set to T, initialize their routes with 2 base nodes (departure and destination). For capacities and speeds, only accepts a list that is of equal lenght to fleet-size."
  (with-gensyms (id capacity speed)
    `(progn
       ;; Checking if input attributes' length is equal to fleet-size
       (when (and ,capacities (not (= (length ,capacities) ,fleet-size)))
	 (error 'not-equal-length :lists (list ,fleet-size ,capacities)))
       (when (and ,speeds (not (= (length ,speeds) ,fleet-size)))
	 (error 'not-equal-length :lists (list ,fleet-size ,speeds)))

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

(defmacro define-problem (name fleet-size &key node-coords-list demands capacities time-windows-list durations speeds (to-depot T) plot-filename log-filename dist-matrix)
  "Creates the appropriate <Problem> object from the inputs. Extra key attributes only accept lists that are of equal length to node-coords-list or fleet-size (depending on what attributes it sets).

A(n asymmetric) dist-matrix may be passed, instead of node-coords, in which case plotting will be disabled. dist-matrix must be a square list of lists or 2-dimensional array.

For demands, durations, capacities and speeds, will also accept a single value, which will set all attributes to this value.

With only the demands-list and capacities, creates a CVRP problem. With time-windows, creates a VRPTW problem. When durations and speeds are not provided, defaults to 0 and 1.  When plot-filename is not given, it will plot in \"plots/name.png\"."
  (with-gensyms (ln network fleet drawer)
    `(let ((,ln (same-lengthp ,demands ,node-coords-list ,time-windows-list ,durations
;			      (if (arrayp ,dist-matrix)
;				  (2d-array-to-list ,dist-matrix)
				  ,dist-matrix)))
       (unless ,ln (error 'not-equal-length :lists (list ,demands ,node-coords-list ,time-windows-list ,durations ,dist-matrix)))
       (let* ((,network (create-nodes ,@(when node-coords-list
					      `(:node-coords ,node-coords-list))
				      ,@(when demands 
					      `(:demands
						(if (listp ,demands)
						    ,demands
						    (make-list ,ln
							       :initial-element ,demands))))
				      ,@(when time-windows-list
					      `(:time-windows ,time-windows-list))
				      ,@(when durations 
					      `(:durations
						(if (listp ,durations)
						    ,durations
						    (make-list ,ln
							       :initial-element ,durations))))
				      ,@(when dist-matrix
					      `(:dist-matrix ,dist-matrix))))
	      (,fleet (create-vehicles ,fleet-size (if (= 0 (length ,network))
						       (error 'empty-network)
						       (aref ,network 0)) ,to-depot
						       ,@(when capacities 
							       `(:capacities
								 (if (listp ,capacities)
								     ,capacities
								     (make-list ,fleet-size 
										:initial-element ,capacities))))
						       ,@(when speeds
							       `(:speeds
								 (if (listp ,speeds)
								     ,speeds
								     (make-list ,fleet-size 
										:initial-element ,speeds))))))
	      ,@(when node-coords-list
		      `((,drawer (make-instance 'drawer
						:min-coord (get-min-coord ,node-coords-list)
						:max-coord (get-max-coord ,node-coords-list)
						:filename (if ,plot-filename ,plot-filename
							      (merge-pathnames (concatenate 'string "plots/" (string ,name) ".png")
									       (asdf:system-source-directory 'open-vrp))))))))
	 (format t "~&Processed ~A nodes succesfully for ~A" ,ln ,name)
	 ,@(unless node-coords-list
		   `((warn "No coords: Plotting function disabled.")))
	 ,@(unless (or node-coords-list dist-matrix)
		   `((warn "No dist-matrix: Must set dist-matrix with #'set-dist-matrix first!")))
	 (make-instance ,@(cond ((and time-windows-list capacities) '('cvrptw))
				(time-windows-list '('vrptw))
				((and demands capacities) '('cvrp))
				(t '('problem)))
			:name (string ,name)
			:fleet ,fleet
			:network ,network
					; if dist-matrix is a list, convert it to an array
			,@(if dist-matrix
			      `(:dist-array
				(if (listp ,dist-matrix)
				    (2d-list-to-array ,dist-matrix)
				    ,dist-matrix))
			      (when node-coords-list ; can only generate dist-matrix with coords
				`(:dist-array (generate-dist-array ,node-coords-list))))
		      
			:to-depot ,to-depot
			,@(when node-coords-list `(:drawer ,drawer))
			:log-file (if ,log-filename ,log-filename
				      (merge-pathnames (concatenate 'string "run-logs/" (string ,name) ".txt")
						       (asdf:system-source-directory 'open-vrp))))))))



