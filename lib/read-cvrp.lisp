(in-package :open-vrp.util)

(defun %read-string (stream fn)
  (let ((current-char (read-char stream nil nil))
	(result (make-array 0 
			    :element-type 'character
			    :fill-pointer 0
			    :adjustable t)))
    (while (funcall fn current-char)
      (vector-push-extend current-char result)
      (setq current-char (read-char stream)))
    (unread-char current-char stream)
    result))

(defun read-string-while-member (stream lst)
  (%read-string stream (lambda (char) (member char lst))))

(defun read-string-while-not-member (stream lst)
  (%read-string stream (lambda (char) (not (member char lst)))))

(defun load-tsplib-vrp-file (file)
  "Load a subset of the VRP in the TSPLIB. Do not support time windows.
###################
NAME: xxxx
...
DIMENSION: xxx
...
EDGE_WEIGHT_FORMAT: FUNCTION
...
EDGE_WEIGHT_TYPE: EXACT_2D
CAPACITY: xxx
...
NODE_COORD_SECTION
....
DEPOT_SECTION
1
-1
EOF
####################
EDGE_WEIGHT_FORMAT and EDGE_WEIGHT_TYPE are optional"
  (with-open-file (stream file)
    (let (keyword
	  name
	  (capacity 0)
	  (customers 0)
	  (edge-weight-format (intern "FUNCTION"))
	  (edge-weight-type (intern "EXACT_2D"))
	  x-coords 
	  y-coords 
	  demands)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; specification section ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (tagbody 
       :top
	 (setq keyword (read-string-while-not-member stream '(#\: #\Space #\Tab #\Return #\Linefeed)))
	 (when (string-equal keyword "NODE_COORD_SECTION") (go :down))
	 (read-string-while-member stream '(#\Space #\: #\Tab))
	 (cond 
	   ((string-equal keyword "NAME") (setf name (string (read stream))))
	   ((string-equal keyword "DIMENSION") (setf customers (read stream)))
	   ((string-equal keyword "CAPACITY") (setf capacity (read stream)))
	   (t (read stream nil nil)))
	 (read-char stream nil nil)
	 (go :top)
       :down)

      (when (not (eq (intern "FUNCTION") edge-weight-format))
      	(error "EDGE_WEIGHT_FORMAT must be FUNCTION"))
      (when (not (eq (intern "EXACT_2D") edge-weight-type))
      	(error "EDGE_WEIGHT_TYPE must be EXACT_2D"))

      ;;;;;;;;;;;;;;;;;;
      ;; data section ;;
      ;;;;;;;;;;;;;;;;;;
      (loop
	 while (not (eq  (intern "DEMAND_SECTION") (read stream)))
	 collect (read stream) into x
	 collect (read stream) into y
	 finally (setf x-coords x y-coords y))
      (when (/= (length x-coords) customers)
      	(error "The number of customers is incorrect"))
      (loop 
	 while (not (eq (intern "DEPOT_SECTION") (read stream)))
	 collect (read stream) into d
	 finally (setf demands d))
      (when (/= (length demands) customers)
      	(error "The demand size is incorrect"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; check depot section and eof keyword ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (/= 1 (read stream))
      	(error "The number of depots must be 1"))
      (when (/= -1 (read stream))
      	(error "DEPOT_SECTION must be terminated by a -1"))
      (when (not (eq (intern "EOF") (read stream)))
      	(error "The file must be terminated by EOF"))

      (define-problem  name
      	               (couple-lists x-coords y-coords) 
      	               25 
      		       :demands demands
      		       :capacities capacity))))




