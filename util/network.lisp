;;; Mon Jun 20, 2011 (c) Marc Kuo
;;; Utilities for generating a distance table using a list of node coords.
;;; -----------------------------------------
;;; 1. create-network generates/initialises the <network> object (using node-coords)
;;; 2. read from dist-table with (distance i j)
(in-package :open-vrp.util)

(defun distance (i j dist-table)
  "Read from the distance-table with two indices."
  (nth j (nth i dist-table)))

(defun distance-coords (x1 y1 x2 y2)
  "Calculates pythagoras distance"
  (flet ((square (x)
	   (* x x)))
    (sqrt (+ (square (- x1 x2)) (square (- y1 y2))))))

(defgeneric node-distance (node1 node2)
  (:method (node1 node) "Inputs are not two nodes.")
  (:documentation "Given two node objects, calculate and return their distance (Cartesian)."))

(defmethod node-distance ((n1 node) (n2 node))
  (let ((x1 (node-xcor n1)) (y1 (node-ycor n1))
	(x2 (node-xcor n2)) (y2 (node-ycor n2)))
    (distance-coords x1 y1 x2 y2)))

;; Function for generating dist-table
;; ---------------------------------------
(defun generate-dist-table (coord-list)
  "Given a list of coord pairs, generate a matrix of distances."
  (flet ((helper (n1 n2)
	   "Calculates distance given two coord pairs. Returns NIL if both coords are the same."
	   (if (eql n1 n2)
	       NIL
	       (distance-coords (car n1) (cdr n1)
				(car n2) (cdr n2)))))	   
    (mapcar #'(lambda (node)
		(mapcar #'(lambda (node2)
			    (helper node node2))
			coord-list))
	    coord-list)))

;; ----------------------------------------

;; Accessor functions
;;--------------------------
;; Thu Nov 10, 2011

(defgeneric node (obj id)
  (:method (obj id) "This node method only accepts a <network> or <problem> object.")
  (:documentation "Returns <Node> object, given a <network>/<problem> and a node ID (int)."))

(defmethod node ((net network) id) 
  (nth id (network-nodes net)))

(defmethod node ((prob problem) id)
  (node (problem-network prob) id))

(defgeneric coords (object)
  (:method (object)
    "No coords can be returned from this object type. Or object is non-existent, e.g. when index is out of bounds.")
  (:documentation "Returns the coords of the nodes from the object. Route in vehicle, whole network if network object, and one cons if object is a node."))

(defmethod coords ((n node))
  (cons (node-xcor n) (node-ycor n)))
    
(defmethod coords ((net network))
  (mapcar #'(lambda (node) (cons (node-xcor node) (node-ycor node))) (network-nodes net)))

(defmethod coords ((prob problem))
  (coords (problem-network prob)))

(defmethod coords ((veh vehicle))
  (mapcar #'(lambda (node) (cons (node-xcor node) (node-ycor node))) (vehicle-route veh)))

(defgeneric dist-table (object)
  (:documentation "Returns the dist-matrix; a precalculated distance table to all nodes."))

(defmethod dist-table ((net network))
  (network-dist-table net))

(defmethod dist-table ((prob problem))
  (dist-table (problem-network prob)))
;; --------------------------------

;; Initializing functions
;; ------------------

(defun create-node-objects (node-coords &optional (type 'node))
  "Given a coord-list (which is a list of x,y pairs), return list of Node objects, starting with ID 0."
  (let ((node-list nil))
    (do ((i 0 (1+ i)) ;id generator
	 (coords-list node-coords (cdr coords-list))) ;iterate over the list
	((null (car coords-list)))
      (push (make-instance type :id i :xcor (caar coords-list) :ycor (cdar coords-list)) node-list))
    (nreverse node-list)))
    
(defun create-network (node-coords &key (type 'node))
  "Given a coord-list, return an instance of class 'network, with nodes and dist-table initialised. Dist-table is a precalculated Cartesian distance matrix, for quick table-lookup. The nodes are created and numbered starting from 0, which is the base node. Reads by default *node-coords*. Use :type to choose what types of nodes will be generated."
;Thu Dec 29, 2011 - type may not be useful, since more args are required for e.g. TW-nodes
  (let ((nodes (create-node-objects node-coords type))
	(dist-table (generate-dist-table node-coords)))
    (make-instance 'network :dist-table dist-table :nodes nodes)))