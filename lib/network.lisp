;;; Utilities for generating a distance table using a list of node coords.
;;; -----------------------------------------
;;; - distance (int int array)		- Expects two node-IDs and a dist-array
;;; - node-distance (<Node> <Node>)	- Calculates distance between two <Node> objects
;;; - node (<Problem> int)		- Returns <Node> given a <Problem> and a node-id
;;; - generate-dist-array (coord-list)	- Returns array of distances
;;; - new-node				- Macro that creates a <Node> according to input
;;; -----------------------------------------
;;; Distance matrix data-structure: Hash-table of hash-tables
;;; -----------------------------------------
;;;

(in-package :open-vrp.util)
;(proclaim '(optimize (speed 3)))

(defun sethash (key val hash-table)
  "Setter for hash-table"
  (setf (gethash key hash-table) val))

(defun alist-to-hash (alist)
  "Given an alist matrix, convert it into a hash table"
  (let ((matrix (make-hash-table)))
    (dolist (row alist)
      (sethash (first row) (make-hash-table) matrix)
      (dolist (col (rest row))
        (sethash (car col) (cdr col) (gethash (first row) matrix))))
    matrix))

(defun distance (from to dist-matrix)
  "Read from the distance-matrix with two keys (location IDs). Expects dist-matrix to be a hash table of hash tables."
  (when (eq from to) (error 'same-origin-destination :from from :to to))
  (unless (and (eq (type-of from) 'keyword)
               (eq (type-of to) 'keyword))
    (error 'expect-keyword-arguments :fun 'distance :arg (list from to)))
  (unless (eq (type-of dist-matrix) 'hash-table)
    (error 'expect-hash-table :fun 'distance :arg dist-matrix))
  (let ((row (gethash from dist-matrix)))
    (if (eq (type-of row) 'hash-table)
        (gethash to row)
        (error 'expect-hash-table :fun 'distance :arg row))))

(defun distance-coords (x1 y1 x2 y2)
  "Calculates pythagoras distance"
  (flet ((square (x)
           (* x x)))
    (sqrt (+ (square (- x1 x2)) (square (- y1 y2))))))

(defun distance-coord-pair (n1 n2)
  "Calculates distance given two coord pairs. Returns NIL if both coords are the same."
  (if (eql n1 n2)
      NIL
      (distance-coords (car n1) (cdr n1)
                       (car n2) (cdr n2))))

(defun generate-dist-array (coord-list)
  "Given a list of coord pairs, generate an array of distances."
  (let* ((size (length coord-list))
         (dist-array (eval `(make-array '(,size ,size) :initial-element nil))))
    (map0-n #'(lambda (x)
                (map0-n #'(lambda (y)
                            (setf (aref dist-array x y)
                                  (distance-coord-pair (nth x coord-list)
                                                       (nth y coord-list))))
                        (1- size)))
            (1- size))
    dist-array))

;; ----------------------------------------

;; Accessor functions
;;--------------------------

(defmethod node ((prob problem) id)
  (aref (problem-network prob) id))

;; --------------------------------

;; Create Node macro
;; ------------------
(defmacro new-node (id xcor ycor &key demand start end duration)
  `(make-node :id ,id :xcor ,xcor :ycor ,ycor
              ,@(when demand `(:demand ,demand))
              ,@(when start `(:start ,start))
              ,@(when end `(:end ,end))
              ,@(when duration `(:duration ,duration))))
