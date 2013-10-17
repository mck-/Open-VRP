;;; Utilities for generating a distance table using a list of node coords.
;;; -----------------------------------------
;;; - distance (int int array)		- Expects two node-IDs and a dist-array
;;; - node (<Problem> int)		- Returns <Node> given a <Problem> and a node-id
;;; - new-node				- Macro that creates a <Node> according to input
;;; -----------------------------------------
;;; Distance matrix data-structure: Hash-table of hash-tables
;;; -----------------------------------------
;;;

(in-package :open-vrp.util)
;(proclaim '(optimize (speed 3)))

(defun sethash (key val hash-table)
  "Setter for hash-table"
  (check-type hash-table hash-table)
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
  (check-type from symbol)
  (check-type to symbol)
  (check-type dist-matrix hash-table)
  (when (eq from to) (error 'same-origin-destination :from from :to to))
  (let ((row (gethash from dist-matrix)))
    (unless row (error 'distance-between-nodes-undefined :from from :to to))
    (check-type row hash-table)
    (aif (gethash to row)
         it
         (error 'distance-between-nodes-undefined :from from :to to))))

(defun get-distance (from-id to-id matrix)
  "Helper to distance, which will return distance 0 for same from and to condition"
  (handler-case (distance from-id to-id matrix)
    (same-origin-destination () 0)))

;; -------------------------

;; Accessor functions
;;--------------------------

(defmethod node ((prob problem) id)
  (gethash id (problem-network prob)))

(defmethod visit-node ((prob problem) id)
  (gethash id (problem-visits prob)))

;; -------------------------
