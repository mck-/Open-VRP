;;; Updated Tue Oct 25, 2011 (c) Marc Kuo
;;; ------------------------
;;; Simple Util for list manipulation. Used on simple routes, i.e. permutation of numbers that represent nodes.
(in-package :open-vrp.util)

;; Simple list utils
(defun get-from-list (list pred &optional ans)
  "Gets from list the value (max or min) while ignoring NIL's. Returns NIL if the whole list is nil. Use get-min or get-max!"
  (if (null list) (if (numberp ans) ans nil)
      (get-from-list (cdr list)
		     pred
		     (cond ((null ans) (car list))
			   ((not (numberp (car list))) ans)
			   ((funcall pred (car list) ans) (car list))
			   (t ans)))))
(defun get-min (list)
  "Gets the minimum value from a list, while ignoring the NIL values."
  (get-from-list list #'<))

(defun get-max (list)
  "Gets the maximum value from a list, while ignoring the NIL values."
  (get-from-list list #'>))

(defun get-index-of (list fn)
  "Helper function of the below. Use get-min-index or get-max-index!"
  (aif (funcall fn list)
       (values (position it list) it)
       nil))
  
(defun get-min-index (list)
  "Returns index of the smallest value on list, while ignoring NIL. Returns index and its value (closest node and value)."
  (get-index-of list #'get-min))

(defun get-max-index (list)
  "Returns index of the largest value on list, while ignoring NIL. Returns index and its value (closest node and value)."
  (get-index-of list #'get-max))

;; Single route
;; Sun Dec 11, 2011 - Adjusted to not allow for index-out-of-bounds
(defun insert-before (object index list)
  "Insert object before index of list. 0 implies inserting in front, length of list implies appending at the end. Throws index out of bounds when index is larger."
  (labels ((iter (obj i ls)
	     (if (= 0 i)
		 (cons obj ls)
		 (cons (car ls)
		       (insert-before obj
				      (1- i)
				      (cdr ls))))))
    (if (> index (length list))
	(error "Index out of bounds! Cannot insert-before at index larger than length of list")
	(iter object index list))))

(defun insert-at-end (object list)
  "Appends the object at the end of the list"
  (insert-before object (length list) list))

(defun remove-index (index list)
  "Given a list, remove the object on index. Does not accept index out of bounds. Returns the new list AND the object that was removed."
  (if (>= index (length list)) (error "Index is out of bound of list, cannot remove nil")
      (let ((item))
	(labels ((iter (n lst)
		   (if (= 0 n) (progn (setf item (car lst))
				      (cdr lst))
		       (cons (car lst)
			     (iter (1- n)
				   (cdr lst))))))
	  (values (iter index list) item)))))

(defmacro apply-on-index (list index function)
  "Macro to apply function only on index in list. NON-destructive. Accepts a function with one argument which will use as input the nth of the list. Returns a list."
  `(if (>= ,index (length ,list)) (error "Index is larger than index!")
       (labels ((iter-fn (i rest)
		  (if (= -1 i) rest
		      (cons (if (= 0 i) (funcall ,function (car rest)) (car rest))
			    (iter-fn (1- i) (cdr rest))))))
	 (iter-fn ,index ,list))))

(defun mark-nill (list indices)
  "Marks the indices on list with NIL. DESTRUCTIVE."
  (mapcar #'(lambda (x) (setf (nth x list) nil)) indices)
  list)

(defun mark-nill-table (table x y)
  "Given 2-coords, mark the cell NIL. DESTRUCTIVE."
  (setf (nth y (nth x table)) nil)
  table)

(defun mark-nill-items (list items)
  "Marks the items on list with NIL."
  (labels ((iter (rest-items ans)
	     (if (null rest-items) ans
		 (iter (cdr rest-items)
		       (substitute nil (car rest-items) ans)))))
    (iter items list)))
   
(defmacro with-tabu-indices (tabu-indices fn arg-list)
  `(funcall ,fn (mark-nill (copy-list ,arg-list) ,tabu-indices)))

(defmacro with-tabu-items (tabu-items fn arg-list)
  `(funcall ,fn (mark-nill-items (copy-list ,arg-list) ,tabu-items)))

(defun enumerate-interval (x)
  "Returns a list from 1 to x."
  (if (= 0 x)
      nil
      (append (enumerate-interval (1- x))
	      (list x))))

(defun shuffle-pool (pool)
  "Given a pool, shuffle the pool and return a new pool. A pool is a list of any object."
  (if (null pool)
      nil
      (let* ((len (length pool))
	     (x (random len))
	     (val (nth x pool)))
	(cons val
	      (shuffle-pool (remove val pool))))))

(defun random-list-permutation (length)
  "Randomly creates a permutation from 1 to length."
  (shuffle-pool (enumerate-interval length)))
