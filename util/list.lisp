;;; Simple Util for list manipulation. Basic tools used by route.lisp mostly.

(in-package :open-vrp.util)

;; Simple list utils
;; --------------------
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

(defmacro sort-ignore-nil (sequence predicate &key key)
  "Sorts the sequence with #'< or #'> while passing all NIL values towards the end of result."
  (let ((list (gensym))
	(ignore (gensym)))
    `(let* ((,list ,(if key `(mapcar ,key ,sequence) sequence))
	    (,ignore (if (eq ,predicate #'<)
			  (1+ (or (get-max ,list) 0))
			  (1- (or (get-min ,list) 0)))))
       (sort (copy-list ,sequence)
	     ,predicate
	     :key #'(lambda (x) (or ,(if key `(funcall ,key x) `x)
				    ,ignore))))))

;; --------------------------
  
;; Single route
;; -------------------------

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

(defun mark-nill (list indices)
  "Marks the indices on list with NIL. DESTRUCTIVE."
  (mapcar #'(lambda (x) (setf (nth x list) nil)) indices)
  list)
   
(defmacro with-tabu-indices (tabu-indices fn arg-list)
  `(funcall ,fn (mark-nill (copy-list ,arg-list) ,tabu-indices)))

(defun enumerate-interval (n)
  "Returns a list from 1 to n."
  (map1-n #'(lambda (x) x) n))

(defun random-list-permutation (length)
  "Randomly creates a permutation from 1 to length."
  (shuffle (enumerate-interval length)))
