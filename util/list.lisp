;;; Simple Util for list manipulation. Basic tools used by route.lisp mostly.

(in-package :open-vrp.util)

;; Simple list utils
;; --------------------
(defun get-from-list (list pred &key key)
  "Gets from list the value (max or min) while ignoring NIL's. Returns NIL if the whole list is nil. Use get-min or get-max!"
  (let ((in-list (if key (mapcar key list) list)))
    (labels ((iter (ls ans)
	       (if (null ls) ans
		   (iter (cdr ls)
			 (let ((x (car ls)))
			   (cond ((null ans) x)
				 ((null x) ans)
				 ((funcall pred x ans) x)
				 (t ans)))))))
      (iter (cdr in-list) (car in-list)))))

(defun get-min (list &key key)
  "Gets the minimum value from a list, while ignoring the NIL values."
  (get-from-list list #'< :key key))

(defun get-max (list &key key)
  "Gets the maximum value from a list, while ignoring the NIL values."
  (get-from-list list #'> :key key))

(defun get-index-of (list fn &key key)
  "Helper function of the below. Use get-min-index or get-max-index!"
  (aif (funcall fn list :key key)
       (values (position it list :key key) it)
       nil))
  
(defun get-min-index (list &key key)
  "Returns index of the smallest value on list, while ignoring NIL. Returns index and its value (closest node and value)."
  (get-index-of list #'get-min :key key))

(defun get-max-index (list &key key)
  "Returns index of the largest value on list, while ignoring NIL. Returns index and its value (closest node and value)."
  (get-index-of list #'get-max :key key))

(defun sort-ignore-nil (list predicate &key key)
  "Sorts the sequence with #'< or #'> while passing all NIL values towards the end of result."
  (if (find-if-not #'null list :key key)
      (let ((ignore (cond ((eq predicate #'<) (1+ (get-max list :key key)))
			  ((eq predicate #'>) (1- (get-min list :key key)))
			  (t (error 'unaccepted-predicate :pred predicate)))))
	(sort (copy-list list) predicate
	      :key #'(lambda (x) (or (if key (funcall key x) x)
				     ignore))))
      list))

;; --------------------------
  
;; Single route
;; -------------------------

(defun insert-before (object index list)
  "Insert object before index of list. 0 implies inserting in front, length of list implies appending at the end. Throws index out of bounds when index is larger."
  (unless (<= 0 index (length list))
    (error 'index-out-of-bounds :index index :ls list))
  (labels ((iter (obj i ls)
	     (if (= 0 i)
		 (cons obj ls)
		 (cons (car ls)
		       (insert-before obj
				      (1- i)
				      (cdr ls))))))
    (iter object index list)))

(defun insert-at-end (object list)
  "Appends the object at the end of the list"
  (insert-before object (length list) list))

(defun remove-index (index list)
  "Given a list, remove the object on index. Does not accept index out of bounds. Returns the new list AND the object that was removed."
  (unless (< -1 index (length list))
    (error 'index-out-of-bounds :index index :ls list))
  (let ((item))
    (labels ((iter (n lst)
	       (if (= 0 n) (progn (setf item (car lst))
				  (cdr lst))
		   (cons (car lst)
			 (iter (1- n)
			       (cdr lst))))))
      (values (iter index list) item))))

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
