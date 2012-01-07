;;; Updated Tue Oct 25, 2011 (c) Marc Kuo
;;; ------------------------
(in-package :open-vrp.util)

;; Simple utils from Paul Graham's Onlisp
;; -------------------------------

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun mapa-b (fn a b &optional (step 1))
  "Maps function over integers from a to b"
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  "maps from 0 to n"
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  "maps from 1 to n"
  (mapa-b fn 1 n))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun flatten (x)
  "Returns a list of all the elements in a tree. Flat."
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

;; ----------------------------------------------------------

(defun max-car (list)
  "Provided a list, return the maximum value considering the cars"
  (reduce #'max list :key #'car))

(defun max-cdr (list)
  "Provided a list, return the maximum value considering the cdrs"
  (reduce #'max list :key #'cdr))

;; Tue Nov 29, 2011
;; quick ugly tsp cloner - DUPLICATES NODES! Network nodes != Vehicle route !!!
;; copies every slot, but if slot contains an object that may contain more objects, recursively copy-object it. If it is a list (but;; not a matrix (for dist-table), then mapcar copy-object it, since this list may contain objects (e.g. <node> objects in a <vehicle>'s route slot. Very non-generic function, might run into trouble when extending. Needs fix?

(defun vrp-object (object)
  "Tests if the object is an instance of a VRP object. (problem, fleet, network, vehicle, node)"
  (let ((type (type-of object)))
    (member type '(tsp fleet network node vehicle))))

(defun copy-object (object)
  "A deep-cloner for CLOS."
  (let* ((i-class (class-of object))
	 (clone (allocate-instance i-class)))
    (dolist (slot (sb-mop:class-slots i-class))
      (let ((slot-name (sb-mop:slot-definition-name slot)))
	(when (slot-boundp object slot-name)
	  (let ((value (slot-value object slot-name)))
	    (setf (slot-value clone slot-name)
		  (cond ((vrp-object value)
			 (copy-object (slot-value object slot-name)))
			((and (listp value) (not (listp (car value))))
			 (mapcar #'copy-object value))
			(t value)))))))
    clone))
