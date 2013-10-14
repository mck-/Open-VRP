(in-package :open-vrp.util)

;; Simple utils from Paul Graham's Onlisp
;; -------------------------------

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

;; ----------------------------------------------------------

;; Tue Nov 29, 2011
;; quick ugly tsp cloner - DUPLICATES NODES! Network nodes != Vehicle route !!!
;; copies every slot, but if slot contains an object that may contain more objects, recursively copy-object it. If it is a list, then mapcar copy-object it, since this list may contain objects (e.g. <node> objects in a <vehicle>'s route slot. Very non-generic function, might run into trouble when extending. Needs fix?

(defun vrp-object (object)
  "Tests if the object is an instance of a VRP object that needs deep copy. (problem, fleet, vehicle)"
  (member (type-of object) '(problem fleet vehicle)))

(defun copy-object (object)
  "A deep-cloner for CLOS."
  (let* ((i-class (class-of object))
         (clone (allocate-instance i-class)))
    (dolist (slot (class-slots i-class))
      (let ((slot-name (slot-definition-name slot)))
        (when (slot-boundp object slot-name)
          (let ((value (slot-value object slot-name)))
            (setf (slot-value clone slot-name)
                  (cond ((eq (type-of value) 'network)
                         value)
                        ((vrp-object value)
                         (copy-object (slot-value object slot-name)))
                        ((listp value)
                         (mapcar #'copy-object value))
                        (t value)))))))
    clone))
