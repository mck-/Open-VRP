;;; Output functions
;;; --------------------------
(in-package :open-vrp.util)

(defgeneric print-routes (prob/algo &optional stream)
  (:documentation "Prints solution given a <problem>/<algo> object. Also prints the total distance when the input is a <problem>/<algo> object."))

(defmethod print-routes ((prob problem) &optional (stream t))
  (format stream "~&---------------")
  (format stream "~&Fitness: ~A" (fitness prob))
  (format stream "~&---------------")
  (dolist (busy-veh (get-busy-vehicles prob))    
    (format stream "~&[~2D]: ~A~%" (vehicle-ID busy-veh) (route-indices busy-veh)))
  (format stream "~&---------------~%"))

(defmethod print-routes ((a algo) &optional (stream t))
  (print-routes (algo-best-sol a) stream))

(defun print-run-time (stream start-time)
  "Prints the difference between current time and start-time in seconds to stream for logging."
  (format stream "~&Run took a total of ~A seconds.~%" (- (get-universal-time) start-time)))

(defun print-multi-run-stats (algo-objects &optional (stream t))
  "Given a list of algo-objects returned by multi-run, print run-stats."
  (let ((results (mapcar #'algo-best-fitness algo-objects)))
    (print-run-time stream *multi-run-start-time*)
    (format stream "~&Runs: ~8a~%Max: ~8a~%Min: ~8a~%Avg: ~8a~%Std: ~8a~%"
	    (length results) (get-max results) (get-min results) (mean results) (standard-deviation results))))

(defun print-final-results (prob algo &optional (stream t))
  "Prints final results of run, helper function to :after methods of run-algo and solve-prob."
  (print-run-time stream *start-time*)
  (format stream "Final solution of run with ~A on ~A was found on iteration ~A~%"
	  (string (type-of algo)) (problem-name prob) (algo-best-iteration algo))
  (print-routes algo stream))


;; ---------------------------

;; Object printing methods
;; ---------------------------

(defun print-vrp-object (object &optional (stream t))
  "Given object, will print it's object's slots and values"
  (format stream "~&--------------")
  (format stream "~A object details:" (class-of object))
  (format stream "--------------~%~%")
  (dolist (slot (class-slots (class-of object)))
    (let ((slot-name (slot-definition-name slot)))
      (when (and
	     (slot-boundp object (slot-definition-name slot))
	     (not (or
		   (equal slot-name (intern "NETWORK" (find-package 'open-vrp.classes)))
		   (equal slot-name (intern "DIST-ARRAY" (find-package 'open-vrp.classes)))
		   (equal slot-name (intern "FLEET" (find-package 'open-vrp.classes))))))
	(format stream "~&Slot: ~18a Value: ~a~%" slot-name (slot-value object slot-name)))))
  (format stream "------------------------------------~%~%"))

;; -------------------------

;; Timestamp printing
;; -------------------------
(defun print-timestamp (&optional (stream t))
  "Prints timestamp to stream, source from cl-cookbook."
  (let ((days '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore dst-p))
    (format stream "~&It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~2,'0d/~2,'0d/~d (GMT~@d)"
	    hour minute second (nth day-of-week days) month date year (- tz)))))

(defun universal-time-to-string (&optional (time (get-universal-time)))
  "Returns yymmdd-hhmmss in a string. Used for timestamped log-files."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (with-output-to-string (s)
      (format s "~2,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
	      year month date hour minute second))))

;; -------------------------

;; with-log-file macro
;; -------------------------

(defun insert-time-stamp-in-path (path time)
  "Given path, return the string of the path with the timestamp inserted before the .xxx"
  (let* ((string (namestring path))
	 (cutoff (- (length string) 4)))
    (concatenate 'string
		 (subseq string 0 cutoff)
		 "_"
		 (universal-time-to-string time)
		 (subseq string cutoff))))

(defmacro with-log-or-print ((stream prob time &optional (appendp T)) &body body)
  "A wrapper on top of with-open-file, where we use the filepath stored in the :log-file slot of a problem object. When :log-mode is 0, return nil. If 1, use the file stream; if 2, use the T stream. Optional parameter appendp can be set to NIL in order to :supersede if file exists. By default appends. Returns T if logging is succesful. Requires universal-time, to append to log-file."
  (with-gensyms (func)
    `(flet ((,func (,stream)
	      ,@body))
       (ccase (problem-log-mode ,prob)
	 (0 nil)
	 (1 (with-open-file (,stream (insert-time-stamp-in-path (problem-log-file ,prob) ,time)
				     :direction :output
				     :if-exists (if ,appendp :append :supersede))
	      (,func ,stream)) t)
	 (2 (,func t) t)))))
	 

;; Acccessors for log-mode
(defgeneric log-to-replp (prob/algo)
  (:documentation "Returns T if :log-mode is set to 2, which is REPL."))

(defmethod log-to-replp ((p problem))
  (= (problem-log-mode p) 2))

(defmethod log-to-replp ((a algo))
  (= (problem-log-mode (algo-current-sol a)) 2))
  