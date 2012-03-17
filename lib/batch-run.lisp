;; Batch run function -- used for benchmarking/algo testing
;; Requires a directory with test-case files of same format (may be nested)
;; --------------------------
(in-package :open-vrp.util)

(defun print-run-results-table (matrix-algo-objects &optional (stream t))
  "Given a matrix (list of lists) of algo objects, print a table summarising the results."
  (format stream "~&| Test-case |   Min   |   Max   |   Avg   |   Std   | Runs | Time | Time/Run |~%")
  (dolist (one-case matrix-algo-objects)
    (multiple-value-bind (min max avg std runs time time-p-run)
	(get-multi-run-stats one-case)
      (format stream "~&|~11a|~9,2f|~9,2f|~9,2f|~9,2f|~6d|~6d|~10,2f|~%"
	      (problem-name (algo-current-sol (car one-case)))
	      min max avg std runs time time-p-run))))   



(defmacro batch-run ((x dir-path loader-fn &key (plotp nil) (log-mode 1)) output-file num-times &body algo-call)
  "Given a directory, will call algo-call on each file that is loaded using the loader-fn and bound to x. Output-file is a mandatory filepath to which the results will be written. Algo-call must return an <Algo> object (e.g. multi-run-algo or solve-prob). Num-times is the number of times the algo-call will run on each test-case, which will be used for stats.

Example: (batch-run (test-case \"~/VRP-benchmarks/solomon-100/\" #'load-solomon-vrp-file)
	      \"~/CL/Open-VRP/run-logs/batch-test.txt\" 20
       (solve-prob test-case (make-instance 'tabu-search :iterations 300)))."
  (with-gensyms (file str)
    `(with-open-file (,str ,output-file :if-exists :supersede :direction :output)
       (print ',(car algo-call) ,str) ;to record what the algo-call was
       (unless (directory-exists-p ,dir-path) (error 'missing-directory :dir ,dir-path))
       (let ((all-results
	      (mapcar
	       (lambda (,file)
		 (let ((,x (funcall ,loader-fn ,file :plotp ,plotp :log-mode ,log-mode)))
		   (multi-run ,num-times ,@algo-call)))
	       (list-directory ,dir-path))))
	 (print-run-results-table all-results ,str)
	 all-results))))