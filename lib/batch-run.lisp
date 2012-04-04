;; Batch run function -- used for benchmarking/algo testing
;; Requires a directory with test-case files of same format (may be nested)
;; --------------------------
(in-package :open-vrp.util)

(defmacro batch-run ((x dir-path &key (plotp nil) (log-mode 1)) output-file num-times &body algo-call)
  "Given a directory, will call algo-call on each file that is loaded using the loader-fn and bound to x. Output-file is a mandatory filepath to which the results will be written. Algo-call must return an <Algo> object (e.g. multi-run-algo or solve-prob). Num-times is the number of times the algo-call will run on each test-case, which will be used for stats. Will return a list of list of algo objects holding the solutions.

Example: (batch-run (test-case \"test-cases/Solomon-25/\")
	      \"run-logs/Solomon-25-batch.txt\" 20
       (solve-prob test-case (make-instance 'tabu-search :iterations 300)))."
  (with-gensyms (file str results all-results)
    `(progn
       (with-open-file (,str (ensure-directories-exist ,output-file)
			     :if-exists :supersede :direction :output)
	 (print ',(car algo-call) ,str) ;to record what the algo-call was
	 (print-batch-run-table-header ,str))
       (let ((,all-results))
	 (walk-directory
	  ,dir-path
	  (lambda (,file)
	    (let ((,x (load-test-case-file ,file)))
	      (setf (drawer-plotp (problem-drawer ,x)) ,plotp)
	      (set-log-mode ,x ,log-mode)
	      (let ((,results (multi-run ,num-times ,@algo-call)))
		(append-run-result ,output-file ,results)
		(push ,results ,all-results)))))
	 (nreverse ,all-results)))))
	 