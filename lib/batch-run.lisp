;; Batch run function -- used for benchmarking/algo testing
;; Requires a directory with test-case files of same format (may be nested)
;; --------------------------
(in-package :open-vrp.util)

(defmacro batch-run ((x dir-path loader-fn &key (plotp nil) (log-mode 1)) output-file num-times &body algo-call)
  "Given a directory, will call algo-call on each file that is loaded using the loader-fn and bound to x. Output-file is a mandatory filepath to which the results will be written. Algo-call must return an <Algo> object (e.g. multi-run-algo or solve-prob). Num-times is the number of times the algo-call will run on each test-case, which will be used for stats.

Example: (batch-run (test-case \"~/VRP-benchmarks/solomon-100/\" #'load-solomon-vrp-file)
	      \"~/CL/Open-VRP/run-logs/batch-test.txt\" 20
       (solve-prob test-case (make-instance 'tabu-search :iterations 300)))."
  (with-gensyms (file str results)
    `(progn
       (with-open-file (,str (ensure-directories-exist ,output-file)
			     :if-exists :supersede :direction :output)
	 (print ',(car algo-call) ,str) ;to record what the algo-call was
	 (print-batch-run-table-header ,str))
       (walk-directory
	,dir-path
	(lambda (,file)
	  (let* ((,x (funcall ,loader-fn ,file :plotp ,plotp :log-mode ,log-mode))
		 (,results (multi-run ,num-times ,@algo-call)))
	    (append-run-result ,output-file ,results)))))))