;;; High-level methods to start solving
;;; --------------------------
;;; - run-algo (<problem> <algo>)        - DESTRUCTIVE
;;; - solve-prob (<problem> <algo>)      - UNDESTRUCTIVE
;;; - solve-plot (<problem> <algo>)      - plots the best solution after solving
;;; - multi-run (int algo-call)          - Run algo int times - collect all results
;;; - get-best-solution-from-multi-run   - returns the best solution from collection
;;; - multi-run-algo                     - Calls multi-run, prints stats and returns best
;;; ----------------------------
(in-package :open-vrp.util)

;; Run Algo
;; -------------------------
(defun init-algo (prob algo)
  (setf (algo-current-sol algo) prob
	 (algo-best-sol algo) (copy-object prob)
	 (algo-best-fitness algo) (fitness prob)))

(defgeneric run-algo (problem algo)
  (:method (problem algo)
    "run-algo: Either problem or algo is not defined/correct")
  (:documentation "Runs the algo on the problem. Destructive - will have side-effects on the <problem> and <algo> objects. Use solve-prob to prevent <problem> object being touched. Will return the <Algo> object, which will contain the solution (in the form of a copy of the <problem> object) in the :best-sol slot. When defining your own algorithms, make sure to implement a run-algo method for your algo, which sets the <algo> slots appropriately, and returns the <algo> object. For algorithms that just build a solution (like greedy-best-insertion or greedy-append), you can use init-algo to set :current-sol, :best-sol, :best-fitness simultaneously. For search algorithms -- such as local search, population based algorithms -- may make use of the iterate method to automatically set the slots after each iteration."))

;; In case of error or interrupt, bind current state of algo in *algo-backup*.
(defmethod run-algo :around ((p problem) (a algo))
  (unwind-protect
       (call-next-method)
    (defparameter *algo-backup* a)))

;; After method that makes all algos print the final solution
(defmethod run-algo :after ((p problem) (a algo))
  (print (concatenate 'string "Final solution of run with " (string (type-of a)) " on " (problem-name p)))
  (print "---------------------")
  (print-routes a)
  (print "---------------------"))

;; -----------------------------

;; Solve Prob
;; ---------------------------------

;; a wrapper method to prevent destructive behaviour of CLOS. 
(defgeneric solve-prob (problem algo)
  (:method (problem algo)
    "solve-prob: Either problem or algo is not defined/correct")
  (:documentation "Solves the problem with the algo. Uses run-algo, but leaves the <problem> object untouched (<Algo> will suffer side-effects). Works with a clone (copy-object in lib/simple-utils.lisp). NON-destructive wrapper to the run-algo method."))

(defmethod solve-prob ((problem problem) (algo algo))
  (let ((clone (copy-object problem)))
    (run-algo clone algo)))

;; ----------------------------

;; Solve Plot
;; ------------------------------

(defgeneric solve-plot (problem algo)
  (:method (problem algo) "solve-plot: Requires two input objects: <problem>, <algo>")
  (:documentation "Solves and plots. Returns <algo> object. Will have side-effects on <algo> object, which contains the solution. Will leave <problem> object untouched. Calls solve-prob and plot-solution. By default, plots in plots/name.png where name is the :name of <problem>."))

(defmethod solve-plot ((problem problem) (algo algo))
  (let ((algo-obj (solve-prob problem algo)))
    (plot-solution (algo-best-sol algo-obj))
    algo-obj))

;; ---------------------------

;; Multi-run
;; ---------------------------

(defmacro multi-run (times &body algo-call)
  "Run algo x times and collect all resulting solution objects in a list."
  `(loop for ,(gensym) below ,times collect ,@algo-call))

(defun get-best-solution-from-multi-run (solutions)
  "Given a list of solutions (from multi-run), return the best solution."
  (labels ((iter (sols best)
	     (if sols
		 (iter (cdr sols)
		       (if (< (algo-best-fitness (car sols)) (algo-best-fitness best))
			   (car sols)
			   best))
		 best)))
    (iter (cdr solutions) (car solutions))))

(defmacro multi-run-algo (times &body algo-call)
  `(let* ((results (multi-run ,times ,@algo-call))
	  (best (get-best-solution-from-multi-run results)))
     (print-multi-run-stats results)
     (print-routes best)
     best))
;; -------------------

;; Iterate
;; -------------------
(defgeneric iterate (algo)
  (:method (algo) "iterate: This algo is not defined.")
  (:documentation "Runs the algo one iteration. Implementation of this method should use the algo's slot current-sol as current solution and destructively adjust it to a new solution. When algo's slot iterations is 0, then print the best solution found by this algo object. Returns the <algo> object when finished. After each iterate, will automatically check if a new best solution has been found and adjust the :best-sol and :best-fitness slots for you."))

;; when no more iterations, print solution and return the <Algo> object.
(defmethod iterate :around ((a algo))
  (if (< (algo-iterations a) 1)
      (progn
	(format t "No more iterations left.")
	(print-routes (algo-best-sol a))
	a)
      (call-next-method))) ; otherwise iterate

;; After each iteration, check to see if a new best solution has been found and save it.
(defmethod iterate :after ((a algo))
  (setf (algo-iterations a) (1- (algo-iterations a)))
  (format t "~&Iterations to go: ~A~%" (algo-iterations a))
  (let* ((sol (algo-current-sol a))
	 (new-fitness (fitness sol))
	 (best-fitness (algo-best-fitness a)))
    (print-routes sol)
    (when (or (null best-fitness)
	      (< new-fitness best-fitness))
      (setf (algo-best-fitness a) new-fitness)
      (setf (algo-best-sol a) (copy-object sol)))))

;; Resume run - add some more iterations
;; ------------------------
(defgeneric iterate-more (algo int)
  (:method (algo int) "iterate-more: expects <Algo> and int as inputs")
  (:documentation "When an algo finished (i.e. iterations = 0) using iterate-more allows you to keep running it x more iterations. Calls run-algo on the :current-sol of and with <algo>."))

(defmethod iterate-more ((a algo) int)
  (setf (algo-iterations a) int)
  (run-algo (algo-current-sol a) a))
;; ---------------------
