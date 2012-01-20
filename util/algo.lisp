;;; Thu Nov 10, 2011 (c) Marc Kuo
;;; High-level methods to start solving
;;; -----
;;; 1. run-algo (problem algo) - DESTRUCTIVE
;;; 2. solve-prob (problem algo) - UNDESTRUCTIVE
;;; 3. solve-plot (problem algo) - plots the best solution after solving
;;; 4. multi-run (int algo-call) - Run algo int times - collect all results
(in-package :open-vrp.algo)

;; Run Algo
;; -------------------------

(defgeneric run-algo (problem algo)
  (:method (problem algo)
    "run-algo: Either problem or algo is not defined/correct")
  (:documentation "Runs the algo on the problem. Destructive - will have side-effects on the <problem> and <algo> objects. Use solve-prob to prevent <problem> object being touched. Will return the <Algo> object, which will contain the solution (in the form of a copy of the <problem> object) in the :current-sol slot. When defining your own algorithms, make sure to implement a run-algo method for your algo, which sets the :current-sol slot appropriately, and returns the <algo> object."))

;; In case of error or interrupt, bind current state of algo in *algo-backup*.
(defmethod run-algo :around ((p problem) (a algo))
  (unwind-protect
       (call-next-method)
    (defparameter *algo-backup* a)))

;; After method that makes all algos print the final solution
(defmethod run-algo :after ((p problem) (a algo))
  (print (concatenate 'string "Final solution of run with " (string (type-of a))))
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
  (:documentation "Solves the problem with the algo. Uses run-algo, but leaves the <problem> object untouched (<Algo> will suffer side-effects). Works with a clone (clone-problem in clone.lisp). NON-destructive wrapper to the run-algo method."))

(defmethod solve-prob ((problem problem) (algo algo))
  (let ((clone (copy-object problem)))
    (run-algo clone algo)))

;; ----------------------------

;; Solve Plot
;; ------------------------------

(defgeneric solve-plot (problem algo)
  (:method (problem algo) "solve-plot: Requires two input objects: <problem>, <algo>")
  (:documentation "Solves and plots. Returns <algo> object. Will have side-effects on <algo> object, which contains the solution. Will leave <problem> object untouched. Calls solve-prob and plot-solution."))

(defmethod solve-plot ((problem problem) (algo algo))
  (let ((algo-obj (solve-prob problem algo)))
    (plot-solution (algo-best-sol algo-obj))
    algo-obj))

;; ---------------------------

;; Multi-run
;; ---------------------------

(defmacro multi-run (times algo-call)
  "Run algo x times and collect all resulting solution objects in a list."
  `(loop for ,(gensym) below ,times
	collect ,algo-call into solutions
	finally (return solutions)))

(defun get-best-solution (solutions)
  "Given a list of solutions (from multi-run), return the best solution."
  (labels ((iter (sols best)
	     (if sols
		 (iter (cdr sols)
		       (if (< (algo-best-fitness (car sols)) (algo-best-fitness best))
			   (car sols)
			   best))
		 best)))
    (iter (cdr solutions) (car solutions))))
	