;;; High-level methods to start solving
;;; --------------------------
;;; - run-algo (<problem> <algo>)        - DESTRUCTIVE
;;; - solve-prob (<problem> <algo>)      - UNDESTRUCTIVE
;;; - solve-plot (<problem> <algo>)      - plots the best solution after solving
;;; - multi-run (int algo-call)          - Run algo int times - collect all results
;;; - get-best-solution-from-multi-run   - returns the best solution from collection
;;; - multi-run-algo                     - Calls multi-run, prints stats and returns best
;;; - iterate (<algo>)                   - run one iteration on <algo>
;;; - iterate-more (<algo> int)          - reset number of iterations and call run-algo
;;; ----------------------------
(in-package :open-vrp.util)

(defparameter *start-time* nil)
(defparameter *finish-time* nil)

;; Run Algo
;; -------------------------
(defun init-algo (sol algo)
  "Given a solution, sets the :current-sol, :best-fitness and :best-sol slots of the <algo> object. Makes a copy of the solution for :best-sol. Returns <algo>."
  (setf (algo-current-sol algo) sol
        (algo-best-fitness algo) (fitness sol)
        (algo-best-sol algo) (copy-object sol))
  algo)


(defgeneric run-algo (problem algo)
  (:method (problem algo)
    "run-algo: Either problem or algo is not defined/correct")
  (:documentation "Runs the algo on the problem. Destructive - will have side-effects on the <problem> and <algo> objects. Use solve-prob to prevent <problem> object being touched. Will return the <Algo> object, which will contain the solution (in the form of a copy of the <problem> object) in the :best-sol slot. When defining your own algorithms, make sure to implement a run-algo method for your algo, which sets the <algo> slots appropriately, and returns the <algo> object. For algorithms that just build a solution (like greedy-best-insertion or greedy-append), you can use init-algo to set :current-sol, :best-sol, :best-fitness simultaneously. For search algorithms -- such as local search, population based algorithms -- may make use of the iterate method to automatically set the slots after each iteration."))

;; In case of error or interrupt, bind current state of algo in *algo-backup*.
(defmethod run-algo :around ((p problem) (a algo))
  (unwind-protect
       (call-next-method)
    (defparameter *algo-backup* a)))

;; Before method to start the timer
(defmethod run-algo :before ((p problem) (a algo))
  (setq *start-time* (get-universal-time)))

;; After method that makes all algos print the final solution
(defmethod run-algo :after ((p problem) (a algo))
  (setq *finish-time* (get-universal-time))
  (with-log-or-print (str p *start-time*)
    (print-final-results p a str)))
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

;; Before method to supersede log-file and print log file heading
;; This method is not part of the run-algo :before, because that would cause iterate-more
;; which calls run-algo to supersede instead of append to file.
(defmethod solve-prob :before ((p problem) (a algo))
  (with-log-or-print (str p (get-universal-time) nil)
    (print-header p a str)))

;; When all logging is done in file, at least print the final solution in repl
(defmethod solve-prob :after ((p problem) (a algo))
  (unless (log-to-repl-p p)
    (print-final-results p a)))

  ;; @mck- Oct 9, 2013 - disable drawing for now
  ;; (when (and (problem-drawer p) (drawer-plotp (problem-drawer p)))
  ;;   (plot-solution (algo-best-sol a))))
;; ---------------------------

;; Multi-run
;; ---------------------------
(defparameter *multi-run-start-time* nil)
(defparameter *multi-run-finish-time* nil)

(defmacro multi-run (times &body algo-call)
  "Run algo x times and collect all resulting solution objects in a list."
  `(progn
     ;so it won't override the first log-file made by solve-prob use -1 seconds
     (setq *multi-run-start-time* (- (get-universal-time) 1))
     (loop for ,(gensym) below ,times collect ,@algo-call
        finally (setq *multi-run-finish-time* (get-universal-time)))))

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
  "Run algo x times, print multi-run-stats and return the best result."
  (with-gensyms (prob results best)
    `(let* ((,prob ,(cadar algo-call))
            (,results (multi-run ,times ,@algo-call))
            (,best (get-best-solution-from-multi-run ,results)))
       (with-log-or-print (str ,prob *multi-run-start-time* nil)
         (print-multi-run-stats ,results str)
         (print-routes ,best str))
       (unless (log-to-repl-p ,prob)
         (print-multi-run-stats ,results)
         (print-routes ,best))
       ,best)))
;; -------------------

;; Iterate
;; -------------------
(defgeneric iterate (algo)
  (:method (algo) "iterate: This algo is not defined.")
  (:documentation "Runs the algo one iteration. Implementation of this method should use the algo's slot current-sol as current solution and destructively adjust it to a new solution. When algo's slot iterations is 0, then print the best solution found by this algo object. Returns the <algo> object. After each iterate, will automatically check if a new best solution has been found and adjust the :best-sol and :best-fitness slots for you."))

(defmethod iterate :around ((a algo))
  (if (< (algo-iterations a) 1)
      (progn (format t "No more iterations left.") a)
      (call-next-method)))

(defmethod iterate :after ((a algo))
  (let ((sol (algo-current-sol a)))
    ;; reduce iteration
    (setf (algo-iterations a) (1- (algo-iterations a)))

    ;; Logging
    (with-log-or-print (str sol *start-time*)
      (format str "~&Iterations to go: ~A~%" (algo-iterations a))
      (print-routes sol str))
    ;; Print dots in REPL if logging is to file
    (unless (log-to-repl-p a)
      (princ "."))

    ;; Checking if new best solution
    (let ((new-fitness (fitness sol))
          (best-fitness (algo-best-fitness a)))
      (when (or (null best-fitness)
                (< new-fitness best-fitness))
        (setf (algo-best-fitness a) new-fitness
              (algo-best-sol a) (copy-object sol)
              (algo-best-iteration a) (algo-iterations a))))))

    ;; Plot frame if animatep is set to T
    ;; (when (algo-animatep a)
    ;;   (plot-solution sol (merge-pathnames
    ;;                       (with-output-to-string (s)
    ;;                         (princ "run-frames/Iteration " s)
    ;;                         (princ (algo-iterations a) s)
    ;;                         (princ ".png" s))
    ;;                       (asdf:system-source-directory 'open-vrp))))))

;; Resume run - add some more iterations
;; ------------------------
(defgeneric iterate-more (algo int)
  (:method (algo int) "iterate-more: expects <Algo> and int as inputs")
  (:documentation "When an algo finished (i.e. iterations = 0) using iterate-more allows you to keep running it x more iterations. Also resets :best-iteration, which the stopping condition uses. Calls run-algo on the :current-sol of and with <algo>."))

(defmethod iterate-more ((a algo) int)
  (setf (algo-iterations a) int
        (algo-best-iteration a) int)
  (run-algo (algo-current-sol a) a))

(defmethod iterate-more :after ((a algo) int)
  (unless (log-to-repl-p a)
    (print-final-results (algo-best-sol a) a)))
;; ---------------------
