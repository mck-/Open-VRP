;;;; Thu Nov 10, 2011 (c) Marc Kuo
;;;; Utilities to be shared among all algorithms
;;;; -----
(in-package :open-vrp.algo)

(defgeneric run-algo (problem algo)
  (:method (problem algo)
    "run-algo: Either problem or algo is not defined/correct")
  (:documentation "Runs the algo on the problem. Destructive - will have side-effects on the <problem> and <algo> objects. Use solve-prob to prevent <problem> object being touched. Will return the <Algo> object, which will contain the solution (in the form of a copy of the <problem> object) in the :current-sol slot. When defining your own algorithms, make sure to implement a run-algo method for your algo, which sets the :current-sol slot appropriately, and returns the <algo> object."))

;; After method that makes all algos print the final solution, and save last state solution in the current-solution slot of the algo object.
(defmethod run-algo :around ((p problem) (a algo))
  (unwind-protect
       (call-next-method)
    (defparameter *algo-backup* a)))

(defmethod run-algo :after ((p problem) (a algo))
  (print (concatenate 'string "Final solution of run with " (string (type-of a))))
  (print "---------------------")
  (print-routes a)
  (print "---------------------"))

;; Fri Dec 9, 2011 - THINK OF WAY TO AUTOMATICALLY SET algo-best-sol & algo-best-fitness
;; Currently it is defined in each run-algo defmethod. Should be general?

;;; Wed Nov 30, 2011 (Cesenatico)
;;; ---- trying a wrapper method to prevent destructive behaviour of CLOS. 
(defgeneric solve-prob (problem algo)
  (:method (problem algo)
    "solve-prob: Either problem or algo is not defined/correct")
  (:documentation "Solves the problem with the algo. Uses run-algo, but leaves the <problem> object untouched (<Algo> will suffer side-effects). Works with a clone (clone-problem in clone.lisp). NON-destructive wrapper to the run-algo method."))

(defmethod solve-prob ((problem problem) (algo algo))
  (let ((clone (copy-object problem)))
    (run-algo clone algo)))

;; Fri Dec 9, 2011
;; to quicken testing - solves and plots in one call.

(defgeneric solve-plot (problem algo)
  (:method (problem algo) "solve-plot: Requires two input objects: <problem>, <algo>")
  (:documentation "Solves and plots. Returns <algo> object. Will have side-effects on <algo> object, which contains the solution. Will leave <problem> object untouched. Calls solve-prob and plot-solution."))

(defmethod solve-plot ((problem problem) (algo algo))
  (let ((algo-obj (solve-prob problem algo)))
    (plot-solution (algo-best-sol algo-obj))
    algo-obj))

    
;;; Tue Nov 15, 2011
;;; -----------------------
;;; Since CLOS requires non-funcitonal programming style, below methods quickly reset the state ;;; of the objects. 
;;; Thu Dec 8, 2011 (Monselice) - might be obsolete after -> use solve-prob instead of run-algo!
(defgeneric re-init (obj)
  (:method (obj) "re-init: Cannot reset object of this type!")
  (:documentation "Reinitializes the state of the object. For fleet, reset locations/routes."))   
;; Originially thought of distinguishing between from-depot or not. But initial location will al;; ways be the first node in the vehicle's route.
(defmethod re-init ((f fleet))
  "For all vehicles, reset the routes to their initial location. Prints-route in output."
  (dolist (v (fleet-vehicles f))
    (setf (vehicle-route v) (list (car (vehicle-route v)))))
  (print-routes f))

;; Re-initializes the whole problem (currently only fleet. Perhaps later algo object too.)
(defmethod re-init ((p problem))
  (re-init (problem-fleet p)))  

      

  
 