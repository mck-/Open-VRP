;;; A test suite for Open-VRP using FiveAM
(in-package :open-vrp)

(def-suite :suite-open-vrp)
(in-suite :suite-open-vrp)

;; Generic algo runs
;; --------------------------------------
(defmacro on-all-testcases (algo-symbol)
  (labels ((mkstr (&rest args)	   
	     (with-output-to-string (s)
	       (dolist (a args) (princ a s))))
	   (symb (&rest args)
	     (values (intern (apply #'mkstr args)))))
    `(progn
       (test ,(symb algo-symbol '-tsp) (is (solve-prob test-tsp (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-vrp) (is (solve-prob test-vrp (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-25) (is (solve-prob solomon25 (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-100) (is (solve-prob solomon100 (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-chr1) (is (solve-prob christofides-1 (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-chr2) (is (solve-prob christofides-2 (make-instance ,algo-symbol)))))))

;; routine tests
(on-all-testcases 'greedy-nn)
(on-all-testcases 'greedy-append)
(on-all-testcases 'greedy-best-insertion)
(on-all-testcases 'tabu-search)


;; special tabu-search tests

(test tabu-100 (is (solve-prob solomon100 (make-instance 'tabu-search :iterations 100))))

;; --------------------------------

;; Constraints checking tests
;; --------------------------------
;; Capacity
(define-symbol-macro space-v (make-instance 'vehicle
					   :capacity 3
					   :route (list (make-instance 'node :demand 1)
							(make-instance 'node :demand 1))))
(define-symbol-macro overfull-v (make-instance 'vehicle
					       :capacity 2
					       :route (list (make-instance 'node :demand 1)
							    (make-instance 'node :demand 2))))
(test capacity-veh-in
  (is (in-capacityp space-v)))

(test capacity-veh-out
  (is (not (in-capacityp overfull-v))))

(test capacity-fleet-in
  (is (in-capacityp (make-instance 'cvrp :fleet (list space-v space-v space-v)))))

(test capacity-fleet-out
  (is (not (in-capacityp (make-instance 'cvrp :fleet (list overfull-v space-v))))))

;; Time Windows
(defmacro make-node-tw (id x y start end duration &optional demand)
  `(make-instance 'node :id ,id :xcor ,x :ycor ,y :start ,start :end ,end :duration ,duration
		  ,@(when demand `(:demand ,demand))))

(define-symbol-macro on-time-v
    (make-instance
     'vehicle
     :speed 1
     :route (list
	     (make-node-tw 1 1 0 0 2 1)
	     (make-node-tw 2 2 0 0 2 1) 
	     (make-node-tw 3 3 0 5 8 2)
	     (make-node-tw 4 4 0 0 10 1))))

(define-symbol-macro late-v-duration
    (make-instance
     'vehicle
     :speed 1
     :route (list
	     (make-node-tw 1 1 0 0 2 10)
	     (make-node-tw 2 2 0 0 2 1)
	     (make-node-tw 3 3 0 5 8 2)
	     (make-node-tw 4 4 0 0 10 1))))

(define-symbol-macro late-v-speed
    (make-instance
     'vehicle
     :speed 0.5
     :route (list
	     (make-node-tw 1 1 0 0 2 1)
	     (make-node-tw 2 2 0 0 2 1)
	     (make-node-tw 3 3 0 5 8 2)
	     (make-node-tw 4 4 0 0 10 1))))


(test time-window-test-on-time
  (is (veh-in-timep on-time-v)))

(test time-window-test-too-late
  (is (null (not (veh-in-timep late-v-duration)))))

(test time-window-test-too-late
  (is (null (not (veh-in-timep late-v-speed)))))

(test time-window-test-fleet-on-time
  (is (in-timep (make-instance 'vrptw :fleet (list on-time-v on-time-v on-time-v)))))

(test time-window-test-fleet-late
  (is (in-timep (make-instance 'vrptw :fleet (list on-time-v late-v-speed on-time-v)))))

;; Capacity AND Time Windows
(define-symbol-macro on-time-and-in-cap-v
    (make-instance
     'vehicle
     :speed 1
     :capacity 10
     :route (list
	     (make-node-tw 1 1 0 0 2 1 3)
	     (make-node-tw 2 2 0 0 2 1 1) 
	     (make-node-tw 3 3 0 5 8 2 1)
	     (make-node-tw 4 4 0 0 10 1 1))))

(define-symbol-macro on-time-but-overfull-v
    (make-instance
     'vehicle
     :speed 1
     :capacity 2
     :route (list
	     (make-node-tw 1 1 0 0 2 1 3)
	     (make-node-tw 2 2 0 0 2 1 1) 
	     (make-node-tw 3 3 0 5 8 2 1)
	     (make-node-tw 4 4 0 0 10 1 1))))

(test tw-and-cap-test-ok
  (is (constraintsp (make-instance 'cvrptw :fleet (list on-time-and-in-cap-v on-time-and-in-cap-v)))))

(test tw-and-cap-test-fail
  (is (not (constraintsp (make-instance 'cvrptw :fleet (list on-time-and-in-cap-v on-time-but-overfull-v))))))
;; -----------------------

;; Move feasibility checks
;; -----------------------

(test cap-move-feasible
  (is (feasible-movep (make-instance 'cvrp :fleet (list space-v space-v) :network (vector (new-node 1 1 1 :demand 1)))
		      (make-instance 'insertion-move :node-id 0 :vehicle-id 0))))

(test cap-move-infeasible
  (is (not (feasible-movep (make-instance 'cvrp :fleet (list space-v space-v) :network (vector (new-node 1 1 1 :demand 5)))
			   (make-instance 'insertion-move :node-id 0 :vehicle-id 0)))))
				     
(test tw-move-feasible
  (is (feasible-movep (make-instance 'vrptw :fleet (list on-time-v on-time-v) :network (vector (make-node-tw 5 2 1 3 5 1)))
		      (make-instance 'insertion-move :node-id 0 :vehicle-id 0 :index 2))))

(test tw-move-infeasible
  (is (not (feasible-movep (make-instance 'vrptw :fleet (list on-time-v on-time-v) :network (vector (make-node-tw 5 2 1 3 5 1)))
			   (make-instance 'insertion-move :node-id 0 :vehicle-id 0 :index 3)))))
