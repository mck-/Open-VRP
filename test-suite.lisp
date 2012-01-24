;;; Sun 22 Jan, 2012
;;; ------------------
;;; A test suite for Open-VRP using FiveAM
(in-package :open-vrp)

(def-suite :suite-open-vrp)
(in-suite :suite-open-vrp)

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
       (test ,(symb algo-symbol '-100) (is (solve-prob solomon100 (make-instance ,algo-symbol)))))))

;; routine tests
(on-all-testcases 'greedy-nn)
(on-all-testcases 'greedy-append)
(on-all-testcases 'greedy-best-insertion)
(on-all-testcases 'tabu-search)


;; special tabu-search tests

(test tabu-100 (is (solve-prob solomon100 (make-instance 'tabu-search :iterations 100))))

;; capacity constraint tests
;; Could not use symbol-macro-let, since FiveAM's test macro won't allow lexical scoping
(define-symbol-macro full-v (make-instance 'vehicle-c
					   :capacity 2
					   :route (list (make-instance 'node-c :demand 1)
							(make-instance 'node-c :demand 1))))
(define-symbol-macro overfull-v (make-instance 'vehicle-c
					       :capacity 2
					       :route (list (make-instance 'node-c :demand 1)
							    (make-instance 'node-c :demand 2))))
(test capacity-veh-in
  (is (in-capacityp full-v)))

(test capacity-veh-out
  (is (null (not (in-capacityp overfull-v)))))

(test capacity-fleet-in
  (is (in-capacityp (make-instance 'fleet :vehicles (list full-v full-v full-v)))))

(test capacity-fleet-out
  (is (null (not (in-capacityp (make-instance 'fleet :vehicles (list overfull-v full-v)))))))

;; time window tests
(defmacro make-node-tw (id x y start end duration)
  `(make-instance 'node-tw :id ,id :xcor ,x :ycor ,y :start ,start :end ,end :duration ,duration))

(define-symbol-macro on-time-v
    (make-instance
     'vehicle-tw
     :route (list
	     (make-node-tw 1 1 0 0 2 1)
	     (make-node-tw 2 2 0 0 2 1) 
	     (make-node-tw 3 3 0 5 8 2)
	     (make-node-tw 4 4 0 0 10 1))))

(define-symbol-macro late-v-duration
    (make-instance
     'vehicle-tw
     :route (list
	     (make-node-tw 1 1 0 0 2 10)
	     (make-node-tw 2 2 0 0 2 1)
	     (make-node-tw 3 3 0 5 8 2)
	     (make-node-tw 4 4 0 0 10 1))))

(define-symbol-macro late-v-speed
    (make-instance
     'vehicle-tw
     :speed 0.5
     :route (list
	     (make-node-tw 1 1 0 0 2 1)
	     (make-node-tw 2 2 0 0 2 1)
	     (make-node-tw 3 3 0 5 8 2)
	     (make-node-tw 4 4 0 0 10 1))))


(test time-window-test-on-time
  (is (in-timep on-time-v)))

(test time-window-test-too-late
  (is (null (not (in-timep late-v-duration)))))

(test time-window-test-too-late
  (is (null (not (in-timep late-v-speed)))))

(test time-window-test-fleet-on-time
  (is (in-timep (make-instance 'fleet :vehicles (list on-time-v on-time-v on-time-v)))))

(test time-window-test-fleet-late
  (is (in-timep (make-instance 'fleet :vehicles (list on-time-v late-v-speed on-time-v)))))