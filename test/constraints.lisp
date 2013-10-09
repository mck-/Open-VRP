(in-package :open-vrp.test)

;; Constraints predicates
;; --------------------


;; Capacity
;; --------------------------------

(define-test capacity-constraints
  "Test the capacity constraints checkers"
  (:tag :constraints)
  (let* ((t0 (make-vehicle :id :space :capacity 3 :route (list (make-order :demand 1)
                                                               (make-order :demand 1))))
         (t1 (make-vehicle :id :full :capacity 2 :route (list (make-order :demand 1)
                                                              (make-order :demand 2))))
         (t2 (make-vehicle :id :not-full :capacity 10 :route (list (make-order :demand 1)
                                                               (make-order :demand 2))))
         (prob-overfull (make-instance 'problem :fleet (list t0 t1)))
         (cvrp-overfull (make-instance 'cvrp :fleet (list t0 t1)))
         (cvrp-overfull2 (make-instance 'cvrp :fleet (list t2 t1)))
         (cvrp-ok (make-instance 'cvrp :fleet (list t0 t2)))
         (cvrp-ok2 (make-instance 'cvrp :fleet (list t0))))
    (assert-true (in-capacity-p t0))
    (assert-false (in-capacity-p t1))
    (assert-true (in-capacity-p t2))
    (assert-false (in-capacity-p cvrp-overfull))
    (assert-false (in-capacity-p cvrp-overfull2))
    (assert-true (in-capacity-p cvrp-ok))
    (assert-true (in-capacity-p cvrp-ok2))
    (assert-true (constraints-p prob-overfull))
    (assert-false (constraints-p cvrp-overfull))
    (assert-true (constraints-p cvrp-ok))))

;; Time Windows
;; --------------------------------

(define-test time-after-visit
  "Test the time-after-visit util"
  (:tag :constraints)
  (assert-equal 8 (time-after-visit (make-order :start 5 :end 10 :duration 1) 7))
  (assert-equal 8 (time-after-visit (make-order :start 5 :end 10 :duration 3) 5))
  (assert-equal 8 (time-after-visit (make-order :start 5 :end 10 :duration 3) 1))
  (assert-equal 18 (time-after-visit (make-order :start 5 :end 10 :duration 10) 8))
  (assert-equal 600 (time-after-visit (make-order :start 500 :end 800 :duration 100) 0))
  (assert-equal 11 (time-after-visit (make-order :start 5 :end 10 :duration 1) 10))
  (assert-error 'too-late-arrival (time-after-visit (make-order :start 5 :end 10 :duration 1) 11))
  (assert-error 'simple-type-error (time-after-visit "visit?" 10))
  (assert-error 'simple-type-error (time-after-visit (make-order :start 5 :end 10 :duration 1) "8")))

(define-test time-window-constraints
  "Test the time-window constraints checkers"
  (:tag :constraints)
  (let* ((t1 (make-vehicle :id :on-time :start-location :1 :end-location :4
                           :route (list (make-order :node-id :2 :start 0 :end 2 :duration 1)
                                        (make-order :node-id :3 :start 5 :end 8 :duration 2))))
         (t2 (make-vehicle :id :too-late :start-location :1 :end-location :4
                           :route (list (make-order :node-id :2 :start 0 :end 2 :duration 10)
                                        (make-order :node-id :3 :start 5 :end 8 :duration 2))))
         (t3 (make-vehicle :id :on-time-speed :speed 2  :start-location :1 :end-location :4 :shift-end 7
                           :route (list (make-order :node-id :2 :start 0 :end 2 :duration 1)
                                        (make-order :node-id :3 :start 5 :end 8 :duration 1))))
         (t4 (make-vehicle :id :late-home :start-location :1 :end-location :4 :shift-end 6
                           :route (list (make-order :node-id :2 :start 0 :end 2 :duration 1)
                                        (make-order :node-id :3 :start 5 :end 8 :duration 2))))
         (t5 (make-vehicle :id :late-start :start-location :1 :end-location :4 :shift-end 7 :shift-start 3
                           :route (list (make-order :node-id :2 :start 0 :end 2 :duration 1)
                                        (make-order :node-id :3 :start 5 :end 8 :duration 1))))
         (dist-matrix (alist-to-hash '((:1 (:2 . 1)) (:2 (:3 . 1)) (:3 (:4 . 1)))))
         (on-time (make-instance 'vrptw :fleet (list t1 t1 t1) :dist-matrix dist-matrix))
         (too-late (make-instance 'vrptw :fleet (list t1 t2 t3) :dist-matrix dist-matrix)))
    (assert-true (veh-in-time-p t1 dist-matrix))
    (assert-false (veh-in-time-p t2 dist-matrix))
    (assert-true (veh-in-time-p t3 dist-matrix))
    (assert-false (veh-in-time-p t4 dist-matrix))
    (assert-false (veh-in-time-p t5 dist-matrix))
    (assert-true (in-time-p on-time))
    (assert-false (in-time-p too-late))))


;; Capacity AND Time Windows
;; --------------------------------


(define-test CVRPTW-constraints
  "Test the capacity AND time-window constraints checkers"
  (:tag :constraints)
  (let* ((t1 (make-vehicle :id :on-time-in-cap :capacity 10 :start-location :1 :end-location :4
                           :route (list (make-order :node-id :2 :demand 3 :start 0 :end 2 :duration 1)
                                        (make-order :node-id :3 :demand 3 :start 5 :end 8 :duration 2))))
         (t2 (make-vehicle :id :on-time-overfull :capacity 2 :start-location :1 :end-location :4
                           :route (list (make-order :node-id :2 :demand 2 :start 0 :end 2 :duration 1)
                                        (make-order :node-id :3 :demand 2 :start 5 :end 8 :duration 2))))
         (t3 (make-vehicle :id :late-in-cap :capacity 20 :start-location :1 :end-location :4 :shift-end 6
                           :route (list (make-order :node-id :2 :demand 2 :start 0 :end 2 :duration 1)
                                        (make-order :node-id :3 :demand 2 :start 5 :end 8 :duration 2))))
         (dist-matrix (alist-to-hash '((:1 (:2 . 1)) (:2 (:3 . 1)) (:3 (:4 . 1)))))
         (on-time-in-cap (make-instance 'cvrptw :fleet (list t1 t1 t1) :dist-matrix dist-matrix))
         (on-time-overfull (make-instance 'cvrptw :fleet (list t2 t2) :dist-matrix dist-matrix))
         (late-in-cap  (make-instance 'cvrptw :fleet (list t3 t3) :dist-matrix dist-matrix)))
    (assert-true (constraints-p on-time-in-cap))
    (assert-false (constraints-p on-time-overfull))
    (assert-false (constraints-p late-in-cap))))
