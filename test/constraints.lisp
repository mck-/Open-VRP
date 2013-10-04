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
         (t2 (make-vehicle :id :full :capacity 10 :route (list (make-order :demand 1)
                                                               (make-order :demand 2))))
         (prob-overfull (make-instance 'problem :fleet (vector t0 t1)))
         (cvrp-overfull (make-instance 'cvrp :fleet (vector t0 t1)))
         (cvrp-overfull2 (make-instance 'cvrp :fleet (vector t2 t1)))
         (cvrp-ok (make-instance 'cvrp :fleet (vector t0 t2)))
         (cvrp-ok2 (make-instance 'cvrp :fleet (vector t0))))
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
