(in-package :open-vrp.test)

;; Algo tools
;; --------------------

(define-test route-from
  (:tag :algo)
  "Test route-from tool, which returns the route that contains the node"
  (let* ((ins-move (make-insertion-move :node-id :1 :vehicle-id :A :index 1))
         (ins-move2 (make-insertion-move :node-id :2 :vehicle-id :B :index 1))
         (a (make-vehicle :id :A :route (list (make-order :node-id :1)
                                              (make-order :node-id :2))))
         (b (make-vehicle :id :B :route (list (make-order :node-id :3))))
         (prob (make-instance 'problem :fleet (list a b)))
         (prob2 (make-instance 'problem :fleet (list b))))
    (assert-equal '(:1 :2) (mapcar #'visit-node-id (open-vrp.algo::route-from ins-move prob)))
    (assert-false (open-vrp.algo::route-from ins-move prob2))
    (assert-equal '(:1 :2) (mapcar #'visit-node-id (open-vrp.algo::route-to ins-move prob)))
    (assert-equal '(:1 :2) (mapcar #'visit-node-id (open-vrp.algo::route-from ins-move2 prob)))
    (assert-equal '(:3) (mapcar #'visit-node-id (open-vrp.algo::route-to ins-move2 prob)))))

;; Feasible move-p
;; --------------------

(define-test feasible-move-p
  (:tag :algo)
  "Test feasible-move-p predicate"
    (let* ((o1 (make-order :node-id :1 :demand 2))
           (o2 (make-order :node-id :2 :demand 1))
           (o3 (make-order :node-id :3 :demand 1))
           (o4 (make-order :node-id :4 :demand 2))
           (t1 (make-vehicle :id :full :capacity 3 :route (list o1 o2)))
           (t2 (make-vehicle :id :space :capacity 10 :route (list o3 o4)))
           (prob (make-instance 'problem :fleet (list t1 t2) :visits {:1 o1 :2 o2 :3 o3 :4 o4}))
           (cvrp (make-instance 'cvrp :fleet (list t1 t2) :visits {:1 o1 :2 o2 :3 o3 :4 o4})))
    ;; Capacity is ignored for non-CVRP problems
    (assert-true (feasible-move-p prob (make-insertion-move :node-id :4 :vehicle-id :full :index 1)))
    (assert-true (feasible-move-p prob (make-insertion-move :node-id :1 :vehicle-id :space :index 1)))
    ;; CVRP
    (assert-false (feasible-move-p cvrp (make-insertion-move :node-id :4 :vehicle-id :full :index 1)))
    (assert-true (feasible-move-p cvrp (make-insertion-move :node-id :1 :vehicle-id :space :index 1)))))

(define-test feasible-move-p-tw
  (:tag :algo)
  "Test feasible-move-p predicate for VRPTW problems"
  (let* ((o1 (make-order :node-id :1 :demand 2 :start 0 :end 11 :duration 1))
         (o2 (make-order :node-id :2 :demand 1 :start 0 :end 12 :duration 1))
         (o3 (make-order :node-id :3 :demand 1 :start 0 :end 13 :duration 1))
         (o4 (make-order :node-id :4 :demand 2 :start 0 :end 14 :duration 1))
         (t1 (make-vehicle :start-location :A :end-location :A :shift-end 10 :id :time :route (list o1 o2)))
         (t2 (make-vehicle :start-location :A :end-location :A :shift-end 7 :id :late :capacity 10 :route (list o3 o4)))
         (dist {:A {:1 3 :2 1 :3 5}
                :1 {:2 1 :3 1}
                :2 {:A 1 :3 1 :4 5}
                :3 {:A 1 :1 5 :2 2 :4 1}
                :4 {:A 1 :1 8}})
         (vrptw (make-instance 'vrptw :fleet (list t1 t2) :visits {:1 o1 :2 o2 :3 o3 :4 o4} :dist-matrix dist)))
    (assert-true (feasible-move-p vrptw (make-insertion-move :node-id :3 :vehicle-id :time :index 2)))
    (assert-true (feasible-move-p vrptw (make-insertion-move :node-id :3 :vehicle-id :time :index 1)))
    (assert-false (feasible-move-p vrptw (make-insertion-move :node-id :3 :vehicle-id :time :index 0)))

    (assert-true (feasible-move-p vrptw (make-insertion-move :node-id :2 :vehicle-id :late :index 0)))
    (assert-false (feasible-move-p vrptw (make-insertion-move :node-id :2 :vehicle-id :late :index 1)))
    (assert-false (feasible-move-p vrptw (make-insertion-move :node-id :1 :vehicle-id :late :index 0)))))
