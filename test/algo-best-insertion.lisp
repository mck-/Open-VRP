(in-package :open-vrp.test)

;; Algo best insertion
;; --------------------

(define-test generate-insertion-moves
  (:tag :algo)
  "Test generate-insertion-moves which returns a list of all possible moves -- and assess them"
  (let* ((o1 (make-order :node-id :o1))
         (o2 (make-order :node-id :o2))
         (o3 (make-order :node-id :o3))
         (o4 (make-order :node-id :o4))
         (o5 (make-order :node-id :o5))
         (t1 (make-vehicle :id :t1 :route (list o1 o2 o3 o4) :start-location :A :end-location :B))
         (dist {:o1 {      :o2 1 :o3 2 :o4 3 :o5 5 :A 1 :B 4}
                :o2 {:o1 1       :o3 1 :o4 2 :o5 4 :A 2 :B 3}
                :o3 {:o1 2 :o2 1       :o4 1 :o5 3 :A 3 :B 2}
                :o4 {:o1 3 :o2 2 :o3 1       :o5 1 :A 4 :B 1}
                :o5 {:o1 4 :o2 3 :o3 2 :o4 1       :A 6 :B 2}
                :A  {:o1 1 :o2 2 :o3 3 :o4 4 :o5 6      :B 5}
                :B  {:o1 4 :o2 3 :o3 2 :o4 1 :o5 2 :A 5     }})
         (prob (make-instance 'problem :fleet (list t1)
                              :dist-matrix dist
                              :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5})))
    (assert-equalp (list (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 4)
                         (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 3)
                         (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 2)
                         (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 1)
                         (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 0))
                   (open-vrp.algo::generate-insertion-moves prob :t1 :o5))
    (assert-equal 9 (assess-move prob (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 0)))
    (assert-equal 7 (assess-move prob (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 1)))
    (assert-equal 5 (assess-move prob (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 2)))
    (assert-equal 3 (assess-move prob (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 3)))
    (assert-equal 2 (assess-move prob (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 4)))))

(define-test assess-moves-with-capacity-feasibility
  (:tag :algo)
  "Test assess-move while checking capacity feasibility"
  (let* ((o1 (make-order :demand 1 :node-id :o1))
         (o2 (make-order :demand 1 :node-id :o2))
         (o3 (make-order :demand 1 :node-id :o3))
         (o4 (make-order :demand 1 :node-id :o4))
         (o5 (make-order :demand 1 :node-id :o5))
         (t1 (make-vehicle :id :t1 :route (list o1 o2) :start-location :A :end-location :B :capacity 5))
         (t2 (make-vehicle :id :t2 :route (list o3 o4) :start-location :A :end-location :B :capacity 2))
         (dist {:o1 {      :o2 1 :o3 2 :o4 3 :o5 5 :A 1 :B 4}
                :o2 {:o1 1       :o3 1 :o4 2 :o5 4 :A 2 :B 3}
                :o3 {:o1 2 :o2 1       :o4 1 :o5 3 :A 3 :B 2}
                :o4 {:o1 3 :o2 2 :o3 1       :o5 1 :A 4 :B 1}
                :o5 {:o1 4 :o2 3 :o3 2 :o4 1       :A 6 :B 2}
                :A  {:o1 1 :o2 2 :o3 3 :o4 4 :o5 6      :B 5}
                :B  {:o1 4 :o2 3 :o3 2 :o4 1 :o5 2 :A 5     }})
         (cvrp (make-instance 'cvrp :fleet (list t1 t2)
                              :dist-matrix dist
                              :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5})))
    (assert-equal 9 (assess-move cvrp (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 0)))
    (assert-equal 7 (assess-move cvrp (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 1)))
    (assert-equal 3 (assess-move cvrp (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 2)))
    (assert-equal nil (assess-move cvrp (make-insertion-move :node-id :o5 :vehicle-id :t2 :index 0)))
    (assert-equal nil (assess-move cvrp (make-insertion-move :node-id :o5 :vehicle-id :t2 :index 1)))
    (assert-equal nil (assess-move cvrp (make-insertion-move :node-id :o5 :vehicle-id :t2 :index 2)))))

(define-test assess-insertion-moves-with-tw-feasibility
  (:tag :algo)
  "Test assess-move while checking tw and cvrptw feasibility"
  (let* ((o1 (make-order :duration 1 :start 0 :end 11 :node-id :o1 :demand 1))
         (o2 (make-order :duration 2 :start 0 :end 20 :node-id :o2 :demand 1))
         (o3 (make-order :duration 3 :start 10 :end 13 :node-id :o3 :demand 1))
         (o4 (make-order :duration 4 :start 10 :end 14 :node-id :o4 :demand 1))
         (o5 (make-order :duration 5 :start 10 :end 15 :node-id :o5 :demand 1))
         (t1 (make-vehicle :id :t1 :route (list o1 o2) :start-location :A :end-location :B :shift-end 25 :capacity 2))
         (t2 (make-vehicle :id :t2 :route (list o3 o4) :start-location :A :end-location :B :shift-start 10 :shift-end 25))
         (dist {:o1 {      :o2 1 :o3 2 :o4 3 :o5 5 :A 1 :B 4}
                :o2 {:o1 1       :o3 1 :o4 2 :o5 4 :A 2 :B 3}
                :o3 {:o1 2 :o2 1       :o4 1 :o5 3 :A 3 :B 2}
                :o4 {:o1 3 :o2 2 :o3 1       :o5 1 :A 4 :B 1}
                :o5 {:o1 4 :o2 3 :o3 2 :o4 1       :A 6 :B 2}
                :A  {:o1 1 :o2 2 :o3 3 :o4 4 :o5 6      :B 5}
                :B  {:o1 4 :o2 3 :o3 2 :o4 1 :o5 2 :A 5     }})
         (vrptw (make-instance 'vrptw :fleet (list t1 t2)
                               :dist-matrix dist
                               :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5}))
         (cvrptw (make-instance 'cvrptw :fleet (list t1 t2)
                               :dist-matrix dist
                               :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5})))
    (assert-equal nil (assess-move vrptw (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 0)))
    (assert-equal 7 (assess-move vrptw (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 1)))
    (assert-equal 3 (assess-move vrptw (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 2)))
    (assert-equal nil (assess-move vrptw (make-insertion-move :node-id :o5 :vehicle-id :t2 :index 0)))
    (assert-equal nil (assess-move vrptw (make-insertion-move :node-id :o5 :vehicle-id :t2 :index 1)))
    (assert-equal nil (assess-move vrptw (make-insertion-move :node-id :o5 :vehicle-id :t2 :index 2)))
    (assert-equal nil (assess-move cvrptw (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 1)))
    (assert-equal nil (assess-move cvrptw (make-insertion-move :node-id :o5 :vehicle-id :t1 :index 2)))))

;; Perform moves
;; ------------------

(define-test perform-move
  (:tag :algo)
  "Test the perform-move util"
  (let* ((o1 (make-order :duration 1 :start 0 :end 11 :node-id :o1 :demand 1))
         (o2 (make-order :duration 2 :start 0 :end 20 :node-id :o2 :demand 1))
         (o3 (make-order :duration 3 :start 10 :end 13 :node-id :o3 :demand 1))
         (o4 (make-order :duration 4 :start 10 :end 14 :node-id :o4 :demand 1))
         (o5 (make-order :duration 5 :start 10 :end 15 :node-id :o5 :demand 1))
         (t1 (make-vehicle :id :t1 :route (list o1 o2) :start-location :A :end-location :B :shift-end 25 :capacity 2))
         (t2 (make-vehicle :id :t2 :route (list o3 o4) :start-location :A :end-location :B :shift-start 10 :shift-end 25))
         (dist {:o1 {      :o2 1 :o3 2 :o4 3 :o5 5 :A 1 :B 4}
                :o2 {:o1 1       :o3 1 :o4 2 :o5 4 :A 2 :B 3}
                :o3 {:o1 2 :o2 1       :o4 1 :o5 3 :A 3 :B 2}
                :o4 {:o1 3 :o2 2 :o3 1       :o5 1 :A 4 :B 1}
                :o5 {:o1 4 :o2 3 :o3 2 :o4 1       :A 6 :B 2}
                :A  {:o1 1 :o2 2 :o3 3 :o4 4 :o5 6      :B 5}
                :B  {:o1 4 :o2 3 :o3 2 :o4 1 :o5 2 :A 5     }})
         (prob (make-instance 'problem :fleet (list t1 t2)
                              :dist-matrix dist
                              :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5})))
    (assert-equal '(:A :o1 :o2 :B) (route-indices t1))
    (assert-error 'index-out-of-bounds
                  (perform-move prob (make-insertion-move :vehicle-id :t1 :node-id :o5 :index 3)))
    (assert-error 'index-out-of-bounds
                  (perform-move prob (make-insertion-move :vehicle-id :t2 :node-id :o5 :index 3)))
    (assert-equal (list o5 o1 o2)
                  (vehicle-route
                   (vehicle
                    (perform-move prob (make-insertion-move :vehicle-id :t1 :node-id :o5 :index 0))
                    :t1)))
    (assert-equal (list o3 o5 o4)
                  (vehicle-route
                   (vehicle
                    (perform-move prob (make-insertion-move :vehicle-id :t2 :node-id :o5 :index 1))
                    :t2)))
    (assert-equal (list o5 o1 o2 o3)
                  (vehicle-route
                   (vehicle
                    (perform-move prob (make-insertion-move :vehicle-id :t1 :node-id :o3 :index 3))
                    :t1)))))

;; Optimal insertion (used by Greedy Best Insertion)
;; ---------------------

(define-test get-best-insertion-move
  (:tag :algo)
  "Test get-best-insertion-move util"
  (let* ((o1 (make-order :duration 1 :start 0 :end 11 :node-id :o1 :demand 1))
         (o2 (make-order :duration 2 :start 0 :end 20 :node-id :o2 :demand 1))
         (o3 (make-order :duration 3 :start 10 :end 13 :node-id :o3 :demand 1))
         (o4 (make-order :duration 4 :start 10 :end 14 :node-id :o4 :demand 1))
         (o5 (make-order :duration 5 :start 10 :end 15 :node-id :o5 :demand 1))
         (t1 (make-vehicle :id :t1 :route (list o1 o2) :start-location :A :end-location :B :shift-end 25 :capacity 2))
         (t2 (make-vehicle :id :t2 :route (list o3 o4) :start-location :A :end-location :B :shift-start 10 :shift-end 25))
         (dist {:o1 {      :o2 1 :o3 2 :o4 3 :o5 5 :A 1 :B 4}
                :o2 {:o1 1       :o3 1 :o4 2 :o5 4 :A 2 :B 3}
                :o3 {:o1 2 :o2 1       :o4 1 :o5 3 :A 3 :B 2}
                :o4 {:o1 3 :o2 2 :o3 1       :o5 1 :A 4 :B 1}
                :o5 {:o1 4 :o2 3 :o3 2 :o4 1       :A 6 :B 2}
                :A  {:o1 1 :o2 2 :o3 3 :o4 4 :o5 6      :B 5}
                :B  {:o1 4 :o2 3 :o3 2 :o4 1 :o5 2 :A 5     }})
         (prob (make-instance 'problem :fleet (list t1 t2)
                              :dist-matrix dist
                              :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5})))
    (assert-equalp (make-insertion-move :node-id :o5 :vehicle-id :t2 :index 2 :fitness 2)
                   (get-best-insertion-move prob :o5))))
