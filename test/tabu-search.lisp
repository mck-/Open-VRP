(in-package :open-vrp.test)

;; Tabu Search tests
;; --------------------

(define-test initialize/generate/assess/perform
  (:tag :ts)
  "Test initialize algorithm to generate initial feasible solutions, generate all possible moves, assess and perform"
  (let* ((o1 (make-order :duration 1 :start 0 :end 11 :node-id :o1 :demand 1))
         (o2 (make-order :duration 2 :start 0 :end 20 :node-id :o2 :demand 1))
         (o3 (make-order :duration 3 :start 10 :end 13 :node-id :o3 :demand 1))
         (o4 (make-order :duration 4 :start 10 :end 14 :node-id :o4 :demand 1))
         (o5 (make-order :duration 5 :start 10 :end 20 :node-id :o5 :demand 1))
         (t1 (make-vehicle :id :t1 :start-location :A :end-location :B :shift-end 25 :capacity 2))
         (t2 (make-vehicle :id :t2 :start-location :A :end-location :B :shift-start 10 :shift-end 25 :capacity 20 :speed 0.1))
         (dist {:o1 {      :o2 1 :o3 2 :o4 3 :o5 5 :A 1 :B 4}
                :o2 {:o1 1       :o3 1 :o4 2 :o5 4 :A 2 :B 3}
                :o3 {:o1 2 :o2 1       :o4 1 :o5 3 :A 3 :B 2}
                :o4 {:o1 3 :o2 2 :o3 1       :o5 1 :A 4 :B 1}
                :o5 {:o1 4 :o2 3 :o3 2 :o4 1       :A 6 :B 2}
                :A  {:o1 1 :o2 2 :o3 3 :o4 4 :o5 6      :B 5}
                :B  {:o1 4 :o2 3 :o3 2 :o4 1 :o5 2 :A 5     }})
         (prob (make-instance 'problem :fleet (list t1 t2)
                              :allow-unserved nil
                              :dist-matrix dist
                              :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5}))
         (prob2 (make-instance 'problem
                               :fleet (list (make-vehicle :id :t1 :start-location :A :end-location :B :route (list o1 o2 o3 o4))
                                            (make-vehicle :id :t2 :start-location :A :end-location :A :route (list o5))
                                            (make-vehicle :id :t3 :start-location :A :end-location :A))
                               :allow-unserved t
                               :dist-matrix dist
                               :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5}))
         (prob3 (copy-object prob2))
         (algo (initialize prob (make-instance 'tabu-search)))
         (algo2 (make-instance 'tabu-search :current-sol prob2 :best-sol prob2))
         (algo-a (make-instance 'tabu-search :current-sol prob3 :best-sol prob3 :best-fitness 17))
         (algo-no-a (make-instance 'tabu-search :current-sol (copy-object prob3) :best-sol (copy-object prob3) :best-fitness 17 :aspiration-p nil)))
    (assert-equal 7 (algo-best-fitness algo))
    (assert-equal '((:A :O1 :O2 :O3 :O4 :O5 :B) (:A :B))
                  (route-indices (algo-current-sol algo)))
    (assert-equal '((:A :O1 :O2 :O3 :O4 :O5 :B) (:A :B))
                  (route-indices (algo-best-sol algo)))

    ;; Generate moves
    (assert-equal 10 (length (generate-moves algo)))
    (assert-equal 18 (length (generate-moves algo2)))

    ;; Assess moves
    (assert-equal 2 (assess-move prob2 (make-ts-best-insertion-move :node-id :o1 :vehicle-id :t1)))
    (assert-equal 2 (assess-move prob2 (make-ts-best-insertion-move :node-id :o2 :vehicle-id :t1)))
    (assert-equal 2 (assess-move prob2 (make-ts-best-insertion-move :node-id :o3 :vehicle-id :t1)))
    (assert-equal 2 (assess-move prob2 (make-ts-best-insertion-move :node-id :o4 :vehicle-id :t1)))
    (assert-equal -10 (assess-move prob2 (make-ts-best-insertion-move :node-id :o5 :vehicle-id :t1)))
    (assert-equal -1 (assess-move prob2 (make-ts-best-insertion-move :node-id :o1 :vehicle-id :t2)))
    (assert-equal -1 (assess-move prob2 (make-ts-best-insertion-move :node-id :o2 :vehicle-id :t2)))
    (assert-equal -1 (assess-move prob2 (make-ts-best-insertion-move :node-id :o3 :vehicle-id :t2)))
    (assert-equal -1 (assess-move prob2 (make-ts-best-insertion-move :node-id :o4 :vehicle-id :t2)))
    (assert-equal 2 (assess-move prob2 (make-ts-best-insertion-move :node-id :o1 :vehicle-id :t3)))
    (assert-equal 4 (assess-move prob2 (make-ts-best-insertion-move :node-id :o2 :vehicle-id :t3)))
    (assert-equal 6 (assess-move prob2 (make-ts-best-insertion-move :node-id :o3 :vehicle-id :t3)))
    (assert-equal 8 (assess-move prob2 (make-ts-best-insertion-move :node-id :o4 :vehicle-id :t3)))

    ;; Perform moves
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o5 :vehicle-id :t1))
    (assert-equal '((:A :O1 :O2 :O3 :O4 :O5 :B) (:A :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o1 :vehicle-id :t1))
    (assert-equal '((:A :O2 :O1 :O3 :O4 :O5 :B) (:A :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o1 :vehicle-id :t1))
    (assert-equal '((:A :O1 :O2 :O3 :O4 :O5 :B) (:A :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o1 :vehicle-id :t2))
    (assert-equal '((:A :O2 :O3 :O4 :O5 :B) (:A :O1 :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o5 :vehicle-id :t2))
    (assert-equal '((:A :O2 :O3 :O4 :B) (:A :O5 :O1 :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o2 :vehicle-id :t1))
    (assert-equal '((:A :O3 :O2 :O4 :B) (:A :O5 :O1 :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o4 :vehicle-id :t2))
    (assert-equal '((:A :O3 :O2 :B) (:A :O4 :O5 :O1 :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o3 :vehicle-id :t2))
    (assert-equal '((:A :O2 :B) (:A :O4 :O5 :O3 :O1 :A) (:A :A))
                  (route-indices prob2))
    (perform-move prob2 (make-ts-best-insertion-move :node-id :o2 :vehicle-id :t2))
    (assert-equal '((:A :B) (:A :O4 :O5 :O3 :O2 :O1 :A) (:A :A))
                  (route-indices prob2))
    (assert-equal 10 (fitness prob2))

    ;; Select-move
    (let ((best-move (make-ts-best-insertion-move :node-id :o5 :vehicle-id :t1 :fitness -10))
          (assessed-moves (assess-moves (algo-current-sol algo-a) (generate-moves algo-a))))
      (assert-equalp best-move (select-move algo-a assessed-moves))
      (assert-equalp best-move (select-move algo-no-a assessed-moves))
      (add-move-to-tabu algo-a best-move)
      (add-move-to-tabu algo-no-a best-move)
      (assert-equalp best-move (select-move algo-a assessed-moves))
      (assert-equalp (make-ts-best-insertion-move :node-id :o1 :vehicle-id :t2 :fitness -1)
                     (select-move algo-no-a assessed-moves)))

    ;; Iterate
    (iterate algo-a)
    (assert-equal '((:A :O1 :O2 :O3 :O4 :O5 :B) (:A :A) (:A :A))
                  (route-indices (algo-current-sol algo-a)))
    (iterate algo-no-a)
    (assert-equal '((:A :O2 :O3 :O4 :B) (:A :O5 :O1 :A) (:A :A))
                  (route-indices (algo-current-sol algo-no-a)))
    (iterate algo-no-a)
    (assert-equal '((:A :O2 :O3 :B) (:A :O4 :O5 :O1 :A) (:A :A))
                  (route-indices (algo-current-sol algo-no-a)))


    ;; Stopping condition
    ;; pretend we found best solution at iteration 100 -- time to stop now
    (setf (algo-best-iteration algo-a) 100)
    (assert-prints "Stopping condition met.
No more iterations left." (iterate algo-a))))
