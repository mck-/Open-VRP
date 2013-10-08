(in-package :open-vrp.test)

;; Algo best insertion
;; --------------------

(define-test generate-insertion-moves
  (:tag :algo)
  "Test generate-insertion-moves which returns a list of all possible moves"
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
         (prob (make-instance 'problem :fleet (vector t1)
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
