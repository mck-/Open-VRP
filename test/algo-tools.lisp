(in-package :open-vrp.test)

;; Algo tools
;; --------------------

(define-test route-from
  (:tag :algo)
  "Test route-from tool, which returns the route that contains the node"
  (let* ((ins-move (make-insertion-move :node-id :1 :vehicle-id :A :index 1))
         (a (make-vehicle :id :A :route (list (make-order :node-id :1)
                                              (make-order :node-id :2))))
         (b (make-vehicle :id :B :route (list (make-order :node-id :3))))
         (prob (make-instance 'problem :fleet (vector a b)))
         (prob2 (make-instance 'problem :fleet (vector b))))
    (assert-equal '(:1 :2) (mapcar #'visit-node-id (open-vrp.algo::route-from ins-move prob)))
    (assert-false (open-vrp.algo::route-from ins-move prob2))))
