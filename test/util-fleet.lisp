(in-package :open-vrp.test)

;; Fleet utilities
;; --------------------

(define-test route-indices/vehicle-with-node-id
  "Test the route-indices util, which just lists the node-ids of all visits en-route. Also tests vehicle-with-node-id util, to reuse the mocks"
  (:tag :fleet)
  (let* ((t0 (make-vehicle :id :0))
         (t1 (make-vehicle :id :1 :route (list (make-order :node-id :O1)
                                               (make-order :node-id :O2)
                                               (make-order :node-id :O3))))
         (t2 (make-vehicle :id :2 :route (list (make-order :node-id :O5)
                                               (make-order :node-id :O8)
                                               (make-order :node-id :O6))))
         (prob (make-instance 'problem :fleet (list t0 t1 t2))))
    (assert-equal nil (route-indices t0))
    (assert-equal '(:O1 :O2 :O3) (route-indices t1))
    (assert-equal '(:O5 :O8 :O6) (route-indices t2))
    (assert-equal '(nil (:O1 :O2 :O3) (:O5 :O8 :O6)) (route-indices prob))
    (assert-equal :1 (vehicle-with-node-id prob :O1))
    (assert-equal :1 (vehicle-with-node-id prob :O2))
    (assert-equal :1 (vehicle-with-node-id prob :O3))
    (assert-equal :2 (vehicle-with-node-id prob :O5))
    (assert-equal :2 (vehicle-with-node-id prob :O6))
    (assert-equal :2 (vehicle-with-node-id prob :O8))
    (assert-equal nil (vehicle-with-node-id prob :Q8))
    (assert-error 'expect-keyword-arguments (vehicle-with-node-id prob "O1"))))

(define-test node-on-route-p
  "Test node-on-route-p util"
  (:tag :fleet)
  (let ((t1 (make-vehicle :id :1 :route (list (make-depot :node-id :D1)
                                              (make-order :node-id :O1)
                                              (make-depot :node-id :D2)))))
    (assert-equal nil (node-on-route-p :Q8 t1))
    (assert-true T (node-on-route-p :O1 t1))
    (assert-true T (node-on-route-p :D1 t1))
    (assert-true T (node-on-route-p :D2 t1))
    (assert-error 'expect-keyword-arguments (node-on-route-p "c" t1))
    (assert-error 'expect-keyword-arguments (node-on-route-p 1 t1))
    (assert-error 'expect-vehicle (node-on-route-p :D2 "hello"))))
