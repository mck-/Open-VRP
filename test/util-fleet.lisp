(in-package :open-vrp.test)

;; Fleet utilities
;; --------------------

(define-test route-indices
  "Test the route-indices util, which just lists the node-ids of all visits en-route"
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
    (assert-equal '(nil (:O1 :O2 :O3) (:O5 :O8 :O6)) (route-indices prob))))
