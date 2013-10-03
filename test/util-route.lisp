(in-package :open-vrp.test)

;; Route utilities
;; --------------------

(define-test no-visits-p
  "Test the no-visits-p util, which tests if the route contains any order visits."
  (:tag :route)
  (let* ((t0 (make-vehicle :id :0))
         (t1 (make-vehicle :id :1 :route (list (make-order :node-id :O1)
                                               (make-order :node-id :O2)
                                               (make-order :node-id :O3))))
         (t2 (make-vehicle :id :2 :route (list (make-order :node-id :O5)
                                               (make-order :node-id :O8)
                                               (make-order :node-id :O6))))
         (t3 (make-vehicle :id :3 :route (list (make-pitstop))))
         (prob (make-instance 'problem :fleet (list t0 t1 t2 t3))))
    (assert-true (no-visits-p (vehicle-route t0)))
    (assert-true (no-visits-p (vehicle-route t3)))
    (assert-false (no-visits-p (vehicle-route t1)))
    (assert-false (no-visits-p (vehicle-route t2)))
    (assert-equal 2 (length (get-busy-vehicles prob)))))
