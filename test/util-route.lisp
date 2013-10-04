(in-package :open-vrp.test)

;; Route utilities
;; --------------------

(define-test route-utils
  "Test the no-visits-p/get-busy-vehicles/one-destination-p utils"
  (:tag :route)
  (let* ((t0 (make-vehicle :id :0))
         (t1 (make-vehicle :id :1 :route (list (make-order :node-id :O1))))
         (t2 (make-vehicle :id :2 :route (list (make-order :node-id :O5)
                                               (make-order :node-id :O8)
                                               (make-order :node-id :O6))))
         (t3 (make-vehicle :id :3 :route (list (make-pitstop))))
         (prob (make-instance 'problem :fleet (list t0 t1 t2 t3))))
    (assert-true (no-visits-p (vehicle-route t0)))
    (assert-true (no-visits-p (vehicle-route t3)))
    (assert-false (no-visits-p (vehicle-route t1)))
    (assert-false (no-visits-p (vehicle-route t2)))
    (assert-error 'simple-type-error (no-visits-p t0))
    (assert-true (one-destination-p (vehicle-route t1)))
    (assert-false (one-destination-p (vehicle-route t2)))
    (assert-error 'simple-type-error (one-destination-p t0))
    (assert-equal 2 (length (get-busy-vehicles prob)))
    (assert-error 'simple-type-error (get-busy-vehicles t0))))
