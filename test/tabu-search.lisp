(in-package :open-vrp.test)

;; Tabu Search tests
;; --------------------

(define-test tabu-list
  (:tag :ts-util)
  "Test tabu-list utilities"
  (let ((ts (make-instance 'tabu-search :tabu-tenure 2))
        (ts-nv (make-instance 'tabu-search :tabu-tenure 2 :tabu-parameter-f #'open-vrp.algo::ts-pars-nv)))
    (add-to-tabu ts 1)
    (add-to-tabu ts 2)
    (add-to-tabu ts 3)
    (add-to-tabu ts 4)
    (assert-false (is-tabu-p ts 1))
    (assert-false (is-tabu-p ts 2))
    (assert-true (is-tabu-p ts 3))
    (assert-true (is-tabu-p ts 4))
    (clear-tabu-list ts)
    (assert-false (is-tabu-p ts 1))
    (assert-false (is-tabu-p ts 2))
    (assert-false (is-tabu-p ts 3))
    (assert-false (is-tabu-p ts 4))
    (add-move-to-tabu ts (make-insertion-move :node-id :1 :vehicle-id :1 :index 1))
    (add-move-to-tabu ts (make-insertion-move :node-id :2 :vehicle-id :1 :index 1))
    (add-move-to-tabu ts (make-insertion-move :node-id :3 :vehicle-id :1 :index 1))
    (assert-false (is-tabu-move-p ts (make-insertion-move :node-id :1 :vehicle-id :1 :index 1)))
    (assert-true (is-tabu-move-p ts (make-insertion-move :node-id :2 :vehicle-id :1 :index 1)))
    (assert-true (is-tabu-move-p ts (make-insertion-move :node-id :3 :vehicle-id :1 :index 1)))

    ;; Add tabu moves with vehicle id and node id
    (add-move-to-tabu ts-nv (make-insertion-move :node-id :1 :vehicle-id :1 :index 1))
    (add-move-to-tabu ts-nv (make-insertion-move :node-id :2 :vehicle-id :1 :index 1))
    (add-move-to-tabu ts-nv (make-insertion-move :node-id :3 :vehicle-id :1 :index 1))
    (assert-false (is-tabu-move-p ts-nv (make-insertion-move :node-id :1 :vehicle-id :1 :index 1)))
    (assert-true (is-tabu-move-p ts-nv (make-insertion-move :node-id :2 :vehicle-id :1 :index 1)))
    (assert-true (is-tabu-move-p ts-nv (make-insertion-move :node-id :3 :vehicle-id :1 :index 1)))
    (assert-false (is-tabu-move-p ts-nv (make-insertion-move :node-id :2 :vehicle-id :2 :index 1)))
    (assert-false (is-tabu-move-p ts-nv (make-insertion-move :node-id :3 :vehicle-id :2 :index 1)))))
