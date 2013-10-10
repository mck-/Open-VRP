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


;; Tabu Search Candidate Lists
;; --------------------

(define-test candidate-lists
  (:tag :ts-util)
  "Test candidate-lists utilities"
  (let* ((ts (make-instance 'tabu-search :aspiration-p nil))
         (moves (list (make-insertion-move :fitness nil :node-id 1)
                      (make-insertion-move :fitness -8 :node-id 2)
                      (make-insertion-move :fitness 7 :node-id 3)
                      (make-insertion-move :fitness -2 :node-id 4)
                      (make-insertion-move :fitness nil :node-id 5)
                      (make-insertion-move :fitness 12 :node-id 6)
                      (make-insertion-move :fitness 8 :node-id 7)
                      (make-insertion-move :fitness 10 :node-id 8)
                      (make-insertion-move :fitness -10 :node-id 9))))
    (assert-equalp (list (make-insertion-move :fitness -10 :node-id 9)
                         (make-insertion-move :fitness -8 :node-id 2)
                         (make-insertion-move :fitness -2 :node-id 4))
                   (create-candidate-list ts (sort-moves moves)))
    (add-move-to-tabu ts (make-insertion-move :fitness -10 :node-id 9))
    (add-move-to-tabu ts (make-insertion-move :fitness -8 :node-id 2))
    (assert-equalp (list (make-insertion-move :fitness -2 :node-id 4))
                   (create-candidate-list ts (sort-moves moves)))
    (add-move-to-tabu ts (make-insertion-move :fitness -2 :node-id 4))
    (assert-equalp (list (make-insertion-move :fitness 7 :node-id 3))
                   (create-candidate-list ts (sort-moves moves)))
    (add-move-to-tabu ts (make-insertion-move :fitness 7 :node-id 3))
    (add-move-to-tabu ts (make-insertion-move :fitness 8 :node-id 7))
    (add-move-to-tabu ts (make-insertion-move :fitness 10 :node-id 8))
    (add-move-to-tabu ts (make-insertion-move :fitness 12 :node-id 6))
    ;; When all moves are tabu, select best tabu-move
    (assert-equalp (list (make-insertion-move :fitness -10 :node-id 9))
                   (create-candidate-list ts (sort-moves moves)))))
