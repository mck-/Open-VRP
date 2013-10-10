(in-package :open-vrp.test)

;; Tabu Search tests
;; --------------------

(define-test tabu-list
  (:tag :ts-util)
  "Test tabu-list utilities"
  (let ((ts (make-instance 'tabu-search :tabu-tenure 2)))
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
    (assert-false (is-tabu-p ts 4))))
