(in-package :open-vrp.test)

;; Node/Network utilities
;; --------------------

(define-test distance
  "Test the distance util, which accesses the dist-matrix' hash of hash"
  (:tag :network)
  (let ((matrix (alist-to-hash '((:a (:b . 1) (:c . 2)) (:b (:a . 3) (:c . 5)) (:c (:a . 8) (:b . 2))))))
    (assert-equal 1 (distance :a :b matrix))
    (assert-equal 2 (distance :a :c matrix))
    (assert-equal 3 (distance :b :a matrix))
    (assert-equal 5 (distance :b :c matrix))
    (assert-equal 8 (distance :c :a matrix))
    (assert-equal 2 (distance :c :b matrix))
    (assert-error 'same-origin-destination (distance :c :c matrix))
    (assert-error 'simple-type-error (distance "c" "a" matrix))
    (assert-error 'simple-type-error (distance :a :c '((:a (:c . 2)))))
    (assert-error 'simple-type-error (distance :d :c matrix))))
