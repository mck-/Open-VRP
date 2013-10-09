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
    (assert-error 'distance-between-nodes-undefined (distance :d :c matrix))
    (assert-error 'distance-between-nodes-undefined (distance :c :d matrix))))

(define-test travel-time
  "Test the travel-time util, which is the same as dist-matrix if the speed is 1."
  (:tag :network)
  (let ((matrix (alist-to-hash '((:a (:b . 1) (:c . 2)) (:b (:a . 3) (:c . 5)) (:c (:a . 8) (:b . 2))))))
    (assert-equal 1 (travel-time :a :b matrix))
    (assert-equal 2 (travel-time :a :c matrix))
    (assert-equal 3 (travel-time :b :a matrix))
    (assert-equal 5 (travel-time :b :c matrix))
    (assert-equal 8 (travel-time :c :a matrix))
    (assert-equal 2 (travel-time :c :b matrix))
    (assert-error 'same-origin-destination (travel-time :c :c matrix))
    (assert-error 'simple-type-error (travel-time "c" "a" matrix))
    (assert-error 'simple-type-error (travel-time :a :c '((:a (:c . 2)))))
    (assert-error 'distance-between-nodes-undefined (travel-time :d :c matrix))

    ;; With speed
    (assert-equalp 10 (travel-time :b :c matrix :speed 0.5))
    (assert-equal 4 (travel-time :c :a matrix :speed 2))
    (assert-equal 1/5 (travel-time :c :b matrix :speed 10))
    (assert-error 'simple-type-error (travel-time :b :c matrix :speed "2"))))
