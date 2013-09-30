(in-package :open-vrp.test)

;; Simple list utils
;; --------------------

(define-test get-min
  "Test the get-min util"
  (:tag :util)
  (assert-equal -5 (get-min '(0 2 3 -5 1)))
  (assert-equal -5 (get-min '(0 nil 3 -5 1)))
  (assert-error 'list-of-nils (get-min '(nil nil nil)))
  (assert-equal 1 (get-min '(nil 10 nil 203948 3 1))))

(define-test get-max
  "Test the get-max util"
  (:tag :util)
  (assert-equal 3 (get-max '(0 2 3 -5 1)))
  (assert-equal 3 (get-max '(0 nil 3 -5 1)))
  (assert-error 'list-of-nils (get-max '(nil nil nil)))
  (assert-equal 203948 (get-max '(nil 10 nil 203948 3 1))))
