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

(define-test get-min-index
  "Test the get-min-index util"
  (:tag :util)
  (assert-equal 3 (get-min-index '(0 2 3 -5 1)))
  (assert-equal 3 (get-min-index '(0 nil 3 -5 1)))
  (assert-error 'list-of-nils (get-min-index '(nil nil nil)))
  (assert-equal '(5 1) (multiple-value-bind (i val)
                           (get-min-index '(nil 10 nil 203948 3 1))
                         (list i val))))

(define-test get-max-index
  "Test the get-max-index util"
  (:tag :util)
  (assert-equal 2 (get-max-index '(0 2 3 -5 1)))
  (assert-equal 2 (get-max-index '(0 nil 3 -5 1)))
  (assert-error 'list-of-nils (get-max-index '(nil nil nil)))
  (assert-equal '(3 203948) (multiple-value-bind (i val)
                                (get-max-index '(nil 10 nil 203948 3 1))
                              (list i val))))

(define-test sum-list
  "Test sum over list util"
  (:tag :util)
  (assert-equal 5 (sum '(1 4 0)))
  (assert-equal 5 (sum '(1 4 0 -1 -5 6)))
  (assert-error 'expect-number (sum '(nil 1 4 0)))
  (assert-error 'expect-number (sum '("hello" 1 4 0))))
