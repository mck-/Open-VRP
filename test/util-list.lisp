(in-package :open-vrp.test)

;; Simple list utils
;; --------------------

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

(define-test insert-before
  "Test insert-before util, with edge cases and index out of bound errors"
  (:tag :util)
  (assert-equal '(1 2 3) (insert-before 2 1 '(1 3)))
  (assert-equal '(1 2 3) (insert-before 1 0 '(2 3)))
  (assert-equal '(1 2 3) (insert-before 3 2 '(1 2)))
  (assert-equal '(1 2 3 4 5 6) (insert-before 5 4 '(1 2 3 4 6)))
  (assert-error 'index-out-of-bounds (insert-before 2 -1 '(1 3)))
  (assert-error 'index-out-of-bounds (insert-before 2 3 '(1 3)))
  (assert-equal '(1 "hello" 2) (insert-before "hello" 1 '(1 2))))

(define-test insert-at-end
  "Test insert-at-end util"
  (:tag :util)
  (assert-equal '(1 2 3) (insert-at-end 3 '(1 2)))
  (assert-equal '(1 2 nil) (insert-at-end nil '(1 2)))
  (assert-equal '(1) (insert-at-end 1 '()))
  (assert-equal '(1 2 3 "hello") (insert-at-end "hello" '(1 2 3))))

(define-test remove-index
  "Test remove index with edge cases"
  (:tag :util)
  (assert-equal '(1 2 3) (remove-index 3 '(1 2 3 4)))
  (assert-equal '(1 2 3) (remove-index 0 '(0 1 2 3)))
  (assert-error 'index-out-of-bounds (remove-index -1 '(1 2 3 4)))
  (assert-error 'index-out-of-bounds (remove-index 5 '(1 2 3 4)))
  (assert-equal (list '(1 2 3) 4) (multiple-value-bind (list item)
                                      (remove-index 3 '(1 2 3 4))
                                    (list list item))))
