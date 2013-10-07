(in-package :open-vrp.test)

;; Time library
;; --------------------

(define-test convert-time
  "Tests the time utility, which converts 24hrs notation to minutes since midnight."
  (:tag :time)
  (assert-equal 15 (time-to-minutes 15))
  (assert-equal 59 (time-to-minutes 59))
  (assert-equal 59 (time-to-minutes "0059"))
  (assert-equal 870 (time-to-minutes 1430))
  (assert-equal 1220 (time-to-minutes "2020"))
  (assert-equal 0 (time-to-minutes 0))
  (assert-equal 879 (time-to-minutes 1439))
  (assert-error 'simple-type-error (time-to-minutes 2600))
  (assert-error 'simple-type-error (time-to-minutes 280))
  (assert-error 'simple-type-error (time-to-minutes "-23")))
