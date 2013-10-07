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

(define-test convert-minutes
  "Tests the reverse, converting minutes since midnight to 24hrs notation. Expects numbers"
  (:tag :time)
  (assert-equal 15 (minutes-to-time 15))
  (assert-equal 59 (minutes-to-time 59))
  (assert-equal 1430 (minutes-to-time 870))
  (assert-equal 2020 (minutes-to-time 1220))
  (assert-equal 0 (minutes-to-time 0))
  (assert-equal 1439 (minutes-to-time 879))
  (assert-error 'simple-type-error (minutes-to-time 1440))
  (assert-error 'simple-type-error (minutes-to-time -1))
  (assert-error 'simple-type-error (minutes-to-time "23")))
