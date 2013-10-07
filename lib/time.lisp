(in-package :open-vrp.util)

;; Time library
;; --------------------
;; Converts between 24hr notation and number of minutes since midnight
;; Accepts both string or number notation, (from 0 to 2359) and returns number from 0 <= x < 1440
;; e.g.:
;; (time-to-minutes 500) -> 300
;; (time-to-minutes 1430) -> 870
;; (time-to-minutes 20) -> 20
;; (time-to-minutes 2020) -> 1220

(defun time-to-minutes (time)
  "Given a time (either string or number) between 0000 and 2359, return the number of minutes since midnight."
  (let ((number (if (stringp time) (parse-integer time) time)))
    (check-type number (integer 0 2359))
    (let ((hour (floor (/ number 100)))
          (min (mod number 100)))
      (check-type hour (integer 0 23))
      (check-type min (integer 0 59))
      (+ (* hour 60) min))))

(defun minutes-to-time (minutes)
  "Given number of minutes since midnight, convert it into a number in 24hrs notation format"
  (check-type minutes (integer 0 1439))
  (let ((hour (floor (/ minutes 60)))
        (min (mod minutes 60)))
    (+ (* hour 100) min)))
