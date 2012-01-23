;;; Sun 22 Jan, 2012
;;; ------------------
;;; A test suite for Open-VRP using FiveAM
(in-package :open-vrp)

(def-suite :suite-open-vrp)
(in-suite :suite-open-vrp)

(defmacro on-all-testcases (algo-symbol)
  (labels ((mkstr (&rest args)	   
	     (with-output-to-string (s)
	       (dolist (a args) (princ a s))))
	   (symb (&rest args)
	     (values (intern (apply #'mkstr args)))))
    `(progn
       (test ,(symb algo-symbol '-tsp) (is (solve-prob test-tsp (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-vrp) (is (solve-prob test-vrp (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-25) (is (solve-prob solomon25 (make-instance ,algo-symbol))))
       (test ,(symb algo-symbol '-100) (is (solve-prob solomon100 (make-instance ,algo-symbol)))))))

;; routine tests
(on-all-testcases 'greedy-nn)
(on-all-testcases 'greedy-append)
(on-all-testcases 'greedy-best-insertion)
(on-all-testcases 'tabu-search)


;; special tabu-search tests

(test tabu-100 (is (solve-prob solomon100 (make-instance 'tabu-search :iterations 100))))
