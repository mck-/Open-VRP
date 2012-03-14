(defsystem :open-vrp
  :description "open-vrp"
  :version "0.5.1"
  :author "Marc Kuo"
  :licence "LLGPL"
  :depends-on (vecto alexandria fiveam open-vrp-lib) 
  :serial t
  :components ((:file "algo/algo-conditions")
	       (:file "algo/tools")
	       (:file "algo/iterator")
	       (:file "algo/best-insertion")
	       (:file "algo/greedy-NN")
	       (:file "algo/greedy-append")
	       (:file "algo/greedy-best-insertion")
	       (:file "algo/TS-classdef")
	       (:file "algo/TS-utils")
	       (:file "algo/TS")	       
	       ;; test data
	       (:file "test-cases/test-cases")
	       (:file "test-cases/test-suite")
	       ))
