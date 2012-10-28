(defsystem :open-vrp
  :description "open-vrp"
  :version "0.6.3"
  :author "Marc Kuo"
  :licence "LLGPL"
  :depends-on (vecto alexandria fiveam open-vrp-lib) 
  :serial t
  :components ((:module :algo
			:components
			((:file "algo-conditions")
			 (:file "tools")
			 (:file "iterator")
			 (:file "best-insertion")
			 (:file "greedy-NN")
			 (:file "greedy-append")
			 (:file "greedy-best-insertion")
			 (:file "TS-classdef")
			 (:file "TS-utils")
			 (:file "TS")))
	       (:module :test-cases
			:components
			((:file "test-cases")
			 (:file "test-suite")))))			 
