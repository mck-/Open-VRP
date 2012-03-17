(defsystem :open-vrp-lib
  :description "open-vrp-library"
  :version "0.5.2"
  :author "Marc Kuo"
  :licence "LLGPL"
  :depends-on (vecto alexandria fiveam) 
  :serial t
  :components ((:file "packages")
	       (:module :lib
			:components
			((:file "class-definitions")
			 (:file "simple-utils")
			 (:file "list")
			 (:file "network")
			 (:file "fleet")
			 (:file "fitness")
			 (:file "output")
			 (:file "route")
			 (:file "draw-solution")
			 (:file "solver")
			 (:file "constraints")
			 (:file "conditions")
			 (:file "init-macros")
			 (:file "read-solomon")
			 (:file "read-cvrp")
			 (:file "config-functions")))))

(defsystem :open-vrp
  :description "open-vrp"
  :version "0.5.2"
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
