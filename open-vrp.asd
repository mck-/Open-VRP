(defsystem :open-vrp
  :description "open-vrp"
  :version "0.3"
  :author "Marc Kuo"
  :licence "LLGPL"
  :depends-on (vecto alexandria fiveam) 
  :serial t
  :components ((:file "packages")
	       (:file "class-definitions")
	       ;; library
	       (:file "lib/simple-utils")
	       (:file "lib/list")
	       (:file "lib/network")
	       (:file "lib/fleet")
	       (:file "lib/fitness")
	       (:file "lib/output")
	       (:file "lib/route")
	       (:file "lib/draw-solution")
	       (:file "lib/solver")
	       (:file "lib/constraints")
	       (:file "lib/conditions")
	       (:file "lib/init-macros")
	       (:file "lib/read-solomon")
	       (:file "lib/read-cvrp")
	       ;; algos
	       (:file "algo/algo-conditions")
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
	       (:file "test-cases")
	       (:file "test-suite")
	       ))
