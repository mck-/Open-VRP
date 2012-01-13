(defsystem :open-vrp
  :description "open-vrp"
  :version "0.1"
  :author "Marc Kuo"
  :licence "LLGPL"
  :depends-on (vecto) 
  :serial t
  :components ((:file "packages")
	       (:file "class-definitions")
	       ;; utilities
	       (:file "util/simple-utils")
	       (:file "util/list")
	       (:file "util/network")
	       (:file "util/fleet")
	       (:file "util/fitness")
	       (:file "util/output")
	       (:file "util/route")
	       (:file "util/draw-solution")
	       (:file "util/algo")
	       (:file "util/constraints")
	       ;; data
	       (:file "util/var-init")
	       (:file "init-macros")
	       (:file "util/read-solomon")
	       ;; algos
	       (:file "algo/tools")
	       (:file "algo/greedy-NN")
	       (:file "algo/greedy-insertion")
	       (:file "algo/TS-utils")
	       (:file "algo/TS")	       
	       ;; test data
	       (:file "test-cases")
	       ))
