(defsystem :open-vrp-lib
  :description "open-vrp-library"
  :version "0.5.2"
  :author "Marc Kuo"
  :licence "LLGPL"
  :depends-on (vecto alexandria fiveam cl-fad) 
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