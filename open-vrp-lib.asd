(defsystem :open-vrp-lib
  :description "open-vrp-library"
  :version "0.3"
  :author "Marc Kuo"
  :licence "LLGPL"
  :depends-on (vecto alexandria fiveam) 
  :serial t
  :components ((:file "packages")
	       (:file "class-definitions")
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
	       (:file "lib/read-solomon")))
	       