(defsystem :open-vrp-test
  :description "open-vrp-test"
  :author "mck- <kuomarc2@gmail.com>"
  :licence "LLGPL"
  :depends-on (#:lisp-unit #:open-vrp)
  :serial t
  :components ((:module :test
                        :components
                        ((:file "test-package")
                         (:file "util-list")
                         (:file "util-network")
                         (:file "util-fleet")
                         (:file "util-route")
                         (:file "constraints")
                         (:file "time")
                         (:file "algo-tools")
                         (:file "algo-best-insertion")
                         (:file "greedy-best-insertion")
                         (:file "tabu-search-utils")))))
