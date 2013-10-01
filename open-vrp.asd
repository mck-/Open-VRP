(defsystem :open-vrp
  :description "Framework for modeling Vehicle Routing Problems"
  :version "0.8.3"
  :author "mck- <kuomarc2@gmail.com>"
  :licence "LLGPL"
  :depends-on (#:alexandria #:open-vrp-lib)
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
                         (:file "TS")))))
