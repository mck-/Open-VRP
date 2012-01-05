;;; Wed Nov 9,2011 (c) Marc Kuo
;;; Open-VRP loader 
;;; -------------------

;; packages and definitions
(ql:quickload "vecto")
(princ "; Loading 'Open-VRP'")
(load "packages")
(load "class-definitions")

;; utilities
(load "util/simple-utils")
(load "util/list")
(load "util/network")
(load "util/fleet")
(load "util/fitness")
(load "util/output")
(load "util/route")
(load "util/draw-solution")
(load "util/algo")

;; data
(load "util/var-init")
(load "init-macros")
(load "util/read-solomon")

;; algos
(load "algo/tools")
(load "algo/greedy-NN")
(load "algo/greedy-insertion")
(load "algo/TS")

(load "test-cases") ;test-data initialization 
