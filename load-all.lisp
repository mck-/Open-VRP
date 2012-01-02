;;; Wed Nov 9,2011 (c) Marc Kuo
;;; Open-VRP loader 
;;; -------------------

;; packages and definitions
(ql:quickload "vecto")
(princ "; Loading 'Open-VRP'")
(load "packages")
(load "class-definitions")
(load "test-data") ;test-data

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
(load "init-macros")

;; algos
(load "algo/tools")
(load "algo/greedy-NN")
(load "algo/greedy-insertion")

(load "test-data-init") ;test-data initialization 
