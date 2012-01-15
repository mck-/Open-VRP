;;; Tue Nov 15, 2011 (c) Marc Kuo
;;; CLOS package descriptions
;;; -----------------------------
 
(defpackage :open-vrp.classess
  (:use :common-lisp)
  (:export :network
	   :node
	   :node-C
	   :node-TW
	   :vehicle
	   :vehicle-C
	   :vehicle-TW
	   :fleet
	   :problem
	   :TSP
	   :VRP
	   :CVRP
	   :VRPTW
	   :algo
	   :greedy-NN
	   :greedy-insertion
	   :tabu-list
	   :tabu-search
	   :move
	   :TS-best-insertion-move
	   :insertion-move
	   :drawer
	   
	   ;; accessor functions
	   :network-nodes
	   :network-dist-table
	   :node-id
	   :node-xcor
	   :node-ycor
	   :node-demand
	   :node-start
	   :node-end
	   :node-duration
	   :vehicle-id
	   :vehicle-route
	   :vehicle-capacity
	   :vehicle-speed
	   :fleet-vehicles
	   :fleet-from-depot
	   :fleet-to-depot
	   :problem-name
	   :problem-desc
	   :problem-network
	   :problem-fleet
	   :problem-drawer
	   :algo-name
	   :algo-desc
	   :algo-best-sol
	   :algo-best-fitness
	   :algo-current-sol
	   :algo-iterations
	   :tabu-list-tabu
	   :tabu-list-tenure
	   :tabu-search-tabu-list
	   :tabu-search-moves
	   :tabu-search-init-heur
	   :tabu-search-animate
	   :tabu-search-candidate-list
	   :move-node-ID
	   :move-vehicle-ID
	   :move-index
	   :move-fitness
	   :drawer-min-coord
	   :drawer-max-coord
	   :drawer-legend
	   :drawer-legend-x
	   :drawer-legend-y
	   :drawer-x-pos
	   :drawer-y-pos
	   :drawer-max-pix
	   :drawer-filename))

(defpackage :open-vrp.util
  (:use :common-lisp
	:open-vrp.classess)
  (:export ;; simple utils
           :single
           :mac
           :flatten
	   :mapa-b
	   :map1-n
	   :map0-n
	   :while
	   :aif
	   :awhile
	   :it
	   :sum
	   :max-car
	   :max-cdr
	   :copy-object

	   ;; list utils
	   :get-min
	   :get-max
	   :get-min-index
	   :get-max-index
	   :sort-ignore-nil
	   :insert-before
	   :insert-at-end
	   :remove-index
	   :apply-on-index
	   :enumerate-interval
	   :shuffle-pool
	   :random-list-permutation

	   ;; route utils
	   :change-route
	   :insert-node
	   :append-node
	   :remove-node-at
	   :remove-node-ID
	   :last-node
	   :closest-node
	   :move-node
	   :get-route-with

	   ;; network utils
	   :distance
	   :node-distance
	   :generate-dist-array
	   :get-array-row
	   :node
	   :coords
	   :dist-table
	   :create-network

	   ;; fleet utils
	   :route-indices
	   :vehicle-with-node
	   :total-dist
	   :vehicle
	   :create-fleet

	   ;; constraint utils
	   :constraintsp
	   :in-capacityp
	   :node-fit-in-vehiclep

	   :fitness
	   ))


(defpackage :open-vrp.output
  (:use :common-lisp
	:open-vrp.classess
	:open-vrp.util
   	:vecto)
  (:export :print-routes
	   :plot-solution
	   :plot-nodes
	   :toggle-legend))

(defpackage :open-vrp.algo
  (:use :common-lisp
	:open-vrp.classess
	:open-vrp.util
	:open-vrp.output)
  (:export :run-algo
	   :*algo-backup*
	   :solve-prob
	   :solve-plot
	   :re-init
	   :get-closest-vehicle
	   :fitness-before-after
	   :initialize
	   :iterate
	   :iterate-more
	   :generate-moves
	   :perform-move
	   :assess-move
	   :assess-moves
	   :sort-moves
	   :select-move
	   :get-best-insertion-move
	   :add-to-tabu
	   :is-tabup
	   :create-candidate-list))

(defpackage :open-vrp
  (:use :common-lisp
	:open-vrp.classess
	:open-vrp.util
	:open-vrp.algo
	:open-vrp.output)
  (:export :define-problem
	   :load-testcase-Solomon))