;;; Tue Nov 15, 2011 (c) Marc Kuo
;;; CLOS package descriptions
;;; -----------------------------
 
(defpackage :open-vrp.classes
  (:use :common-lisp)
  (:export :node
	   :vehicle
	   :problem
	   :VRP
	   :CVRP
	   :VRPTW
	   :CVRPTW
	   :drawer
	   
	   ;; algo
	   :algo
	   :name
	   :desc
	   :iterations
	   	   
	   ;; accessor functions
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
	   :problem-name
	   :problem-desc
	   :problem-network
	   :problem-dist-array
	   :problem-fleet
	   :problem-to-depot
	   :problem-drawer
	   :algo-name
	   :algo-desc
	   :algo-best-sol
	   :algo-best-fitness
	   :algo-current-sol
	   :algo-iterations
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
	:open-vrp.classes
	:vecto)
  (:import-from	:alexandria :shuffle :flatten :with-gensyms :mean :standard-deviation)
  (:export ;; simple utils
           :single
           :mac
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
	   :with-tabu-indices

	   ;; route utils
	   :empty-routep
	   :get-busy-vehicles
	   :one-destinationp
	   :insert-node
	   :append-node
	   :remove-node-at
	   :remove-node-ID
	   :last-node

	   ;; network utils
	   :distance
	   :node-distance
	   :generate-dist-array
	   :get-array-row
	   :node
	   :new-node

	   ;; fleet utils
	   :route-indices
	   :vehicle-with-node-ID
	   :total-dist
	   :vehicle
	   :new-vehicle

	   ;; constraint utils
	   :constraints-check
	   :constraintsp
	   :in-capacityp
	   :travel-time
	   :time-after-serving-node
	   :in-timep

	   ;; solver
	   :run-algo
	   :*algo-backup*
	   :solve-prob
	   :solve-plot
	   :multi-run
	   :get-best-solution-from-multi-run
	   :multi-run-algo

	   ;; output
	   :print-routes
	   :print-multi-run-stats
	   :plot-solution
	   :plot-nodes
	   :toggle-legend
	   
	   ;; conditions
	   :all-moves-tabu
	   :same-origin-destination

	   ;; init macros
	   :create-nodes
	   :create-vehicles
	   :define-problem

	   :fitness
	   ))

(defpackage :open-vrp.algo
  (:use :common-lisp
	:open-vrp.classes
	:open-vrp.util)
  (:import-from :alexandria :shuffle :flatten)  
  (:export ;; tools
	   :get-closest-vehicle
	   :get-closest-feasible-vehicle
	   :optimal-insertion
	   :fitness-before-after
	   :insertion-move
	   :get-best-insertion-move
	   :get-closest-node
	   :feasible-movep

	   ;; iterator
	   :initialize
	   :iterate
	   :iterate-more
	   :generate-moves
	   :perform-move
	   :assess-move
	   :assess-moves
	   :sort-moves
	   :select-move

	   ;; move
	   :move
	   :move-node-ID
	   :move-vehicle-ID
	   :move-index
	   :move-fitness	   

	   ;; algo-objects
	   :greedy-NN
	   :greedy-append
	   :greedy-best-insertion
	   :tabu-list
	   :tabu-search	   
	   
	   ;; Tabu Search
	   :add-to-tabu
	   :TS-best-insertion-move		   
	   :is-tabup
	   :create-candidate-list))

(defpackage :open-vrp
  (:use :common-lisp
	:open-vrp.classes
	:open-vrp.util
	:open-vrp.algo
	:fiveam)
  (:import-from	:alexandria :shuffle :flatten :with-gensyms)
  (:export :define-problem
	   :load-testcase-Solomon
	   :solve-prob
	   :iterate-more
	   :solve-plot
	   :plot-solution
	   :run!
	   :print-routes

	   ;; algos
	   :tabu-search
	   :greedy-NN
	   :greedy-append
	   :greedy-best-insertion
	   
	   ;; demos
	   :test-tsp
	   :test-vrp
	   :solomon25
	   :solomon100))