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

           ;; constructor functions
           :make-node
           :make-vehicle
           :make-drawer

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
           :problem-log-file
           :problem-log-mode
           :algo-name
           :algo-desc
           :algo-best-sol
           :algo-best-fitness
           :algo-best-iteration
           :algo-current-sol
           :algo-iterations
           :algo-animatep
           :drawer-min-coord
           :drawer-max-coord
           :drawer-legendp
           :drawer-legend-x
           :drawer-legend-y
           :drawer-x-pos
           :drawer-y-pos
           :drawer-max-pix
           :drawer-filename
           :drawer-plotp))

(defpackage :open-vrp.util
  (:use :common-lisp
        :open-vrp.classes
        :vecto)
  (:import-from	:alexandria :shuffle :flatten :with-gensyms :mean :standard-deviation)
  (:import-from :cl-fad :walk-directory)
  #+sbcl (:import-from :sb-mop :class-slots :slot-definition-name)
  #+(or allegro clisp lispworks) (:import-from :clos :class-slots :slot-definition-name)
  #+cmu (:import-from :mop :class-slots :slot-definition-name)
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
   :node-on-routep
   :total-dist
   :vehicle
   :new-vehicle

   ;; constraint utils
   :constraints-check
   :constraintsp
   :in-capacityp
   :travel-time
   :time-after-serving-node
   :veh-in-timep
   :in-timep

   :fitness

   ;; solver
   :init-algo
   :run-algo
   :*algo-backup*
   :solve-prob
   :multi-run
   :get-best-solution-from-multi-run
   :multi-run-algo
   :iterate
   :iterate-more
   :*start-time*
   :*multi-run-start-time*
   :batch-run
   :print-run-results-table

   ;; output
   :print-routes
   :print-multi-run-stats
   :print-final-results
   :print-vrp-object
   :plot-solution
   :plot-nodes
   :print-timestamp
   :with-log-or-print
   :log-to-replp

   ;; conditions
   :same-origin-destination
   :list-of-nils

   ;; init macros
   :create-nodes
   :create-vehicles
   :define-problem

   ;; input
   :load-test-case-file
   :load-solomon-vrp-file
   :load-tsplib-vrp-file

   ;; config utils
   :toggle
   :toggle-legend
   :toggle-plot
   :toggle-animate
   :set-plot-file
   :set-log-mode
   :set-log-file
   :set-dist-array))

(defpackage :open-vrp.algo
  (:use :common-lisp
        :open-vrp.classes
        :open-vrp.util)
  (:import-from :alexandria :shuffle :flatten)
  (:export ;; tools
   :get-closest-vehicle
   :get-closest-feasible-vehicle
   :get-optimal-insertion
   :fitness-before-after
   :insertion-move
   :get-best-insertion-move
   :get-best-insertion-move-in-vehicle
   :get-closest-node
   :feasible-movep

   ;; iterator
   :initialize
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
   :make-insertion-move
   :make-TS-best-insertion-move

   ;; algo-objects
   :greedy-NN
   :greedy-append
   :greedy-best-insertion
   :tabu-list
   :tabu-search

   ;; Tabu Search object
   :ts-move-type
   :ts-init-heur
   :ts-aspirationp
   :ts-elite-listp
   :ts-tabu-list
   :ts-tenure
   :ts-parameter-f
   :ts-candidate-list
   :ts-stopping-condition

   ;; Tabu Search utils
   :toggle-aspiration
   :toggle-elite-list
   :add-to-tabu
   :add-move-to-tabu
   :is-tabup
   :is-tabu-movep
   :TS-best-insertion-move
   :create-candidate-list))

(defpackage :open-vrp.test
  (:use :common-lisp
        :open-vrp.classes
        :open-vrp.util
        :open-vrp.algo
        :lisp-unit)
  (:export :run!
           :*node-coords*
           ;; demos
           :test-tsp
           :test-vrp
           :solomon25
           :solomon100
           :christofides-1
           :christofides-2))

(defpackage :open-vrp
  (:use :common-lisp
        :open-vrp.classes
        :open-vrp.util
        :open-vrp.algo
        :open-vrp.test)
  (:import-from	:alexandria :shuffle :flatten :with-gensyms)
  (:export :define-problem
           :load-testcase-Solomon
           :load-tsplib-vrp-file
           :solve-prob
           :iterate-more
           :plot-solution
           :print-routes

           ;; algos
           :tabu-search
           :greedy-NN
           :greedy-append
           :greedy-best-insertion

           ;; demos
           :run!
           :test-tsp
           :test-vrp
           :solomon25
           :solomon100
           :christofides-1
           :christofides-2))
