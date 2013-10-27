;;; CLOS package descriptions
;;; -----------------------------

(defpackage :open-vrp.classes
  (:use :common-lisp)
  (:export :node
           :visit
           :order
           :pitstop
           :vehicle
           :problem
           :CVRP
           :VRPTW
           :CVRPTW

           ;; algo
           :algo
           :name
           :desc
           :iterations

           ;; constructor functions
           :make-node
           :make-order
           :make-pitstop
           :make-vehicle
           :make-drawer

           ;; predicates
           :node-p
           :order-p
           :pitstop-p
           :vehicle-p
           :drawer-p

           ;; accessor functions
           :node-id
           :node-name
           :node-xcor
           :node-ycor
           :visit-node-id
           :visit-start
           :visit-end
           :visit-duration
           :order-demand
           :pitstop-start
           :pitstop-end
           :pitstop-duration

           :vehicle-id
           :vehicle-route
           :vehicle-capacity
           :vehicle-speed
           :vehicle-start-location
           :vehicle-end-location
           :vehicle-shift-start
           :vehicle-shift-end

           :problem-name
           :problem-desc
           :problem-network
           :problem-visits
           :problem-dist-matrix
           :problem-fleet
           :problem-allow-unserved
           :problem-unserved
           :problem-log-file
           :problem-log-mode

           :algo-name
           :algo-desc
           :algo-best-sol
           :algo-best-fitness
           :algo-best-iteration
           :algo-current-sol
           :algo-iterations

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
   :mac
   :while
   :aif
   :awhile
   :it
   :copy-object

   ;; list utils
   :get-min-index
   :get-max-index
   :sort-ignore-nil
   :insert-before
   :insert-at-end
   :remove-index

   ;; route utils
   :no-visits-p
   :get-busy-vehicles
   :one-destination-p
   :insert-node
   :append-node
   :remove-node-at
   :remove-node-id
   :last-visit
   :add-to-unserved
   :remove-from-unserved

   ;; network utils
   :sethash
   :alist-to-hash
   :distance
   :get-distance
   :node
   :visit-node

   ;; fleet utils
   :route-indices
   :vehicle-with-node-id
   :node-on-route-p
   :route-dist
   :total-dist
   :vehicle
   :veh-arrival-times
   :arrival-times

   ;; time utils
   :time-to-minutes
   :minutes-to-time

   ;; constraint utils
   :constraints-check
   :constraints-p
   :in-capacity-p
   :travel-time
   :time-after-visit
   :veh-in-time-p
   :in-time-p

   :fitness
   :*unserved-penalty*

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

   ;; output
   :print-routes
   :print-multi-run-stats
   :print-final-results
   :print-vrp-object
   :plot-solution
   :plot-nodes
   :print-timestamp
   :with-log-or-print
   :log-to-repl-p

   ;; conditions
   :same-origin-destination
   :distance-between-nodes-undefined
   :list-of-nils
   :index-out-of-bounds
   :unknown-log-mode
   :too-late-arrival
   :vehicle-not-found
   :no-feasible-solution))

(defpackage :open-vrp.algo
  (:use :common-lisp
        :open-vrp.classes
        :open-vrp.util)
  (:import-from :alexandria :shuffle :flatten)
  (:export
   ;; tools
   :get-optimal-insertion
   :fitness-before-after
   :get-best-insertion-move
   :get-best-insertion-move-in-vehicle
   :feasible-move-p

   ;; conditions
   :no-initial-feasible-solution
   :no-feasible-move

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
   :ts-aspiration-p
   :ts-elite-list-p
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
   :clear-tabu-list
   :is-tabu-p
   :is-tabu-move-p
   :TS-best-insertion-move
   :create-candidate-list
   :all-moves-tabu))

(defpackage :open-vrp
  (:use :common-lisp
        :open-vrp.classes
        :open-vrp.util
        :open-vrp.algo)
  (:import-from	:alexandria :shuffle :flatten :with-gensyms)
  (:export :define-problem
           :load-testcase-Solomon
           :load-tsplib-vrp-file
           :multi-run-algo
           :solve-prob
           :iterate-more
           :plot-solution
           :print-routes

           ;; utils
           :arrival-times
           :route-indices
           :algo-best-sol

           ;; algos
           :tabu-search
           :greedy-NN
           :greedy-append
           :greedy-best-insertion))

           ;; demos
           ;; :test-tsp
           ;; :test-vrp
           ;; :solomon25
           ;; :solomon100
           ;; :christofides-1
           ;; :christofides-2))
