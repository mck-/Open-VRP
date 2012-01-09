;;; Thu Nov 10, 2011 (c) Marc Kuo
;;; -------------------
;;; All class definitions for CLOS VRP
;;; Node, Vehicle, Algo, Problem objects are all designed to be extensible
(in-package :open-vrp.classess)

;; Thu Oct 27, 2011
;; The network object
;; ----------------------
;; 1. An object that encapsulates the nodes (starting with ID 0)
;; 2. Holds a distance-matrix variable for easy distance lookup between the nodes

(defclass network ()
  ((nodes :accessor network-nodes :initarg :nodes)
   (dist-table :accessor network-dist-table :initarg :dist-table)))

(defclass node ()
  ((id :reader node-id :initarg :id)
   (xcor :reader node-xcor :initarg :xcor)
   (ycor :reader node-ycor :initarg :ycor)))
;; --------------------------

;; A vehicle class - basic version for TSP/VRP (non-capacitated).
;; ---------------------------
;; id = integer
;; route = list<nodes>

(defclass vehicle ()
  ((id :reader vehicle-id :initarg :id)
   (route :accessor vehicle-route :initarg :route :initform nil)))

;; Class that holds the list of vehicles, which make up the fleet.
(defclass fleet ()
  ((vehicles :accessor fleet-vehicles :initarg :vehicles)))

;; Not required in simple fleets.
;   (from-depot :accessor fleet-from-depot :initarg :from-depot :initform T)
;   (to-depot :accessor fleet-to-depot :initarg :to-depot :initform T)))

;; ----------------------------

;; The problem object class
;; -----------------------
;; Uses: util-geometry.lisp (create-network) - util-vehicle.lisp (create-fleet)
;; Requires: *node-coords* to be the map of nodes
;; NOTE: The <Problem> object is also a <Solution> object interchangably.

(defclass problem ()
  ((name :reader problem-name :initarg :name)
   (desc :reader problem-desc :initarg :desc)
   (network :reader problem-network :initarg :network)
   (fleet :reader problem-fleet :initarg :fleet)
   (drawer :accessor problem-drawer :initarg :drawer)))

(defclass TSP (problem)
  ((name :initform "TSP")
   (desc :initform "Simple Traveling Salesman Problem")))

(defclass VRP (TSP)
  ((name :initform "VRP")
   (desc :initform "Vehicle Routing Problem")))

;; ----------------------

;; Mon Dec 5, 2011   
;; The drawing object class
;; --------------------------------
(defclass drawer ()
  ((min-coord :accessor drawer-min-coord :initarg :min-coord)
   (max-coord :accessor drawer-max-coord :initarg :max-coord)
   (x-pos :accessor drawer-x-pos :initarg :x-pos :initform 0)
   (y-pos :accessor drawer-y-pos :initarg :y-pos :initform 0)
   (max-pix :accessor drawer-max-pix :initarg :max-pix :initform 1000)
   (legend :accessor drawer-legend :initarg :legend :initform T)
   (legend-x :accessor drawer-legend-x :initarg :legend-x :initform 100)
   (legend-y :accessor drawer-legend-y :initarg :legend-y :initform 900)
   (filename :accessor drawer-filename :initarg :filename :initform "testing.png")))

;; -----------------------------

;; Algo class
;; -----------------------

(defclass algo ()
  ((name :reader algo-name :initarg :name)
   (desc :reader algo-desc :initarg :desc)
   (best-sol :accessor algo-best-sol :initform nil)
   (best-fitness :accessor algo-best-fitness :initform nil)
   (current-sol :accessor algo-current-sol :initform nil)
   (iterations :accessor algo-iterations :initarg :iterations)))

(defclass greedy-NN (algo)
  ((name :initform "Greedy NN-algo")
   (desc :initform "Nearest Neighborhood algo; from base/random, select next closest one")))

(defclass greedy-insertion (algo)
  ((name :initform "Greedy Insertion heuristic")
   (desc :initform "Random greedy insertion heuristic; insert nodes to closest vehicle successively")))

;; Tabu Search
;; ---------------------------------------
;; added Mon 9 Jan, 2012
;; Tabu List object

(defclass tabu-list ()
  ((tabu :accessor tabu-list-tabu :initarg :tabu :initform nil)
   (tenure :accessor tabu-list-tenure :initarg :tenure :initform 25)
   (aspiration :accessor tabu-list-aspiration :initarg :aspiratoin :initform T)))

;; added Mon Dec 12, 2011
;; note Thu Dec 15, 2011 - perhaps add slots that define the TS? moves/init sol/ts parameters..
;; .. it is possible to use 'string in slot, and use slot value to make-instance (for moves)
;; - it is possible to define your own moves and initial solution.
(defclass tabu-search (algo)
  ((name :initform "Tabu Search")
   (desc :initform "Simple Tabu Search heuristic, see (Kuo 2011) for details.")
   (moves :accessor tabu-search-moves :initarg :moves :initform 'TS-best-insertion-move)
   (init-heur :accessor tabu-search-init-heur :initarg :init-heur :initform 'greedy-insertion)
   (iterations :initform 5)
   (animate :accessor tabu-search-animate :initarg :animate :initform nil)
   (tabu-list :accessor tabu-search-tabu-list :initarg :tabu-list :initform (make-instance 'tabu-list))))

;; -------------------------------

;; added Thu Dec 15, 2011
(defclass move ()
  ((fitness :accessor move-fitness :initarg :fitness)))

;; Given a node and a vehicle, TS-best-insertion inserts it in the best possible position
(defclass TS-best-insertion-move (move)
  ((node-ID :accessor move-node-ID :initarg :node-ID)
   (vehicle-ID :accessor move-vehicle-ID :initarg :vehicle-ID)))

;; For intra-move insertion (algo/tools.lisp)
(defclass insertion-move (move) 
  ((node-ID :accessor move-node-ID :initarg :node-ID)
   (vehicle-ID :accessor move-vehicle-ID :initarg :vehicle-ID)
   (index :accessor move-index :initarg :index)))
