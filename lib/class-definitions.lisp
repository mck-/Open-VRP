;;; All class definitions for CLOS VRP
;;; Node, Vehicle, Problem, Drawer and Algo objects
(in-package :open-vrp.classes)

;; The node object
;; ----------------------

(defstruct node
  "The super location class, defines a place with optionally x/y coords for plotting"
  (id (gensym) :type symbol :read-only t)
  (name "Nameless Node" :type string)
  xcor
  ycor)

(defstruct visit
  "These are the actual places on the network that need to be visited, and can be depots, orders, or breaks -- they are linked to a location by node ID"
  (node-id nil :type keyword :read-only t))

(defstruct (order (:include visit))
  "Order that needs to be visited."
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (duration 0 :type fixnum :read-only t))

  ;; Todo:
  ;; (demand 0 :type fixnum :read-only t)
  ;; type

(defstruct (depot (:include visit))
  "Starting or ending location for a vehicle")

(defstruct (break (:include visit))
  "Location that a break may be taken at")

;; --------------------------

;; The vehicle object
;; ---------------------------

(defstruct vehicle
  (id (gensym) :type fixnum :read-only t)
  route
  type
  (start-depot :nil :type symbol :read-only t)
  (end-depot :nil :type symbol :read-only t)
  (speed 1 :read-only t))

  ;; Todo:
  ;; (shift-start 0 :type fixnum :read-only t)
  ;; (shift-end 2359 :type fixnum :read-only t)
  ;; (break-start 1100 :type fixnum :read-only t)
  ;; (break-start 1400 :type fixnum :read-only t)
  ;; (break-duration 100 :type fixnum :read-only t)
  ;; (capacity 0 :type fixnum :read-only t)

;; ----------------------------

;; The problem object class
;; -----------------------
;; NOTE: The <Problem> object is also a <Solution> object interchangably.

(defclass problem ()
  ((name :reader problem-name :initarg :name :initform "VRP")
   (desc :reader problem-desc :initarg :desc :initform "Vehicle Routing Problem")
   (network :reader problem-network :initarg :network)
   (dist-matrix :accessor problem-dist-matrix :initarg :dist-matrix :initform nil)
   (fleet :reader problem-fleet :initarg :fleet)
   (to-depot :accessor problem-to-depot :initarg :to-depot :initform T)
   (drawer :accessor problem-drawer :initarg :drawer :initform nil)
   (log-file :accessor problem-log-file :initarg :log-file :initform nil)
   (log-mode :accessor problem-log-mode :initarg :log-mode :initform 1)))
;; log-mode 0 = off, 1 = output file, 2 = REPL

(defclass CVRP (problem)
  ((name :initform "CVRP")
   (desc :initform "Capacitated Vehicle Routing Problem")))

(defclass VRPTW (problem)
  ((name :initform "VRPTW")
   (desc :initform "Vehicle Routing Problem with Time Windows")))

(defclass CVRPTW (CVRP VRPTW)
  ((name :initform "CVRPTW")
   (desc :initform "Capacitated Vehicle Routing Problem with Time Windows")))

;; ----------------------

;; The drawing object class
;; --------------------------------
(defstruct drawer
  min-coord
  max-coord
  (x-pos 0)
  (y-pos 0)
  (max-pix 1000)
  (legendp T)
  (legend-x 100)
  (legend-y 900)
  filename
  (plotp nil))

;; -----------------------------

;; Algo class
;; -----------------------

(defclass algo ()
  ((name :reader algo-name :initarg :name)
   (desc :reader algo-desc :initarg :desc)
   (best-sol :accessor algo-best-sol :initarg :best-sol :initform nil)
   (best-fitness :accessor algo-best-fitness :initarg :best-fitness :initform nil)
   (best-iteration :accessor algo-best-iteration :initform 0)
   (current-sol :accessor algo-current-sol :initarg :current-sol :initform nil)
   (iterations :accessor algo-iterations :initarg :iterations)
   (animatep :accessor algo-animatep :initarg :animatep :initform nil)))
