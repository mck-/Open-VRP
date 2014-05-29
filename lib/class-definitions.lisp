;;; All class definitions for CLOS VRP
;;; Node, Vehicle, Problem, Drawer and Algo objects
(in-package :open-vrp.classes)

;; The node object
;; ----------------------

(defstruct node
  (id 0 :type fixnum :read-only t)
  (xcor 0 :read-only t)
  (ycor 0 :read-only t)
  (demand 0 :type fixnum :read-only t)
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (duration 0 :type fixnum :read-only t))
;; --------------------------

;; The vehicle object
;; ---------------------------

(defstruct vehicle
  (id 0 :type fixnum :read-only t)
  route
  (capacity 0 :type fixnum :read-only t)
  (speed 1 :read-only t))
;; ----------------------------

;; The problem object class
;; -----------------------
;; NOTE: The <Problem> object is also a <Solution> object interchangably.

(defclass problem ()
  ((name :reader problem-name :initarg :name :initform "VRP")
   (desc :reader problem-desc :initarg :desc :initform "Vehicle Routing Problem" :allocation :class)
   (network :reader problem-network :initarg :network)
   (dist-array :accessor problem-dist-array :initarg :dist-array :initform nil)
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
   (desc :reader algo-desc :initarg :desc :allocation :class)
   (best-sol :accessor algo-best-sol :initarg :best-sol :initform nil)
   (best-fitness :accessor algo-best-fitness :initarg :best-fitness :initform nil)
   (best-iteration :accessor algo-best-iteration :initform 0)
   (current-sol :accessor algo-current-sol :initarg :current-sol :initform nil)
   (iterations :accessor algo-iterations :initarg :iterations)
   (animatep :accessor algo-animatep :initarg :animatep :initform nil)))
