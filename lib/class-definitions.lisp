;;; All class definitions for CLOS VRP
;;; Node, Vehicle, Problem, Drawer and Algo objects
(in-package :open-vrp.classes)

;; The node object
;; ----------------------

(defclass node ()
  ((id :reader node-id :initarg :id)
   (xcor :reader node-xcor :initarg :xcor :initform nil)
   (ycor :reader node-ycor :initarg :ycor :initform nil)
   (demand :reader node-demand :initarg :demand)
   (start :reader node-start :initarg :start)
   (end :reader node-end :initarg :end)
   (duration :reader node-duration :initarg :duration :initform 0)))
;; --------------------------

;; The vehicle object
;; ---------------------------

(defclass vehicle ()
  ((id :reader vehicle-id :initarg :id)
   (route :accessor vehicle-route :initarg :route)
   (capacity :reader vehicle-capacity :initarg :capacity)
   (speed :accessor vehicle-speed :initarg :speed :initform 1)))
;; ----------------------------

;; The problem object class
;; -----------------------
;; NOTE: The <Problem> object is also a <Solution> object interchangably.

(defclass problem ()
  ((name :reader problem-name :initarg :name :initform "VRP")
   (desc :reader problem-desc :initarg :desc :initform "Vehicle Routing Problem")
   (network :reader problem-network :initarg :network)
   (dist-array :accessor problem-dist-array :initarg :dist-array :initform nil)
   (fleet :reader problem-fleet :initarg :fleet)
   (to-depot :accessor problem-to-depot :initarg :to-depot :initform T)
   (drawer :accessor problem-drawer :initarg :drawer :initform nil)
   (log-file :accessor problem-log-file :initarg :log-file :initform nil)
   (log-mode :accessor problem-log-mode :initarg :log-filep :initform 1)))
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

;; Mon Dec 5, 2011   
;; The drawing object class
;; --------------------------------
(defclass drawer ()
  ((min-coord :accessor drawer-min-coord :initarg :min-coord)
   (max-coord :accessor drawer-max-coord :initarg :max-coord)
   (x-pos :accessor drawer-x-pos :initarg :x-pos :initform 0)
   (y-pos :accessor drawer-y-pos :initarg :y-pos :initform 0)
   (max-pix :accessor drawer-max-pix :initarg :max-pix :initform 1000)
   (legendp :accessor drawer-legendp :initarg :legendp :initform T)
   (legend-x :accessor drawer-legend-x :initarg :legend-x :initform 100)
   (legend-y :accessor drawer-legend-y :initarg :legend-y :initform 900)
   (filename :accessor drawer-filename :initarg :filename :initform "testing.png")
   (plotp :accessor drawer-plotp :initarg :plotp :initform T)))

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