;;; All class definitions for CLOS VRP
;;; Node, Vehicle, Algo, Problem objects are all designed to be extensible
(in-package :open-vrp.classes)

;; The node object
;; ----------------------

(defclass node ()
  ((id :reader node-id :initarg :id)
   (xcor :reader node-xcor :initarg :xcor)
   (ycor :reader node-ycor :initarg :ycor)))

(defclass node-C (node)
  ((demand :reader node-demand :initarg :demand)))

(defclass node-TW (node-C)
  ((start :reader node-start :initarg :start)
   (end :reader node-end :initarg :end)
   (duration :reader node-duration :initarg :duration)))
;; --------------------------

;; The vehicle object
;; ---------------------------

(defclass vehicle ()
  ((id :reader vehicle-id :initarg :id)
   (route :accessor vehicle-route :initarg :route :initform nil)))

(defclass vehicle-C (vehicle)
  ((capacity :reader vehicle-capacity :initarg :capacity)))

(defclass vehicle-TW (vehicle-C)
  ((speed :accessor vehicle-speed :initarg :speed :initform 1)))

;; ----------------------------

;; The problem object class
;; -----------------------
;; NOTE: The <Problem> object is also a <Solution> object interchangably.

(defclass problem ()
  ((name :reader problem-name :initarg :name)
   (desc :reader problem-desc :initarg :desc)
   (network :reader problem-network :initarg :network)
   (dist-array :accessor problem-dist-array :initarg :dist-array)
   (fleet :reader problem-fleet :initarg :fleet)
   (to-depot :accessor problem-to-depot :initarg :to-depot :initform T)
   (drawer :accessor problem-drawer :initarg :drawer)))

(defclass VRP (Problem)
  ((name :initform "VRP")
   (desc :initform "Vehicle Routing Problem")))

(defclass CVRP (VRP)
  ((name :initform "CVRP")
   (desc :initform "Capacitated Vehicle Routing Problem")))

(defclass VRPTW (CVRP)
  ((name :initform "VRPTW")
   (desc :initform "(Capacitated) Vehicle Routing Problem with Time Windows")))

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