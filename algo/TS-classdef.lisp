;; Tabu Search - added Mon Dec 12, 2011
;; ---------------------------------------
;; - it is possible to define your own moves and initial solution heuristic.
(in-package :open-vrp.algo)

(defclass tabu-search (algo)
  ((name :initform "Tabu Search")
   (desc :initform "Simple Tabu Search heuristic, see (Kuo 2011) for details.")
   (moves :accessor tabu-search-moves :initarg :moves :initform 'TS-best-insertion-move)
   (init-heur :accessor tabu-search-init-heur :initarg :init-heur :initform 'greedy-best-insertion)
   (iterations :initform 5)
   (animate :accessor tabu-search-animate :initarg :animate :initform nil)
   (tabu-list :accessor tabu-search-tabu-list :initarg :tabu-list :initform (make-instance 'tabu-list))
   (candidate-list :accessor tabu-search-candidate-list :initarg :candidate-list :initform nil)
   (runs :accessor tabu-search-runs :initarg :runs :initform 1)))

;; ----------------------------

;; Tabu List
;; -----------------------------
(defclass tabu-list ()
  ((tabu :accessor tabu-list-tabu :initarg :tabu :initform nil)
   (tenure :accessor tabu-list-tenure :initarg :tenure :initform 25)
   (aspiration :accessor tabu-list-aspiration :initarg :aspiration :initform T)))

;; ---------------------------

;; Tabu search move
;; ---------------------------

;; Given a node and a vehicle, TS-best-insertion inserts it in the best possible position
(defclass TS-best-insertion-move (move)
  ((node-ID :accessor move-node-ID :initarg :node-ID)
   (vehicle-ID :accessor move-vehicle-ID :initarg :vehicle-ID)))
