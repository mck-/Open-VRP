;; Tabu Search Classes
;; ---------------------------------------
;; - it is possible to define your own moves and initial solution heuristic.
(in-package :open-vrp.algo)

(defclass tabu-search (algo)
  ((name :initform "Tabu Search")
   (desc :initform "A local search that escapes local optima by means of declaring certain moves tabu.")
   (move-type :accessor ts-move-type :initarg :move-type :initform 'TS-best-insertion-move)
   (init-heur :accessor ts-init-heur :initarg :init-heur :initform 'greedy-best-insertion)
   (iterations :initform 20)
   (animate :accessor ts-animate :initarg :animate :initform nil)
   (aspiration :accessor ts-aspiration :initarg :aspiration :initform T)
   (elite-list :accessor ts-elite-list :initarg :elite-list :initform T)
   (tabu-list :accessor ts-tabu-list :initarg :tabu-list :initform (make-array 10 :fill-pointer 0))
   (tabu-tenure :accessor ts-tenure :initarg :tabu-tenure :initform 5)
   (tabu-parameter-f :accessor ts-parameter-f :initarg :tabu-parameter-f :initform #'(lambda (mv) (list (move-node-id mv) (move-vehicle-id mv))))
   (candidate-list :accessor ts-candidate-list :initarg :candidate-list :initform nil)))

;; ----------------------------

;; Tabu search move types
;; ---------------------------

;; Given a node and a vehicle, TS-best-insertion inserts it in the best possible position
(defclass TS-best-insertion-move (move)
  ((node-ID :accessor move-node-ID :initarg :node-ID)
   (vehicle-ID :accessor move-vehicle-ID :initarg :vehicle-ID)))
