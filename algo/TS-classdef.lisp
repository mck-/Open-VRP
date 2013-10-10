;; Tabu Search Classes
;; ---------------------------------------
;; - it is possible to define your own moves and initial solution heuristic.
(in-package :open-vrp.algo)

;; Queue data structure for Tabu List -- source: http://rosettacode.org/wiki/FIFO#Common_Lisp
;; ------------------
(defstruct (queue (:constructor %make-queue))
  (items '() :type list)
  (tail '() :type list))

(defun make-queue ()
  "Returns an empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))

(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))))

(defun dequeue (queue)
  "Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
    (error "Cannot dequeue from empty queue.")
    (pop (queue-items queue))))

;; ------------------------

;; Default stopping condition
;; -------------------------

(defun stopping-condition-p (ts)
  "Given a <tabu-search>, tests whether the number of iterations since the best solution was found is larger than triple tenure value. This is an indicator of cycling behaviour. Minimum 20 iterations in case tenure is smaller than 10. Usefull for many multi-runs."
  (let ((iters (- (algo-best-iteration ts) (algo-iterations ts))))
    (and (> iters 20)
   (> iters (* 3 (ts-tenure ts))))))


(defclass tabu-search (algo)
  ((name :initform "Tabu Search")
   (desc :initform "A local search that escapes local optima by means of declaring certain moves tabu.")
   (move-type :accessor ts-move-type :initarg :move-type :initform 'TS-best-insertion-move)
   (init-heur :accessor ts-init-heur :initarg :init-heur :initform 'greedy-best-insertion)
   (iterations :initform 20)
   (aspiration-p :accessor ts-aspiration-p :initarg :aspirationp :initform T)
   (elite-list-p :accessor ts-elite-list-p :initarg :elite-listp :initform T)
   (tabu-list :accessor ts-tabu-list :initarg :tabu-list :initform (make-queue))
   (tabu-tenure :accessor ts-tenure :initarg :tabu-tenure :initform 15)
   (tabu-parameter-f :accessor ts-parameter-f :initarg :tabu-parameter-f :initform #'ts-pars-n)
   (candidate-list :accessor ts-candidate-list :initarg :candidate-list :initform nil)
   (stopping-condition :accessor ts-stopping-condition :initarg :stopping-condition :initform #'stopping-condition-p)))

;; ----------------------------

;; Tabu search move types
;; ---------------------------

;; Given a node and a vehicle, TS-best-insertion inserts it in the best possible position
(defstruct (TS-best-insertion-move (:include insertion-move)))
