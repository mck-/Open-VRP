;; Tabu Search utilities
;; ---------------------

(in-package :open-vrp.algo)

;; Misc
;; ---------------------------

;; (defun toggle-aspiration (ts)
;;   (toggle (ts-aspirationp ts)))

;; (defun toggle-elite-list (ts)
;;   (toggle (ts-elite-listp ts)))

;; Tabu List
;; -----------------------------

;; Add
(defun add-to-tabu (ts pars)
  "Add pars to the tabu-list of <tabu-search>. Expects pars to be a list when more than one parameter is recorded. When tabu-list gets larger than the tenure, will prune away the oldest pars on the list. Destructive."
  (enqueue pars (ts-tabu-list ts))
  (when (> (length (queue-items (ts-tabu-list ts))) (ts-tenure ts))
    (dequeue (ts-tabu-list ts))))

(defun add-move-to-tabu (ts mv)
  "Adds <Move> to tabu-list of <tabu-search>. Calls function held in <ts>'s :tabu-parameter-f slot."
  (add-to-tabu ts (funcall (ts-parameter-f ts) mv)))

;; Clear tabu list
(defun clear-tabu-list (ts)
  "Given a <tabu-search>, erase everything that is on the tabu-list. A useful restart in case all moves were declared tabu."
  (setf (ts-tabu-list ts) (make-queue)))

;; Check
(defun is-tabu-p (ts pars)
  "Given pars, checks if on tabu list of <tabu-search>"
  (find pars (queue-items (ts-tabu-list ts)) :test 'equal))

(defun is-tabu-move-p (ts mv)
  "Given a <Move>, checks if the parameters returned by calling :tabu-parameter-f are recorded on the list."
  (is-tabu-p ts (funcall (ts-parameter-f ts) mv)))

;; --------------------------

;; Candidate Lists
;; -----------------------------

(defun improving-move-p (move)
  "Returns T if move is an improving one, i.e. has a negative fitness."
  (< (move-fitness move) 0))

(defun create-candidate-list (ts sorted-moves)
  "Given a list of sorted moves, return the list with non-tabu improving moves. Will always at least return one (non-tabu) move."
  (labels ((iter (moves ans)
             (if (or (null moves) (and ans (not (improving-move-p (car moves)))))
                 (or (nreverse ans) (list (car sorted-moves)))
                 (iter (cdr moves)
                       (if (is-tabu-move-p ts (car moves))
                           ans
                           (push (car moves) ans)))))) ;only add if move is non-tabu
    (iter sorted-moves '())))

(defmethod remove-affected-moves ((ts tabu-search) move)
  "Given a <Tabu-search> and one <Move> (to be performed), remove all the moves from the candidate-list that do not apply anymore after the selected move is performed."
  (let ((sol (algo-current-sol ts)))
    (setf (ts-candidate-list ts)
          (remove-if #'(lambda (mv)
                         (or (eq (move-node-id mv) (move-node-id move))
                             (eq (move-vehicle-id mv) (move-vehicle-id move))
                             (eq (route-from mv sol)
                                 (route-from move sol))
                             (eq (route-to mv sol)
                                 (route-from move sol))))
                     (ts-candidate-list ts)))))
