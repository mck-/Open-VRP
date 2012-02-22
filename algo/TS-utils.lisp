;; Tabu Search utilities
;; ---------------------

(in-package :open-vrp.algo)

;; Tabu List
;; -----------------------------

;; Add
(defun add-to-tabu (ts pars)
  "Add the <Move> to the tabu-list of <tabu-search>. When tabu-list gets larger than the tenure, will prune away the oldest <Move>s on the list. Destructive."
  (let ((tenure (ts-tenure ts))
	(tl (ts-tabu-list ts)))
    (vector-push-extend pars tl)
    (when (> (length tl) tenure)
      (setf (ts-tabu-list ts) (subseq tl 1)))))

;; Check
(defun is-tabup (ts pars)
  (find pars (ts-tabu-list ts) :test 'equal))

;; Candidate Lists
;; -----------------------------

(defun improving-movep (move)
  "Returns T if move is an improving one, i.e. has a negative fitness."
  (< (move-fitness move) 0))

(defun create-candidate-list (ts sorted-moves)
  "Given a list of sorted moves, return the list with non-tabu improving moves. Will always at least return one (non-tabu) move."
  (labels ((iter (moves ans)
	     (if (or (null moves) (not (improving-movep (car moves))))
		 (nreverse ans)
		 (iter (cdr moves)
		       (if (is-tabup ts (funcall (ts-parameter-f ts) (car moves)))
			   ans
			   (push (car moves) ans)))))) ;only add if move is non-tabu
    (iter (cdr sorted-moves)
	  (list (select-move ts sorted-moves))))) ;first move is non-tabu

(defmethod remove-affected-moves ((ts tabu-search) move)
  "Given a <Tabu-search> and one <Move> (to be performed), remove all the moves from the candidate-list that do not apply anymore after the selected move is performed."
  (setf (ts-candidate-list ts)
	(remove-if #'(lambda (mv) (or (= (move-node-id mv) (move-node-id move))
				      (= (move-vehicle-ID mv) (move-vehicle-ID move))))
		   (ts-candidate-list ts))))
  
