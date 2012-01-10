;; Mon Jan 9, 2011
;; Tabu Search utilities
;; ---------------------

(in-package open-vrp.algo)

;; Tabu List
;; -----------------------------

;; Add
(defgeneric add-to-tabu (obj move)
  (:method (obj move) "add-to-tabu: method for <Algo>/<Tabu-list> or <Move> is not defined!")
  (:documentation "Put the <Move> on the :tabu-list of <Algo>, which is a <Tabu-list> object."))

(defmethod add-to-tabu ((tl tabu-list) (mv ts-best-insertion-move))
  (let ((tenure (tabu-list-tenure tl)))
    (push mv (tabu-list-tabu tl))
    (when (> (length (tabu-list-tabu tl)) tenure)
      (setf (tabu-list-tabu tl) (subseq (tabu-list-tabu tl) 0 tenure)))))

(defmethod add-to-tabu ((ts tabu-search) (mv ts-best-insertion-move))
  (add-to-tabu (tabu-search-tabu-list ts) mv))

;; Check
(defgeneric is-tabup (obj move)
  (:method (obj move) "is-tabup: method for <Algo>/<Tabu-list> or <Move> is not defined!")
  (:documentation "Returns <Move> if on the :tabu-list, NIL otherwise"))

(defmethod is-tabup ((tl tabu-list) (mv ts-best-insertion-move))
  (labels ((iter (tlist)	     
	     (if (null (car tlist)) nil
		 (or		 
		  (and (= (move-node-id mv) (move-node-id (car tlist)))
		       (= (move-vehicle-ID mv) (move-vehicle-ID (car tlist))))
		  (iter (cdr tlist))))))
    (iter (tabu-list-tabu tl))))

(defmethod is-tabup ((ts tabu-search) (mv ts-best-insertion-move))
  (is-tabup (tabu-search-tabu-list ts) mv))

;; Candidate Lists
;; -----------------------------

(defun improving-movep (move)
  "Returns T if move is an improving one, i.e. has a negative fitness."
  (< (move-fitness move) 0))

(defun create-candidate-list (sorted-moves)
  "Given a list of sorted moves, return the list with only improving moves. Will always at least return one move."
  (labels ((iter (moves ans)
	     (if (or (null moves) (not (improving-movep (car moves))))
		 ans
		 (iter (cdr moves)
		       (push (car moves) ans)))))
    (nreverse (iter (cdr sorted-moves)
		    (list (car sorted-moves)))))) ;first move is always chosen

(defun remove-affected-moves (candidate-list move)
  "Given a candidate-list of <Move>s and one <Move> (to be performed), remove all the moves from the candidate-list that do not apply anymore after the selected move is performed."
  (remove-if #'(lambda (mv) (or (= (move-node-id mv) (move-node-id move))
				(= (move-vehicle-ID mv) (move-vehicle-ID move))))
	     candidate-list))
  