;; Mon Jan 9, 2011
;; Tabu Search utilities
;; ---------------------

(in-package open-vrp.algo)

;; Tabu List
;; -----------------------------

;; Add
(defgeneric add-to-tabu (algo move)
  (:method (algo move) "add-to-tabu: method for <Algo> or <Move> is not defined!")
  (:documentation "Put the <Move> on the :tabu-list of <Algo>, which is a <Tabu-list> object."))

(defmethod add-to-tabu ((ts tabu-search) (mv ts-best-insertion-move))
  (let* ((tl (tabu-search-tabu-list ts))
	 (tenure (tabu-list-tenure tl)))
    (push mv (tabu-list-tabu tl))
    (when (> (length (tabu-list-tabu tl)) tenure)
      (setf (tabu-list-tabu tl) (subseq (tabu-list-tabu tl) 0 tenure)))))

;; Check
(defgeneric is-tabup (algo move)
  (:method (algo move) "is-tabup: method for <Algo> or <Move> is not defined!")
  (:documentation "Returns <Move> if on the :tabu-list, NIL otherwise"))

(defmethod is-tabup ((ts tabu-search) (mv ts-best-insertion-move))
  (let ((tlist (tabu-list-tabu (tabu-search-tabu-list ts))))
    (and (member (move-node-id mv) tlist :key #'move-node-id)
	 (member (move-vehicle-ID mv) tlist :key #'move-vehicle-ID))))