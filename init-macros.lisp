;;; Sat 31, 2011 (c) Marc Kuo
;;; -------------
;;; Problem building macros
;;; Used for quickly defining problems, requiring only a list of node-coords
;;; Needs adjustment when extended with advanced nodes! (e.g. time-windows, load)
;;; Think of a different approach!
(in-package :open-vrp)

;; Initialising Drawer object functions
;; ---------------------------
(defun get-min-coord (node-coords)
  (reduce #'min (flatten node-coords)))

(defun get-max-coord (node-coords)
  (reduce #'max (flatten node-coords)))
;; ---------------------------
  
;; A simple test version of the define-problem macro. Needs a lot of generalization for creating different models. Parameter list different for each model? Strip off drawer-object?
(defmacro define-problem (problem-type node-coords-list fleet-size plot-filename)
  `(let* ((network (create-network ,node-coords-list)) ;todo choose node type automatically
	  (fleet (create-fleet ,fleet-size network));choose vehicle type automatically
	  (drawer (make-instance 'drawer
				 :min-coord (get-min-coord ,node-coords-list)
				 :max-coord (get-max-coord ,node-coords-list)
				 :filename ,plot-filename)))
     (make-instance ,problem-type :fleet fleet :network network :drawer drawer)))
	  
	   