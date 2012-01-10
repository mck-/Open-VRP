;;; Wed Jun 22, 2011 (c) Marc Kuo
;;; Plot the solution given in *solution* using vecto
;;; -------
;;; Use (plot-solution <problem>)
;;; (plot-nodes <problem>) for just the nodes.

(in-package :open-vrp.output)
       
;; Helper functions 
;; -----------------------------
(defun coord->pix (drawer x)
  "Given a drawer and a coord, calculate the associated x/y pixel. Includes 1 coord as border."
  (let ((min (1- (drawer-min-coord drawer)))
	(max (1+ (drawer-max-coord drawer)))
	(max-pix (drawer-max-pix drawer)))
    (* (- x min) (/ max-pix (- max min)))))

(defun get-color ()
  "Returns a random color."
  (let ((r (random 0.99))
	(g (random 0.99))
	(b (random 0.99)))
    (values r g b)))

;; anaphoric macro input index of the node, and binds coords, pix-x and pix-y to the node's.
(defmacro use-node (drawer node &body body)
  `(let ((pix-x (coord->pix ,drawer (node-xcor ,node)))
	 (pix-y (coord->pix ,drawer (node-ycor ,node))))
     ,@body))

;; ------------------------------

;; Arrow drawing
;; ------------------------------

;; Tue Dec 6, 2011 - fixing for CLOS (using defun instead of defmethod for convenience?)
(defun store-pix (drawer pix-x pix-y)
  "Store the position of the path (helper function for arrow calculation)."
  (setf (drawer-x-pos drawer) pix-x
	(drawer-y-pos drawer) pix-y))

(defun arrow-to (drawer pix-x pix-y size skew)
  "Draws an arrow as in line-to, but creates an arrow in the middle of size. Use with canvas!"
  (let* ((v (- pix-x (drawer-x-pos drawer)))
	 (w (- pix-y (drawer-y-pos drawer)))
	 (cen-x (+ (drawer-x-pos drawer) (/ v 2)))
	 (cen-y (+ (drawer-y-pos drawer)  (/ w 2)))
	 (mid-x (+ (drawer-x-pos drawer) (* skew v)))
	 (mid-y (+ (drawer-y-pos drawer) (* skew w)))
	 (A-x (- mid-x (* size w)))
	 (A-y (+ mid-y (* size v)))
	 (B-x (+ mid-x (* size w)))
	 (B-y (- mid-y (* size v))))
    (line-to mid-x mid-y)
    (line-to A-x A-y)
    (line-to cen-x cen-y)
    (line-to B-x B-y)
    (line-to mid-x mid-y)
    (line-to pix-x pix-y)))

;; ----------------------------------

;; Node drawing
;; ---------------------------------
(defgeneric draw-nodes (drawer network)
  (:method (drawer network))
  (:documentation "Given the drawer and network objects, plot the nodes only. Usage only with-canvas!"))

(defmethod draw-nodes ((dr drawer) (net network))
  (dolist (node (network-nodes net))
    (use-node dr node
      (set-rgb-fill 1.0 0.65 0.3)
      (centered-circle-path pix-x pix-y 10)
      (fill-path)
      (set-rgb-fill 0 0 0)
      (draw-centered-string pix-x (- pix-y 5) (write-to-string (node-id node))))))
;; ----------------------------

;; Legend drawing
;; -----------------------------
(defun draw-legend-item (drawer veh-obj r g b)
  (let ((leg-x (drawer-legend-x drawer))
	(leg-y (drawer-legend-y drawer)))
    (set-rgb-fill r g b)
    (centered-circle-path leg-x leg-y 12)
    (fill-path)
    (set-rgb-fill 0 0 0)
    (draw-centered-string leg-x (- leg-y 6) (write-to-string (vehicle-id veh-obj)))
    (draw-string (+ leg-x 30) (- leg-y 6) (write-to-string (route-indices veh-obj)))
    (setf (drawer-legend-y drawer) (- leg-y 30))))

;-----------------------------

;; Solution drawing
;; -----------------------------

(defgeneric plot-solution (problem/algo &optional output-file)
;  (:method (problem &optional output-file) "Expects <problem> object as input!")
  (:documentation "Given a solution object (<problem>/<algo>), draw the solution in output file given in the drawer object's :filename slot (which is held in problem-drawer slot). When <algo> object as input, draw the best found solution by that algo object."))
 
(defmethod plot-solution ((sol problem) &optional output-file)
  (let ((dr (problem-drawer sol)))
    (with-canvas (:width (drawer-max-pix dr) :height (drawer-max-pix dr))
      (let ((font (get-font "FreeSerif.ttf"))
	    (temp-y (drawer-legend-y dr))) ;save the original value (draw-legend-item sets it)
					;settings
	(set-font font 15)
	(set-rgb-fill 1.0 1.0 1.0)
	(clear-canvas)
	(set-line-width 3)
				        ;iterate over fleet - draw routes & legend together
	(dolist (veh (fleet-vehicles (problem-fleet sol))) ;Fri Dec 2, 2011 - changed for CLOS
	  (unless (null (cdr (vehicle-route veh)))
	    (multiple-value-bind (r g b) (get-color)
	      (when (drawer-legend dr)
		  (draw-legend-item dr veh r g b)) ; draw legend item
	      (set-rgb-stroke r g b))
					
	    (use-node dr (first (vehicle-route veh)) ; draw path
		      (centered-circle-path pix-x pix-y 12) ;circle the departing node
		      (stroke)
		      (move-to pix-x pix-y)	  
		      (store-pix dr pix-x pix-y)
		      (dolist (node (rest (vehicle-route veh)))
			(use-node dr node
				  (arrow-to dr pix-x pix-y 0.038 0.45) ;draw arrows
				  (store-pix dr pix-x pix-y))))
	    (stroke)))
	(setf (drawer-legend-y dr) temp-y) ;reset to original value
					;drawing nodes
	(draw-nodes dr (problem-network sol))
					;draw results (temporary disabled, needs fix!)
	(draw-string (* 0.1 (drawer-max-pix dr)) (* 0.1 (drawer-max-pix dr)) (write-to-string (fitness sol)))
					; save file
	(if output-file
	    (save-png output-file)
	    (save-png (drawer-filename dr)))))))

(defmethod plot-solution ((a algo) &optional output-file)
  (plot-solution (algo-best-sol a) output-file))

(defgeneric plot-nodes (problem)
  (:method (problem) "Expects <drawer> and <network> as input!")
  (:documentation "Draws only the nodes in output file."))

(defmethod plot-nodes ((prob problem))
  (let ((dr (problem-drawer prob))
	(net (problem-network prob)))
    (with-canvas (:width (drawer-max-pix dr) :height (drawer-max-pix dr))
      (let ((font (get-font "FreeSerif.ttf")))
					;settings
	(set-font font 15)
	(set-rgb-fill 1.0 1.0 1.0)
	(clear-canvas)
      (set-line-width 3)
      (draw-nodes dr net)
      (save-png (drawer-filename dr))))))

;; ----------------------------------------------

(defgeneric toggle-legend (obj)
  (:method (obj) "Expects <drawer>/<problem>!")
  (:documentation "toggle the legend on/off."))
	   

(defmethod toggle-legend ((dr drawer))
  (if (drawer-legend dr)
      (setf (drawer-legend dr) nil)
      (setf (drawer-legend dr) T)))

(defmethod toggle-legend ((pr problem))
  (toggle-legend (problem-drawer pr)))