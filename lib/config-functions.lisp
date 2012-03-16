;; Collection of callable configuration functions in one place
;; -------------------
(in-package :open-vrp.util)

;; Config for plotting
;; -------------------

(define-modify-macro toggle () not)

(defgeneric toggle-plot (problem/algo)
  (:documentation "Toggles plotting on/off of best solution. Config boolean is held by <Drawer> object's plotp slot.")

(defmethod toggle-plot ((pr problem))
  (aif (problem-drawer pr)
       (toggle (drawer-plotp it))
       (error 'missing-drawer-object :prob pr)))
  
(defmethod toggle-plot ((a algo))
  (toggle-plot (algo-best-sol a)))

(defun toggle-animate (algo)
  "Toggles animation, which means plotting every iteration in run-frames/ folder"
  (toggle (algo-animatep algo)))

(defgeneric toggle-legend (problem/algo)
  (:documentation "Toggles legend drawing. When <Algo> is provided, toggles :best-sol"))

(defmethod toggle-legend ((pr problem))
  (aif (problem-drawer pr)
       (toggle (drawer-legendp it))
       (error 'missing-drawer-object :prob pr)))

(defmethod toggle-legend ((a algo))
  (toggle-legend (algo-best-sol a)))

(defun set-plot-file (prob path)
  "Sets the plot output file location."
  (setf (drawer-filename (problem-drawer prob)) path))
;; -------------------

;; Config for logging
;; -------------------
(defun set-log-mode (prob x)
  "Sets log-mode: 0 for no log, 1 for log to log-file, 2 for REPL log."
  (setf (problem-log-mode prob) x))

(defun set-log-file (prob path)
  "Sets the output location of the log-file."
  (setf (problem-log-file prob) path))

;; -------------------

;; Config for problem
;; -------------------

(defun set-dist-array (problem dist-array)
  "Given a <problem> and a 2-dimensional list or array in dist-array, set it in <problem>"
  (setf (problem-dist-array problem) (if (listp dist-array)
					 (2d-list-to-array dist-array)
					 dist-array)))