;;; General test-case reader
;;; Defining clues, will recognize file format and dispatch to the right reader
;;; -------------------------
(in-package :open-vrp.util)

(defun load-test-case-file (filepath)
  "Given a file, will recognize the format based on some cues and dispatch to appropriate reader function to parse the file. File with .vrp extension will be read as TSPLIB."
  (with-open-file (in filepath)
    (let ((extension (subseq filepath (- (length filepath) 3))))
      (if (string-equal extension "vrp")
	  (load-tsplib-vrp-file filepath)
	  (let ((first-word (read in))
		(second-word (read in)))
	    (declare (ignore first-word))
	    (cond ((eq second-word (intern "VEHICLE"))
		   (load-solomon-vrp-file filepath))
		  (t (error 'file-not-recognized :file filepath))))))))