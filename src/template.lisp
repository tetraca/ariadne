;;;; Ariadne Template System
;;;; Formats content according to a template
;;;; By Tylor Kobierski

(in-package :ariadne)

(defclass <ariadne-template> (<middleware>)
  ((:documentation "The Ariadne Web Server Template System.")))

;;;; Templates are lisp files. They generate HTML using LML
;;;; Ariadne reads the template file then evals it.

;;;; Templates require at minimum a "template-display-content" somwhere
;;;; Or nothing will be able to be displayed.

;;;; Templates

(defmethod call ((this <ariadne-template>) env)
  ;; Read the loaded configuration file for the template name
  (let* ((template-dir   (gethash :site-template (getf env :ariadne.config)))
	 (template-file  (concatenate 'string "./templates/" template-file "main.lisp"))) 
    (setf lml:*html-output* (getf env :html-out))
    (dispense-template this env template-file)))

(defgeneric dispense-template (this env template-file)
  (:documentation "Read a template file."))
(defmethod dispense-template ((this <ariadne-template>) env template-file)
  (let ((full-file nil))
    (with-open-file (template-file (pathname-as-file template-file))
      ;; Open the file.
      ;; Place complete, singular S-expressions into their own sections
      ;; Evaluate expressions when complete
      (do* ((raw-line (read-line template-file nil) (read-line template-file nil))
	    (complete-line raw-line))
	   ((eql raw-line nil) nil)

	;; Read one line ahead at the start
	(when (string= raw-line complete-line)
	  (setf raw-line (read-line template-file nil)))

	;; When we encounter a line that has some form of content
	;; Concatenate raw lines until we have an equal amount of
	;;     opening and closing brackets.

	;; That should suggest a full S-Expression.
	;; Later on it might be wise to also test for usages of string coded
	;;    parentheses, but there generally shouldn't be a case where a designer decides
	;;    that they need to hardcode unpaired parentheses if they know how to use HTML.
	;;    (JS and JQuery expressions both complete their parentheses so they shouldn't
	;;     be problematic)

	(when (> (length complete-line) 0)
	  (if (eql (count #\( complete-line)
		   (count #\) complete-line))
	      (progn
		(push complete-line full-file)
		(setf complete-line raw-line))
	      (setf complete-line
		    (concatenate 'string complete-line " " raw-line))))))

    (setf full-file (nreverse full-file))
    ;; Evaluate each collected, complete s-expression
    (dolist (current-expression full-file)
      (format t "Now evaluating ~a.~%" current-expression)
      (eval (read-from-string current-expression)))))



;;; Template building functions
(defun template-display-content ()
  "Calls the next middleware sections for a template"
  (let ((response (call-next this env)))
    (template-postprocess this env))) ;; Postprocessing


(defun template-postprocess ()
  "Postprocessing functions for a template should override this function.")

(defun template-build-menu (id)
  "Build a menu from its id"
)

(defun template-build-footer ()
  "Build a page footer"
)
