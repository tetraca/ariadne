(in-package :ariadne)

(defclass <ariadne-template> (<middleware>)
  ((:documentation "The Ariadne Web Server Template System.")))

;;;; Note from drunk self
;;;; Take advantage of clack middleware wrapping.
;;;; Load the template on non-api domains.
;;;; Then have the subsequent modules print on the content section.

;;;; Side modules need to provide hook functions for templates to grab
;;;; Useful information.

;;;; Templates are lisp files. They generate HTML using LML
;;;; Ariadne reads the template file then evals it.

(defmethod call ((this <ariadne-template>) env)

  ;; Read all the lines in the file
  ;; Then let lisp evaluate the template file
  (let ((line nil))
    (with-open-file (template-file (getf :env))
      (loop for line = (read-line template-file nil)
	 while line
	 collect line))
    (eval line)))

(defun template-display-content ()
  "Calls the next middleware sections for a template"
  (let ((response (call-next this env)))
    ;; Postprocessing
    ))

(defun template-build-menu (id)
  "Build a menu from its id"
)

(defun template-build-footer ()
  "Build a page footer"
)
