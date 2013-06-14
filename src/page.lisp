;;;; Ariadne Web Server
;;;; Page Manager
;;;; By Tylor Kobierski
(in-package :ariadne)

(defclass <ariadne-pages> (<middleware>)

)

(defmethod call ((this <ariadne-pages>) env)
  ;; Split the page-info
  (let* ((page-info (getf env :page-info))
	 (page-split (split-sequence #\/ page-info)))

    ;; Determine whether this is an API call
    ;; Or an indirect call
    (cond ((string= (first page-split) "api")
	   (api-call this env))
	  ((string= (first page-split) "page")
	   (main-call this env)))

    (let ((response (call-next env)))
      ;; No postprocessing necessary
      )))

(defgeneric authorize (this env)
  (:documentation "Checks if the operation is permitted based on user group."))
(defmethod  authorize ((this <ariadne-pages>) env)
  "Auth check for a page. Returns nil if user is not authorized"
  (let ((user-level (user-auth env))
	(page-level (query (:select 'auth-group :from 'page
				    :where (:= 'page-id
					       (second (splitted-info env)))))))
    (when (< user-level page-level)
      nil)
    (when (> user-level page-level)
      user-level)))

(defmethod  api-call ((this <ariadne-pages>) env)
  "API calls for the page manager."
  ;; Let main call fill the environment appropriately
  ;; And display the result as text.
  (main-call this)
  `(200
    (:content-type "text/plain")
    (,(getf env :ariadne.pages))))


(defmethod  main-call ((this <ariadne-pages>) env)
  "Standard calls for the page manager."
  (let ((page-split (splitted-info env)))
    (when (eql (length page-split) 2)
      (cond ((eql (second page-split) "new")
	     (new-page this env))

	    ((eql (second page-split) "admin")
	     (administration this env))

	    (t ;; Assume it's a page-id
	     (setf (getf env :ariadne.pages) (get-page ))
	     )))


    (when (> (length page-split) 2)
      (cond ((eql (third page-split) "edit")
	     
	     )
	    ((eql (third page-split) "delete")
	     
	     )

	    (t

	     ))
)
