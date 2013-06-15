;;;; Ariadne Web Server
;;;; Page Manager
;;;; By Tylor Kobierski
(in-package :ariadne)

(defclass <ariadne-pages> (<ariadne-module>)
  (:documentation "Manages static content pages."))

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

(defmethod  main-call ((this <ariadne-pages>) env)
  "Standard calls for the page manager."
  (let ((page-split (splitted-info env)))
    (when (eql (length page-split) 2)
      (cond ((eql (second page-split) "new")
	     (new-page this env))

	    (t ;; Assume it's a page-id and retrieve it.
	     (setf (getf env :ariadne.pages) (get-page this second page-split)))))


    (when (> (length page-split) 2)
      (cond ((eql (third page-split) "edit")
	     ;; Retrieve page-id if it is an argument
	     (if (eql (length page-split) 4)
		 (progn 
		   ;; Retrieve page-id if exists
		   (let ((post-info (get-post-info env)))

		     (setf (getf env :ariadne.pages) "Page-id successfully changed.")))
		 (setf (getf env :ariadne.pages) "No page-id specified.")))
	    ((eql (third page-split) "delete")
	     (if (eql (length page-split) 4)
		 (progn
		   ;; Retrieve page-id

		   )
		 (setf (getf env :ariadne.pages) "No page-id specified.")))

	    (t

	     ))
)

(defmethod admin-call ((this <ariadne-pages>) env)
  "Ariadne pages administration"
)

(defgeneric get-page (this page-id)
  (:documentation "Retrieves page content"))
(defmethod  get-page ((this <ariadne-pages>) page-id)
  (query (:select :from 'pages :where (:= 'page-id page-id))))
