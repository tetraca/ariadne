;;;; Ariadne Web Server
;;;; Abstract Module Template
;;;; By Tylor Kobierski

;;;; Provides a method of building website modules
;;;; Which interact with the user through API and Web
;;;; In a standardized manner

(in-package :ariadne)

(defclass <ariadne-module> (<middleware>)
  ((identifier
    :initform (error "Invalid identifier.")
    :initarg :identifier
    :reader identifier
    :documentation "The unique URL identifier for this module"))
  (:documentation "A template for a generic piece of ariadne middleware. Ariadne modules require a unique identifier to which they respond. "))

(defmethod call ((this <ariadne-module>) env)
  "Generic ariadne module call method"
  (let ((path-info (splitted-info env)))
    ;; Respond to requests for this specific module.
    (when (string= (second path-info) (identifier this))
      ;; Handle either an API request or a standard request.
      (cond ((string= (first path-info) "api")
	     (api-call this env))

	    ((string= (first path-info) (identifier this))
	     (main-call this))

	    ((string= (first path-info) "admin")
	     (admin-call this)))

      (let ((response (call-next this env)))
	;; Postprocessing
	))


(defgeneric api-call (this env)
  (:documentation "API call handler for a middleware application. The API handler mainly just outputs things handled by the main handler from text."))
(defmethod  api-call ((this <middleware>) env)
  "The generic API call handler. Key is a value under which the main calls are stored."
  ;; Let main call fill the environment appropriately
  ;; And display the result as text. Normally this should not need
  ;; to be overwritten.
  (main-call this)
  `(200
    (:content-type "text/plain")
    (,(getf env key))))


(defgeneric main-call (this env)
  (:documentation "Main call handler for a middleware application. Main calls will accumulate out"))

(defgeneric admin-call (this env)
  (:documentation "Administration function handler."))
