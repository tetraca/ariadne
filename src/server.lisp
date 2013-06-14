(defpackage :ariadne
  (:use :common-lisp
	:cl-fad
	:cl-ppcre
	:postmodern
	:clack
	:<clack-middleware-session>)
  (:documentation "Ariadne, a Web Content Management System running on Clack."))
(in-package :ariadne)

;(with-open-file (file-stream (pathname-as-file "./settings.lisp") 
;		 :direction :input
;		 :if-does-not-exist (error "Settings file not found."))
; (loop for line = (read-line stream nil)
;     while line
;     collect line)


(defclass <ariadne> (<component>)

)

(defmethod call ((this <ariadne>) env)


)

(clackup
 (builder
  (<ariadne-config>
   :config-path "./settings.lisp")
  (<clack-middleware-static> 
   :path "/static/"
   :root #p"./static/")
  (<clack-middleware-postmodern>
   :
)
  <clack-middleware-session>
  <ariadne-auth>
  <ariadne-page>
  <ariadne-links>
  <ariadne>)
