;;;; Ariadne Web Server
;;;; Utility File - Functions useful for every middleware portion
;;;; By Tylor Kobierski

(in-package :ariadne)

(defun splitted-info (env)
  "Split the page URL arguments into separate sections."
  (split-sequence #\/ (getf env :page-info)))

(defun get-post-info (env)
  "Returns the submitted POST plist, if it exists."
  (clack.request:body-parameter (clack.request:make-request env)))

(defmacro with-request-method (method &rest body)
  "Convenience macro for separating POST and GET operations."
  `(when (eql (getf env :request-method) ,method)
    ,@body))
