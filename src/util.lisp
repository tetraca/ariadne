;;;; Ariadne Web Server
;;;; Utility File - Functions useful for every middleware portion
;;;; By Tylor Kobierski

(in-package :ariadne)

(defun splitted-info (env)
  "Split the page URL arguments into separate sections"
  (split-sequence #\/ (getf env :page-info)))

(defgeneric api-call (this env)
  (:documentation "API call handler for a middleware application. The API handler mainly just outputs things handled by the main handler from text."))
(defgeneric main-call (this env)
  (:documentation "Main call handler for a middleware application. Main calls will accumulate out"))
(defgeneric admin-call (this env)
  (:documentation "Administration function handler"))
