;;;; Ariadne Web Server
;;;; By Tylor Kobierski

;;;; Authorization Module
;;;; Manages the current user's session

;;;; TODO:
;;;;   - Registration API
;;;;   - User verification API
;;;;   - Store auth group in environment
;;;;   - Use PBKDF2 to store and verify
;;;;   - Verify session id in logout-user is proper

;;;; Ariadne Auth requires the following modules to be wrapped:
;;;;   clack-middleware-session
;;;;   clack-middleware-postmodern
;;;;   (ariadne-international)
;;;;   (ariadne-templates)

(in-package :ariadne)

(defclass <ariadne-auth> (<middleware>) 
  ((:documentation "The Ariadne Authorization Middleware for Clack.")))

(defmethod call ((this <ariadne-auth>) env)
  ;; Write a logflag cookie for a guest.
  (unless (extract-hash this env "logflag")
    (setf (gethash :logflag (getf env :clack.session)) 0))

  ;; When a user is set as logged in, 
  ;; find the user associated with his session id.
  ;; Otherwise, set :auth.user to nil to tell paektu
  ;; That we have a guest.
  (if (eql (extract-hash this env "logflag") 1)
      (setf (gethash :auth.user (getf env :clack.session)) 
	    (find-associated-user this env))
      (setf (gethash :auth.user (getf env :clack.session))
	    nil))
  (let ((result nil))

    ;; Authorization API Requests
    (cond ((string= (getf env :path-info)
		    "/api/auth/login")
	   (setf result (login-user this env)))
	  ((string= (getf env :path-info)
		    "/api/auth/logout")
	   (setf result (logout-user this env)))
	  ((string= (getf env :path-info)
		    "/api/auth/register")
	   (setf result (register-user this env)))
	  ((string= (getf env :path-info)
		    "/api/auth/verify")
	   (setf result (verify-user this env))))

    ;; Return API request result
    `(200
      (:content-type "text/plain")
      (,result))))

(defgeneric extract-hash (this env key)
  (:documentation "Extracts a hash from the environment table."))
(defmethod  extract-hash ((this ariadne-auth>) env key)
  "Extracts a hash from the environment table."
  (gethash key (getf env :clack.session)))

(defgeneric find-associated-user (this env)
  (:documentation "Finds a user associated with the session id in the cookie."))
(defmethod  find-associated-user ((this <ariadne-auth>) env)
  "Finds the user associated with a session id and returns it."
  (caar (query (:select 'name 
			:from 'users 
			:where (:= 'session_id (extract-hash this env "id"))))))

(defgeneric find-associated-password (this name)
  (:documentation "Finds the password hash associated with the username and returns it."))
(defmethod  find-associated-password ((this <ariadne-auth>) name)
  "Finds the password hash associated with the username and returns it."
  (caar (query (:select 'password
			:from 'users
			:where (:= 'name name)))))

(defgeneric user-exists-p (this name)
  (:documentation "Probes to see if the username exists, returning nil on failure."))
(defmethod  user-exists-p ((this <ariadne-auth>) name)
  "Probes to see if a username exists. This returns nil if the user doesn't exist."
  (query (:select 'user_id :from 'users :where (:= 'name name))))


(defgeneric login-user (this env)
  (:documentation "Attempts to log a user in."))
(defmethod  login-user ((this <ariadne-auth>) env)
  "Attempts to log a user in, returning an error message if something goes wrong."
  (let ((postinfo (clack.request:body-parameter (clack.request:make-request env))))
    ;; In a properly constructed query, the second element in
    ;; The login attempt will be the username entered.
    (if (user-exists-p this (getf postinfo :username))
	(progn
	  ;; Check to see if there is a user already set with this session id
	  ;; Fail if it does.
	  (if (string= (gethash :id (getf env :clack.session))
		       (find-associated-user this env))
	      "Already logged in from a different session."
	      (progn
		;; Check to see if the password submitted is the same as
		;; The password which has been stored.
		(if (string= (getf postinfo :password)
			     (find-associated-password this (getf postinfo :password)))
		    (progn
		      ;; Store a sesion id in both a cookie and
		      ;; on the database
		      (setf (gethash :id (getf env :clack.session))
			    (generate-session-id this (getf postinfo :username)))
		      (execute (:update 'users
					:set 'session_id (gethash :id (getf env :clack.session))
					:where (:= 'name (getf postinfo :username))))
		      "Login success!")
		    "Password is invalid.")))
	  "Username does not exist."))))

(defgeneric logout-user (this env)
  (:documentation "Logs a user out of his session."))
(defmethod  logout-user ((this <ariadne-auth>) env)
  "Log a user out of his session"
  ;; Make sure that a user is actually logged in first.
  (if (gethash :logflag (getf env :clack.session) 0)
      "You are not logged in. There's no need for you to log off!"
      (progn
	;; Only remove a valid session-id.

	;; Remove associated session-id from database.
	(execute (:update 'users
			  :set 'session_id nil
			  :where (:= 'session_id
				     (gethash :id (getf env :clack.session)))))

	;; Delete cookie information
	(setf (gethash :id (getf env :clack.session)) nil)
	(setf (gethash :logflag (getf env :clack.session)) 0)
	"Successfully logged off!")))

(defgeneric register-user (this env)
  (:documentation "Adds a user to the system."))
(defmethod  register-user ((this <ariadne-auth>) env)
  "Add a user into the system. Relies on POST data supplied by the user."
  (let ((postinfo (clack.request:body-parameter (clack.request:make-request env))))
    ;;; Ensure username is unique
    (if (user-exists-p this (getf postinfo :username))
	(progn
          ;;; Ensure email is of a proper format
	  (if (cl-ppcre:scan "^\S+@\S+\.\S+$" (getf postinfo :email))
	      (progn
		;;; Send an email
		()
		)
	      "Mailformed email address.")
	"Username has been taken."))))


(defgeneric generate-session-id (this name)
  (:documentation "Generates a session id."))
(defmethod  generate-session-id ((this <ariadne-auth>) name)
  "Generates a session id for a user"
  ;; Firstly convert everything into octets because that's what
  ;; Ironclad requires, then append all the octets
  (let* ((passphrase   (sb-ext:string-to-octets "iron and copper"))
	 (login-time   (sb-ext:string-to-octets (get-universal-time)))
	 (refined-name (sb-ext:string-to-octets name))
	 (welded-octet (append passphrase login-time refined-name)))
    (sb-ext:octets-to-string
     (ironclad:digest-sequence (ironclad:make-digest :sha1) welded-octet))))

(defgeneric hash-user-password (this password)
  (:documentation "Stores the user's password in a secure format."))
(defmethod  hash-user-password ((this <ariadne-auth>) password)
  "Hashes the user's password."
  (let ((octet-password (sb-ext:string-to-octets password)))
    (ironclad:pbkdf2-hash-password-to-combined-string  octet-password)))

(defgeneric hash-user-password-po (this password octet)
  (:documentation "Checks to see if password matches database-stored octet string."))
(defmethod  hash-user-password-p ((this <ariadne-auth>) password octet)
  "Checks to see if password matches database-stored octet string"
  (let ((octet-password (sb-ext:string-to-octets password)))
    (ironclad:pbkdf2-check-password octet-password octet)))


(defun user-auth (env)
  "Return the auth level of a currently logged in user"
  ;; First, check if user is logged in
  (if (eql (gethash :logflag (getf env :clack.session)) 0)
      ;; Return 0 for a guest
      0
      ;; Otherwise query the database for the appropriate auth group of 
      ;; a user
      (query (:select 'auth-group :from 'user 
			:where (:= 'session-id (gethash :id (getf env :clack.session)))))))
