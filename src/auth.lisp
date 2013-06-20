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
;;;;   ariadne-config
;;;;   (ariadne-international)
;;;;   (ariadne-templates)

(in-package :ariadne)

(defclass <ariadne-auth> (<ariadne-module>)
  ((user
    :initform "Guest"
    :accessor get-user-name
    :documentation "The user currently logged in.")
   (auth-level
    :initform 0
    :accessor get-auth-level
    :documentation))
  (:documentation "The Ariadne Authorization Middleware for Clack."))
(defmethod initialize-instance :after ((this <ariadne-auth>) &allow-other-keys)
  (setf (slot-value this 'identifier) "ariadne.auth"))

(defmethod main-call ((this <ariadne-auth>) env)
  "Main call handler for user authorization."
  ;; Write a logflag cookie for a guest.
  (unless (extract-hash this env "logflag")
    (setf (gethash :logflag (getf env :clack.session)) 0))
  ;; If we are logged in set the slot values appropriately

  ;; When a user is set as logged in, 
  ;; find the user associated with his session id.
  (when   (eql (extract-hash this env "logflag") 1)
    (setf (get-user-name this) (find-associated-user this env))
    (setf (get-auth-level this) (user-auth env)))

  ;; Place a cons in auth which has pertinent user information
  ;; for other modules to read and understand
  (setf (getf env :ariadne.auth) (cons (get-user-name this) (get-auth-level this)))

  ;; User-accessible interfaces
  (let ((split-info (splitted-info env))
	(result     "Request not processed."))
    (cond ((string= (second split-info) "login")
	   (setf result (login-user this env)))
	  ((string= (second split-info) "logout")
	   (setf result (logout-user this env)))
	  ((string= (second split-info) "register")
	   (setf result (register-user this env)))
	  ((string= (second split-info) "verify")
	   (setf result (verify-user this env)))
	  (t
	   (setf result "Invalid request.")))

    ;; Place any messages we get onto the display buffer
    (setf (getf env :ariadne.display) result)))

(defmethod admin-call ((this <ariadne-auth>) env)
  "Administration functions"
  ;; Work in progress
)



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
			:from 'user 
			:where (:= 'session-id (extract-hash this env "id"))))))

(defgeneric find-associated-password (this name)
  (:documentation "Finds the password hash associated with the username and returns it."))
(defmethod  find-associated-password ((this <ariadne-auth>) name)
  "Finds the password hash associated with the username and returns it."
  (caar (query (:select 'password
			:from 'user
			:where (:= 'name name)))))

(defgeneric user-exists-p (this name)
  (:documentation "Probes to see if the username exists, returning nil on failure."))
(defmethod  user-exists-p ((this <ariadne-auth>) name)
  "Probes to see if a username exists. This returns nil if the user doesn't exist."
  (query (:select 'email :from 'users :where (:= 'name name))))

(defgeneric  user-verified-p (this name)
  (:documentation "Check to see if the user is verified. Returns nil if not."))
(defmethod   user-verified-p ((this <ariadne-auth>) name)
  (if (string= "true"
	       (caar (query (:select 'verified :from 'user :where (:= 'name name)))))
      t
      nil))
(defgeneric  login-attempts-exceeded-p (this name)
  (:documentation "Check to see if the number of login attempts was exceeded. Returns nil if not."))
(defmethod   login-attempts-exceeded-p ((this <ariadne-auth>) name)
  (if (eql 3
	   (caar (query (:select 'login-attempts :from 'user :where (:= 'name name)))))
      t
      nil))

(defgeneric increment-attempts (this name)
  (:documentation "Record the number of times an unscrupulous person has attempted to login to this account."))
(defmethod  increment-attempts ((this <ariadne-auth>) name)
  (let ((current-attempts (caar (query (:select 'login-attempts :from 'user :where (:= 'name name))))))
    (query (:update 'user
		    :set login-attempts (+ current-attempts 1)
		    :where (:= 'name name)))))

(defgeneric write-verification-email (this env name &key failure)
  (:documentation "Writes and sends a verfication email, recording it to the sesion id of the database. A verification email written for the case of failure just changes the message for the appropriate circumstance."))
(defmethod  write-verification-email ((this <ariadne-auth>) env name &key (failure nil))
  ;; TODO: Make the sent email perhaps follow an HTML template dictated by the theme.
  (let ((temp-key (generate-session-id this "register"))
	(email (caar (query (:select 'email :from 'user :where (:= 'name name))))))
    ;; In either case, write it as the session id in the database.
    ;; The user is unable to login and set their own session-id until
    ;; They have been verified anyway.
    (execute (:update 'user
		      :set 'session-id temp-key
		      :where (:= 'name name)))

    (unless failure
      (cl-smtp:send-email "localhost"
			  (gethash :site-email (getf env :ariadne.config))
			  email
			  (concatenate 'string "Your user registration for " (gethash :site-name (getf env :ariadne.config)))
			  (concatenate 'string "Please visit the following link to complete your registration: http://" (getf env :server-name) "/auth/verify/" (write-to-string temp-key)))
      (concatenate 'string "An email has been sent to the address " email ". Please go to the URL specified to complete your registration."))

    (when failure
      (cl-smtp:send-email "localhost"
			  (gethash :site-email (getf env :ariadne.config))
			  (concatenate 'string "Security breach of your account at " (gethash :site-name (getf env :ariadne.config)))
			  (concatenate 'string "Please copy and paste the following link to reactivate your account: http://" (getf env :server-name) "/auth/verify/" (write-to-string temp-key)))
      (concatenate 'string "An email has been sent to the address " email "Please go to the URL speicified therein to complete your registration"))))



(defgeneric login-user (this env)
  (:documentation "Attempts to log a user in."))
(defmethod  login-user ((this <ariadne-auth>) env)
  "Attempts to log a user in, returning an error message if something goes wrong."
  (with-request-method :GET
    ;; TODO: Login form
    )

  (with-request-method :POST
    (let ((post-info (get-post-info env)))
      ;; Before logging in, we need to make sure these conditions didn't occur:
      ;; - The username exists.
      ;; - The user's email has been verified.
      ;; - The user has not logged in from a different session
      ;; - The password matches the entry in the database.
      ;; - The user has not failed to login 3 times before.
      ;; Add a demerit in the database each time a user has failed to log in.

      (cond ((null (user-exists-p this (getf post-info :username)))
	     "Invalid username or password.")

	    ((null (user-verified-p this (getf post-info :username)))
	     (increment-attempts this (getf post-info :username))
	     "You have not been verified. Please check your email for a verification link.")
	    ((string= (extract-hash this env "id")
		      (find-associated-user this env))
	     (increment-attempts this (getf post-info :username))
	     "This account is logged in from a different session.")

	    ((null (hash-user-password-p (getf post-info :password)
					 (find-associated-password this (getf post-info :username))))
	     (increment-attempts this (getf post-info :username))
	     "Invalid username or password.")

	    ((login-attempts-exceeded-p this (getf post-info :username))
	     ;; Set the account to unverified.
	     (execute (:update 'user
			       :set 'verified "false"
			       :where (:= 'name (getf post-info :username))))
	     ;; Send an email and notify the user.
	     (write-verification-email this (getf post-info :username))

	     "You have attempted to log in too many times. Your account has been locked. An email has been sent to reverify your account.")
	    (t
	     ;; When the above conditions haven't occured
	     ;; We can log the user in.
	     ;; Insert the session id in a cookie and database entry
	     ;; Update the last time of login.
	     ;; Clear failed login attempts.

	     (setf (gethash :id (getf env :clack.session))
		   (generate-session-id this (getf post-info :username)))
	     (execute (:update 'user
			       :set 'session_id (gethash :id (getf env :clack.session))
			       :where (:= 'name (getf post-info :username))))

	     (execute (:update 'user
			       :set 'last-login (simple-date:universal-time-to-timestamp (get-universal-time))
			       :where (:= 'name (getf post-info :username))))

	     (execute (:update 'user
			       :set 'login-attempts 0
			       :where (:= 'name (getf post-info :username))))
	     (concatenate 'string "Login successful. Welcome " (getf post-info :username) "!"))))))


(defgeneric logout-user (this env)
  (:documentation "Logs a user out of his session."))
(defmethod  logout-user ((this <ariadne-auth>) env)
  "Log a user out of his session"
  (cond ((eql (extract-hash this env "logflag") 0)
	 "You are not logged in, there is no need for you to log off!")
	(t
	 (handler-case
	     (execute (:update 'user
			       :set 'session-d nil
			       :where (:= 'session-id
					  (extract-hash this env "id"))))
	   (simple-error () 
	     (return-from logout-user "Could not log you out. Invalid session id encountered. Are you sure you logged in?")))
	 ;; Delete cookie information
	 (setf (gethash :id (getf env :clack.session)) nil)
	 (setf (gethash :logflag (getf env :clack.session)) 0)
	 "Successfully logged off!")))

(defgeneric register-user (this env)
  (:documentation "Adds a user to the system."))
(defmethod  register-user ((this <ariadne-auth>) env)
  "Add a user into the system."

  (with-request-method :GET
    ;; TODO: Place the user registration form
    ;; A TOS might be a nice feature to add.

    )

  (with-request-method :POST
    (let ((post-info (get-post-info env)))
      ;; Make sure every parameter satisfies the conditions
      ;; Required
      (cond ((eql (length (getf post-info :username)) 0)
	     "You must enter a username.")
	    ((null (user-exists-p this (getf post-info :username)))
	     "This username has already been taken.")
	    ((> (length (getf post-info :username)) 20)
	     "Your username is too long.")

	    ((eql (length (getf post-info :password)) 0)
	     "You must enter a password.")
	    ((null (string= (getf post-info :password)
			    (getf post-info :password-confirm)))
	     "Your passwords do not match.")

	    ((eql (length (getf post-info :email)) 0)
	     "You must enter an email.")
	    ((null (cl-ppcre:scan "^\S+@\S+\.\S+$" (getf post-info :email)))
	     "Your email may be malformed.")

	    (t
	     ;; Create an entry into the user database
	     (execute (:insert-into 'user
				    :set 'name     (getf post-info :username)
				         'password (getf post-info :password)
					 'email    (getf post-info :email)))
	     (send-verification-email this env (getf post-info :username)))))))


(defgeneric verify-user (this env)
  (:documentation "Verifies the user's email address."))
(defmethod  verify-user ((this <ariadne-auth>) env)

)


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

(defgeneric hash-user-password-p (this password octet)
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
