(defpackage :ariadne.installer
  (:use :common-lisp)
  (:documentation "The installer for the ariadne web server.")
  (:export run-installer
	   run-uninstaller))

(in-package :ariadne.installer)

(defvar *ariadne-version* "0.0.1")

(defun run-installer (&key (site-name "Ariadne Default Website")
			   (site-email "admin@localhost")
			    site-address ""
			   (site-port 5000)
			   database-name 
			   database-user
			   database-password
			   (database-host "localhost")
			   (database-port 5432))
  "Installs the ariadne web server to the current directory."
  (format t "Ariadne v ~a Installer.~%" *ariadne-version*)
  (format t "NOTE: Settings will be stored in ./settings.lisp.~%~%")

  (format t "Quickloading dependencies...~%")
  (ql:quickload :alexandria)
  (ql:quickload :bordeaux-threads)
  (ql:quickload :clack)
  (ql:quickload :ironclad)
  (ql:quickload :postmodern)
  (ql:quickload :cl-fad)
  (ql:quickload :clack-middleware-postmodern)
  (ql:quickload :cl-smtp)
  (ql:quickload :lml)
  (format t "Done.~%")

  (format t "Formatting PostgreSQL database...~%")
  (postmodern:with-connection (list database-name 
				    database-user
				    database-password
				    database-host)

    (format t "-- Creating user_group table...~%")
    (postmodern:execute (:create-table user-group
			    ((auth-level
			      :type integer
			      :unique t
			      :primary-key t)
			     (group-name
			      :type (varchar 255)
			      :unique t)
			     (group-description
			      :type text))
			    (:constraint auth-level-check
					 :check (:> 'auth-level 0))))
    (format t "--- Done.~%")


    (format t "-- Creating user table...")
    (postmodern:execute (:create-table user 
			    ((name       :type    (varchar 20)
					 :unique  t
					 :primary-key t) 
			     (password   :type    (string 32)) 
			     (email      :type    (varchar 255)
					 :unique  t)
			     (session-id :type    (string 32))
			     (verified   :type    boolean
					 :default "false")
			     (auth-group :type    integer
					 :default 10)
			     (last-login :type    timestamp)
			     (login-attempts :type integer
					     :default 0))
			    (:foreign-key (auth-group) (user-group auth-level))))
    (format t "--- Done.~%")


    (format t "-- Creating page table...~%")
    (postmodern:execute (:create-table page
			    ((page-id    :type        (varchar 20)
					 :unique      t
					 :primary-key t)
			     (auth-group  :type integer)
			     (created     :type timestamp)
			     (modified    :type timestamp)
			     (modified-by :type (varchar 20))
			     (title       :type (varchar 255))
			     (content     :type text))
			    (:foreign-key (auth-group) (user-group auth-level))
			    (:foreign-key (modified-by) (user name))))
    (format t "--- Done.~%")

    (format t "-- Creating blog table...~%")
    (postmodern:execute (:create-table blog
			    ((blog-id :type        (varchar 20)
				      :unique      t
				      :primary-key t)
			     (auth-group :type integer)
			     (blog-name  :type (varchar 255))
			     (sort-order :type integer))
			    (:foreign-key (auth-group) (user-group auth-level))))
    (format t "--- Done.~%")

    (format t "-- Creating links table...~%")
    (postmodern:execute (:create-table links
			    ((link-group :type integer
					 :unique t
					 :primary-key t)
			     (link-title :type (varchar 255))
			     (link-url   :type text))))
    (format t "--- Done.~%")

    (format t "-- Creating menu table...~%")
    (postmodern:execute (:create-table menu
			    ((menu-id :type (varchar 20)
				      :unique t
				      :primary-key t)
			     (menu-name :type (varchar 255))
			     (menu-type :type integer)
			     (menu-pos  :type (varchar 20))
			     (menu-order :type integer)
			     (link-group :type integer))
			    (:foreign-key (link-group) (links link-group))))
    (format t "--- Done.~%"))

  (format t "-- Done.~%")

  (if (cl-fad:file-exists-p "./settings.lisp")
      (format t "WARNING: Previous config file will be overwritten.~%")
      (format t "Config file does not exist. Creating.~%"))

  (with-open-file (file-stream (cl-fad:pathname-as-file "./settings.lisp") 
			       :direction :output
			       :if-exists :supersede
			       :if-does-not-exist :create)
    (format t "Writing config file...~%")
    (let ((plist nil))
      (setf (getf plist :db-name) database-name)
      (setf (getf plist :db-user) database-user)
      (setf (getf plist :db-pass) database-password)
      (setf (getf plist :db-host) database-host)
      (setf (getf plist :db-port) database-port)
      (setf (getf plist :site-name) site-name)
      (setf (getf plist :site-template) "default")
      (setf (getf plist :site-port)  site-port)
      (setf (getf plist :site-email) site-email)
      (with-standard-io-syntax
	(print plist file-stream))))
  (format t "-- Done.~%")

  (format t "Ariadne was successfully installed. Load and run server.lisp to start your server.~%"))


(defun run-uninstaller (&key database-name
			     database-user
			     database-password
			     (database-host "localhost"))
  "DEBUG FEATURE: Clear all tables in database"
  (postmodern:with-connection (list database-name
				    database-user
				    database-password
				    database-host)
    (format t "Dropping tables...~%")
    (postmodern:query (:drop-table 'menu))
    (postmodern:query (:drop-table 'links))
    (postmodern:query (:drop-table 'pages))
    (postmodern:query (:drop-table 'users))
    (postmodern:query (:drop-table 'user_group))
    (format t "Done.~%")))
