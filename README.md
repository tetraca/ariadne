Ariadne Web Server
==================
Ariadne is a modular, extensible web content management system built in Common Lisp upon the clack framework. It stores data using PostgreSQL.

Ariadne is currently not in a usable state and is under heavy development. So go away.

Required Packages
-----------------
Ariadne is developed using SBCL and is currently not guaranteed to run on any other common lisp distribution. The authorization module is reliant on the sb-ext package's string-to-octets and octets-to-string function.

All packages required to run ariadne are available using [Quicklisp](http://www.quicklisp.org/). In fact, the install script uses quicklisp in order to install several things:
 * Clack
 * Postmodern
 * Alexandria
 * Bordeaux Threads
 * cl-smtp
 * Common Lisp Files and Directories (cl-fad)
 * Common Lisp Portable Perl-compatable Common Regular Expresions (cl-ppcre)
 * Ironclad

Installation
------------
 # Flip to your working directory
 # Load the postmodern and cl-fad libraries
 # Load install.lisp in your lisp installation.
 # Run the installer via (run-installer). Run installer takes several key arguments. Look at install.lisp for details.

You can then run the server via loading server.lisp.
