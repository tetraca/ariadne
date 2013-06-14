(defun debug-load-config ()
  (let ((config-table  (make-hash-table))
	(read-data     nil)
	(keylist       nil)
	(vallist       nil))

    (with-open-file (config-stream 
		     (cl-fad:pathname-as-file "./Projects/ariadne/settings.lisp")
		     :direction :input)
      (with-standard-io-syntax
	(setq read-data (read config-stream))))

    (mapcar #'(lambda (x)
		(if (keywordp x)
		    (push x keylist)
		    (push x vallist))
		x)
	    read-data)

    (do ((i           0               (incf i))
	 (current-key (elt keylist 0) (elt keylist i))
	 (current-val (elt vallist 0) (elt vallist i)))
	((eql i (length keylist)) nil)
      (setf (gethash current-key config-table) current-val))

    config-table))

