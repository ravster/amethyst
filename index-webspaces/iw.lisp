;; Copyright 2012 Ravi Desai <rd7190@gmail.com>
;; Distributed under the terms of the GNU Affero GPL version 3 or any later version.

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)
(ql:quickload "osicat")
(import 'osicat:list-directory
	'osicat:current-directory
	'osicat:directory-exists-p
	'osicat:regular-file-exists-p)

;; Variables

(defparameter ignore nil)
(defparameter lib-dir (pathname (truename ".")))
(defparameter output-dir nil)
(defparameter target-dir nil)
(defparameter base-href "")
(defparameter function-to-run nil)

;; Parse command-line
(defparameter list-of-args (copy-list *posix-argv*))
;; Remove the first (Since its the sbcl program)
(pop list-of-args)
;; Parse loop
(loop while list-of-args
     do
     (case (first list-of-args)
       ("-i" (setf ignore t)
	     (return))
       (("-V" "-h") (usage)
	(setf list-of-args nil))
       ("-s" (pop list-of-args)
	     (setf target-dir (pop list-of-args))
	     (setf function-to-run #'scan))
       ("-a" (pop list-of-args)
	     (setf target-dir (pop list-of-args))
	     (setf function-to-run #'apply))
       ("-b" (pop list-of-args)
	     (setf base-href (pop list-of-args)))
       ("-o" (pop list-of-args)
	     (setf output-dir (pop list-of-args)))
       (otherwise (usage)
		  (return))))

;; Run the appropriate function
(funcall function-to-run)
       
(defun usage ()
  (format t "this program accepts the following options:

-i : Ignore missing head and tail files.
-V | -h : Show this message.
-s target-dir : Run scan and output table to STDOUT.
-a target-dir : Apply this program and save the finished file in output-dir.
-o output-dir : Set output-dir
-b base-href : Set the <base href=.../> tag to base-href.~%~%"))

(defun scan-target-dir ()
  "This program returns a string."
  (setf (current-directory) target-dir)
  (let ((list-of-matching-dirs (return-match-list (current-directory)))
	(scan-string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    ;; Extract the directory names and have them in plain string form.
    (setf list-of-matching-dirs
	  (loop for i in list-of-matching-dirs
	       with output = ()
	       do
	       (push (first (last (pathname-directory i)))
		     output)
	       finally (return output)))
    ;; Should make a filter to remove the hidden directories.
    ;; Print links to string
    (loop for i in list-of-matching-dirs do
	 (format scan-string "<a href=\"~A\">~A</a> " i i))
    ;; Return the scan-string
    scan-string))

(defun return-match-list (root-directory)
  "Take a root directory and return a list of the sub-dirs that have matching files."
  (loop for file in (list-directory root-directory)
     with match-list = ()
     do
     ;; If its a directory that matches the criteria, add to the list.
       (if (and (directory-exists-p file)
		(matching-dir-p (list-directory file)))
	   (push file match-list))
     finally (return match-list)))

(defun matching-dir-p (list-of-files)
  "Take a list of files in a directory and return T if there is a file of size >0 in it or its subdirectories."
  (if list-of-files
      (let ((file (first list-of-files)))
	(cond ((and (regular-file-exists-p file)
		    (plusp (with-open-file (s file)
			     (file-length s))))
	       t)
	      ;; If its a directory that matches, return true.
	      ((and (directory-exists-p file)
		    (matching-dir-p (list-directory file)))
	       t)
	      ;; If there is more to the list, run it
	      ((rest list-of-files)
	       (matching-dir-p (rest list-of-files)))	       
	      ;; if anything else, false.
	      (t nil)))
      ;; if list-of-files is nil (End of list), return nil.
      nil))

(defun apply-html ()
  "Call (scan-target-dir).  Add the framework of the html document around it.  Enter timestamp and base-href (If necessary).  Print the new string."
  (let ((scan-string (scan-target-dir))
	(head-string nil)
	(tail-string nil)
	(output-string nil))
    ;; Input index.head
    (with-open-file (s (make-pathname :name "index" :type "head" :defaults lib-dir))
      (setf head-string (make-string (file-length s)))
      (read-sequence head-string s))
    ;; Input index.tail
    (with-open-file (s (make-pathname :name "index" :type "tail" :defaults lib-dir))
      (setf tail-string (make-string (file-length s)))
      (read-sequence tail-string s))
    ;; Put all the strings together
    (setf output-string (concatenate 'string
				     head-string
				     scan-string
				     tail-string))
    ;; Set timestamp
    (setf output-string (regex-replace "Z_ts"
				       output-string
				       (multiple-value-bind
					     (second min hr date mth yr dow dst tz)
					   (get-decoded-time)
					 (declare (ignore second dow dst tz))
					 (format nil "~A-~A-~A ~A:~2,'0D" yr mth date hr min))))
    ;; Set base-href
    (setf output-string (regex-replace "Z_basehref"
				       output-string
				       base-href))
    ;; Done. Print it out.
;    (format t "DONE")))
    (format nil "~A" output-string)))
