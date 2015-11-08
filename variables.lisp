(in-package :ftd)

(defvar *numeric-argument-p* nil)

(defun user-name (symbol)
  (format nil "~{~A~^ ~}"
	  (mapcar #'string-capitalize
		  (split-string
		   (string-trim "*" (string symbol))
		   #\-))))

(defun split-string (string char)
  (loop for start = 0 then (1+ end)
	for end = (position char string :start start)
	collect (subseq string start end)
	while end))

(DEFVAR *FLAG-CHARACTER* #\D "Character used to flag entries for deletion.")
(DEFVAR *MARK-CHARACTER* #\* "Characer used to mark entries.")
(DEFVAR *KEPT-NEW-VERSIONS* 2
  "Number of most recent numbered backups to keep.")
(DEFVAR *KEPT-OLD-VERSIONS* 2 "Number of oldest numbered backups to keep.")
(DEFVAR *CHOWN-PROGRAM* "chown" "Program to use as 'chown'")
(DEFVAR *CHGRP-PROGRAM* "chgrp" "Program to use as 'chgrp'")
(DEFVAR *CHMOD-PROGRAM* "chmod" "Program to use as 'chmod'")
(DEFVAR *TOUCH-PROGRAM* "touch" "Program to use as 'touch'")
(DEFVAR *CURSOR-INK* +YELLOW+ "Colour of the cursor")
(DEFVAR *FLAG-INK* +DARK-GREEN+ "Colour of the flags")
(DEFVAR *DELETED-FILES-INK*
  +DARK-RED+
  "Colour of filenames for files flagged for deletion")
(DEFVAR *MARKED-FILES-INK*
  +DARK-BLUE+
  "Colour of filenames for marked files")

(define-presentation-type named-color ()
  :inherit-from 'symbol
  :description "Named color")

(defun all-named-colors ()
  (loop for v being the hash-values of climi::*color-hash-table*
	collect v))

(define-presentation-method accept ((type named-color) stream view &key default default-type)
  (declare (ignore default default-type))
  (multiple-value-bind (object success string)
      (complete-input stream
                      (lambda (so-far action)
                        (complete-from-possibilities
                         so-far (all-named-colors) '() :action action
                         :name-key (lambda (c) (slot-value c 'climi::name))
                         :value-key #'identity))
                      :partial-completers '(#\Space)
                      :allow-any-input t)
    (declare (ignore success string))
    object))


