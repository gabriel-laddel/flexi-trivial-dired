;; user variable infrastructure from FTD

;;; See licence and disclaimer in application.lisp

(in-package :ftd)

(defparameter *ftd-user-variables* nil
  "User-setable variables")

(defun split-string (string char)
  (loop for start = 0 then (1+ end)
	for end = (position char string :start start)
	collect (subseq string start end)
	while end))

(defun user-name (symbol)
  (format nil "~{~A~^ ~}"
	  (mapcar #'string-capitalize
		  (split-string
		   (string-trim "*" (string symbol))
		   #\-))))

(defmacro define-variable (name
			   &optional
			   (val nil)
			   (docstring "Undocumented")
			   (p-type t))
  (let* ((user-name (user-name name))
	 (entry (cons user-name (list :value (cons name p-type)))))
     `(progn
	(defvar ,name ,val ,docstring)
	(push ',entry *ftd-user-variables*))))

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
