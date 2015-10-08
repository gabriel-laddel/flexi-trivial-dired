;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: FTD; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Ftd, the Flexi-Trivial Dired
;;;   Created: 2005-12-04
;;;    Author: John Q. Splittist (splittist @ yahoo.com)
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005, 2006 by John Q. Splittist

;;; 
;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;; 

(in-package :ftd)

(defclass ftd-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (display-info-for-directory
   pane
   (pane-directory (master-pane pane))))

(defgeneric display-info-for-directory (pane directory))

(defclass ftd-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20))

(defclass ftd-pane (esa-pane-mixin application-pane)
  ((directory :initarg :directory :initform nil :accessor pane-directory)
   (cursor-line :initarg :cursor-line :initform 0 :accessor pane-cursor-line))
  (:default-initargs
      :directory (make-directory (user-homedir-pathname))
    :width 700 :height 800
    :display-function 'display-my-pane
    :display-time nil
    :incremental-redisplay t
    :end-of-line-action :allow
    :end-of-page-action :allow
    :command-table 'global-ftd-table))

(defun make-window (directory)
  (with-look-and-feel-realization
      ((frame-manager *application-frame*) *application-frame*)
    (let* ((new-pane (make-pane 'ftd-pane :directory directory))
	   (new-info-pane (make-pane 'ftd-info-pane
				     :master-pane new-pane
				     :width 700)))
      (let ((new-window (vertically ()
			  (scrolling ()
			    new-pane)
			  new-info-pane)))
	(push (cons new-pane new-window) (pane/window *application-frame*))
	(values new-window new-pane)))))
					 
(defclass tab-layout-mixin ()
     ((pane/window :accessor pane/window :initform nil)))

(define-application-frame ftd (standard-application-frame
			       tab-layout-mixin
			       esa-frame-mixin)
  ()
  (:panes
   (window (let* ((my-pane (make-pane 'ftd-pane
				     :width 700 :height 800
				     :display-function 'display-my-pane
				     :display-time nil
				     :incremental-redisplay t
				     :command-table 'global-ftd-table))
		  (my-info-pane (make-pane 'ftd-info-pane
					   :master-pane my-pane 
					   :width 700))
		  (my-window
		   (vertically ()
		     (scrolling ()
		       my-pane)
		     my-info-pane)))
	     (setf (windows *application-frame*) (list my-pane))
	     (setf (pane/window *application-frame*) (list (cons my-pane my-window)))
	     my-window))
   (minibuffer (make-pane 'ftd-minibuffer-pane :width 700)))
;;   (:pointer-documentation t)
  (:layouts
   (default
       (vertically ()
	 (with-tab-layout ('pane :name 'tab-layout-pane)
	   ((last1 (pathname-directory (user-homedir-pathname))) window))
	 (20 minibuffer))))
  (:top-level (esa-top-level)))

(defun current-pane ()
  (car (rassoc
	(tab-layout::tab-pane-pane
	 (enabled-pane
	  (find-pane-named *application-frame* 'tab-layout-pane)))
	(pane/window *application-frame*))))

(defmethod find-applicable-command-table ((frame tab-layout-mixin))
  (declare (ignore frame))
  (command-table (current-pane)))

(defun display-my-pane (frame pane)
  (declare (ignore frame))
  (loop with entries = (entries (pane-directory pane))
	with now = (get-universal-time)
	for count below (flexichain:nb-elements entries)
	for entry = (flexichain:element* entries count)
	maximizing (slot-value entry 'link-count) into max-link-count
	maximizing (slot-value entry 'size) into max-size
	finally
     (loop for count below (flexichain:nb-elements entries)
	   for entry = (flexichain:element* entries count)
	   with link-count-width
	     = (max 4 (ftd-directory::integer-string-length max-link-count))
	   with size-width = (ftd-directory::integer-string-length max-size)
	   do
	(with-slots (flag mode link-count uid gid size mtime name) entry
	   (updating-output (pane 
			     :cache-value (list flag mode uid gid mtime)
			     :cache-test #'equal
			     :unique-id name)
	     (with-output-as-presentation
		 (pane entry 'ftd-entry :single-box t) 
	       (fresh-line pane)
	       (with-drawing-options
		   (pane :ink *flag-ink*)
		 (princ flag pane))
	       (format pane "  ~A" (ftd-directory:mode->string mode))
	       (format pane "  ~vD" link-count-width link-count)
	       (format pane "  ~8A  ~8A"
		       (ftd-directory:uid->name uid)
		       (ftd-directory:gid->name gid))
	       (format pane "  ~vD" size-width size)
	       (format pane "  ~A" (ftd-directory:ls-time-string
				    (ftd-directory:unix->universal mtime)
				    now)) 
	       (with-drawing-options
		   (pane :ink (if (char= flag *flag-character*)
				  *deleted-files-ink*
				  *marked-files-ink*))
		 (format pane "  ~A" name)
		 (when (typep entry 'ftd-link-entry)
		   (format pane " -> ~A" (entry-linkname entry)))))))
	(draw-cursor pane
		     (pane-cursor-line pane)
	     size-width link-count-width))))

(defun draw-cursor (pane cursor-line size-width link-count-width)
  (let* ((text-style (medium-text-style pane))
	 (ascent (text-style-ascent text-style pane))
	 (descent (text-style-descent text-style pane))
	 (spacing (stream-vertical-spacing pane))
	 (height (+ ascent descent spacing))
	 (width (text-style-width text-style pane))
	 (cursor-left (* width (+ size-width link-count-width 53)))
	 (cursor-top (* cursor-line height))
	 (cursor-bottom (+ cursor-top ascent descent)))
    (updating-output (pane :unique-id -1)
      (draw-rectangle* pane
		       cursor-left cursor-top
		       (+ cursor-left 3) cursor-bottom
		       :ink *cursor-ink*))))

(defun ftd ()
  "Starts up the FTD application"
  (let ((frame (make-application-frame
		'ftd)))
    (run-frame-top-level frame)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Conditions

(define-condition shell-error (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Error executing shell command.")))
  (:documentation "This condition is signalled whenever a shell subprocess
returns an error"))

(define-condition no-such-file (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "No such file.")))
  (:documentation "This condition is signalled when the user attempts to
act on a non-existent file"))

(define-condition no-such-directory (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "No such directory.")))
  (:documentation "This condition is signalled when the user attempts to
act on a non-existent directory"))

(defmethod execute-frame-command :around ((frame ftd) command)
  (declare (ignore command))
  (handler-case
      (call-next-method)
    (shell-error ()
      (beep) (display-message "Error executing shell command"))
    (no-such-file ()
      (beep) (display-message "No such file"))
    (no-such-directory ()
      (beep) (display-message "No such directory"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; External programs

#+openmcl
(defun run-shell-program (name args)
  (with-output-to-string (stream)
    (let ((proc
	   (ccl:run-program name args
			    :output stream)))
      (multiple-value-bind (status exit-code)
	  (ccl:external-process-status proc)
	(assert (and (eq status :exited)
		      (= exit-code 0))
		()
		(make-condition 'shell-error))))))

#+sbcl
(defun run-shell-program (name args)
  (with-output-to-string (stream)
    (let ((proc
	   (sb-ext:run-program name args
			:output stream :search t)))
      (let ((status (sb-ext:process-status proc))
	    (exit-code (sb-ext:process-exit-code proc)))
	(assert (and (eq status :exited)
		     (= exit-code 0))
		()
		(make-condition 'shell-error))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Datastructures

(defclass ftd-container ()
     ((entries :initarg :entries :accessor entries :initform nil)
      (pathname :initarg :pathname :accessor directory-pathname :initform nil)))

(defclass ftd-directory (ftd-container)
     ())

(defmethod print-object ((object ftd-directory) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (directory-pathname object) stream)))

(defmethod display-info-for-directory (pane (directory ftd-directory))
  (format pane " (Directory) ~A"
	  (namestring (directory-pathname directory))))

(defun make-directory (pathname)
  (let ((entries (make-directory-entries pathname)))
    (when entries			; should always be . and ..
      (make-instance 'ftd-directory
	 :pathname pathname
	 :entries (make-instance 'flexichain:standard-flexichain
		     :initial-contents entries)))))

(defclass ftd-file-list (ftd-container)
     ((name :initarg :name :accessor list-name :initform nil)
      (pathnames :initarg :pathnames :accessor pathnames)))

(defmethod print-object ((object ftd-file-list) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (list-name object) stream)))

(defmethod display-info-for-directory (pane (directory ftd-file-list))
  (format pane " (List) ~A [~A]"
	  (list-name directory)
	  (namestring (directory-pathname directory))))

(defun make-file-list (name pathnames)
  (multiple-value-bind (entries prefix)
      (make-entries pathnames)
    (make-instance 'ftd-file-list
       :name name
       :pathname (cl-fad:pathname-as-directory prefix)
       :pathnames pathnames
       :entries (make-instance 'flexichain:standard-flexichain
		   :initial-contents entries))))

(defclass ftd-wild-list (ftd-container)
     ((wild-pathname :initarg :wild-pathname :accessor wild-pathname :initform nil)))

(defmethod display-info-for-directory (pane (directory ftd-wild-list))
  (format pane " (Wild) ~A"
	  (namestring (wild-pathname directory))))

(defun make-wild-list (wild-form)
  (multiple-value-bind (entries prefix suffix)
      (make-wild-entries wild-form)
    (when entries
      (values
       (make-instance 'ftd-wild-list
	  :pathname (cl-fad:pathname-as-directory prefix)
	  :wild-pathname wild-form
	  :entries (make-instance 'flexichain:standard-flexichain
		      :initial-contents entries))
       suffix))))

(defclass ftd-system-list (ftd-container)
     ((system-name :initarg :system-name :accessor system-name)))

(defmethod display-info-for-directory (pane (directory ftd-system-list))
  (format pane " (System) ~A [~A]"
	  (system-name directory)
	  (namestring (directory-pathname directory))))

(defun make-system-list (system-name)
  (let ((pathnames (get-system-pathnames system-name)))
    (when pathnames
      (multiple-value-bind (entries prefix)
	  (make-entries pathnames)
	(make-instance 'ftd-system-list
	   :system-name system-name
	   :pathname (cl-fad:pathname-as-directory prefix)
	   :entries (make-instance 'flexichain:standard-flexichain
		       :initial-contents entries))))))

(defclass ftd-find-name-list (ftd-container)
     ((find-directory :initarg :find-directory :accessor find-directory)
      (find-name :initarg :find-name :accessor find-name)))

(defmethod display-info-for-directory (pane (directory ftd-find-name-list))
  (format pane " (Find Name) ~A [~A]"
	  (find-name directory)
	  (namestring (directory-pathname directory))))

(defun make-find-name-list (find-directory find-name)
  (let ((pathnames (get-find-name-matches find-directory find-name)))
    (when pathnames
      (multiple-value-bind (entries prefix)
	  (make-entries pathnames)
	(make-instance 'ftd-find-name-list
	   :find-directory find-directory
	   :find-name find-name
	   :pathname (cl-fad:pathname-as-directory prefix)
	   :entries (make-instance 'flexichain:standard-flexichain
		       :initial-contents entries))))))

(defclass ftd-find-grep-list (ftd-container)
     ((find-directory :initarg :find-directory :accessor find-directory)
      (grep-regex :initarg :grep-regex :accessor grep-regex)))

(defmethod display-info-for-directory (pane (directory ftd-find-grep-list))
  (format pane " (Find Grep) ~A [~A]"
	  (grep-regex directory)
	  (namestring (directory-pathname directory))))

(defun make-find-grep-list (find-directory grep-regex)
  (let ((pathnames (get-find-grep-matches find-directory grep-regex)))
    (when pathnames
      (multiple-value-bind (entries prefix)
	  (make-entries pathnames)
	(make-instance 'ftd-find-grep-list
	   :find-directory find-directory
	   :grep-regex grep-regex
	   :pathname (cl-fad:pathname-as-directory prefix)
	   :entries (make-instance 'flexichain:standard-flexichain
		       :initial-contents entries))))))

(defclass ftd-find-list (ftd-container)
     ((find-directory :initarg :find-directory :accessor find-directory)
      (find-args :initarg :find-args :accessor find-args)))

(defmethod display-info-for-directory (pane (directory ftd-find-list))
  (format pane " (Find) ~A [~A]"
	  (find-args directory)
	  (namestring (directory-pathname directory))))

(defun make-find-list (find-directory find-args)
  (let ((pathnames (get-find-matches find-directory find-args)))
    (when pathnames
      (multiple-value-bind (entries prefix)
	  (make-entries pathnames)
	(make-instance 'ftd-find-list
	   :find-directory find-directory
	   :find-args find-args
	   :pathname (cl-fad:pathname-as-directory prefix)
	   :entries (make-instance 'flexichain:standard-flexichain
		       :initial-contents entries))))))

(defclass ftd-entry ()
     ((pathname :initarg :pathname :accessor entry-pathname :initform nil)
      (flag :initarg :flag :accessor entry-flag :initform #\Space)
      (name :initarg :name :accessor entry-name)
      (device :initarg :device :accessor entry-device)
      (inode :initarg :inode :accessor entry-inode)
      (mode :initarg :mode :accessor entry-mode)
      (link-count :initarg :link-count :accessor entry-link-count)
      (uid :initarg :uid :accessor entry-uid)
      (gid :initarg :gid :accessor entry-gid)
      (special-device-type :initarg :special-device-type
			   :accessor entry-special-device-type)
      (atime :initarg :atime :accessor entry-atime)
      (mtime :initarg :mtime :accessor entry-mtime)
      (ctime :initarg :ctime :accessor entry-ctime)
      (size :initarg :size :accessor entry-size)
      (block-count :initarg :block-count :accessor entry-block-count)
      (block-size :initarg :block-size :accessor entry-block-size)))

(defun entry-owner (ftd-entry)
  (ftd-directory:uid->name (entry-uid ftd-entry)))

(defun entry-group (ftd-entry)
  (ftd-directory:gid->name (entry-gid ftd-entry)))

(defclass ftd-directory-entry (ftd-entry) ())

(defclass ftd-file-entry (ftd-entry) ())

(defclass ftd-link-entry (ftd-entry)
     ((linkname :initarg :linkname :accessor entry-linkname)))

(defclass ftd-other-entry (ftd-entry) ())

(defmethod print-object ((object ftd-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (entry-name object) stream)))

(define-presentation-method present (object (type ftd-entry)
					    stream view &key)
  (declare (ignore view))
  (princ (entry-name object) stream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filling datastructures



(defun get-system-pathnames (system-name)
  (let ((system (asdf:find-system system-name))
	(files nil))
    (labels ((do-it (candidates)
	       (let ((modules nil))
		 (loop for candidate in candidates
		       when (typep candidate 'asdf:source-file)
			 do (push (asdf:component-pathname candidate) files)
		       when (typep candidate 'asdf:module)
			 do (push candidate modules)
		       finally (dolist (module modules)
				 (do-it (asdf:module-components module)))))))
      (do-it (asdf:module-components system)))
    files))

(defun common-prefix (pathnames)
  (loop with first-string = (namestring (first pathnames))
	with length = (length first-string)
	for pathname in (rest pathnames)
	do (setf length (min length
			     (or (mismatch (namestring pathname)
					   first-string)
				 length)))
	finally (let ((directory-prefix-length
		       (1+ (position #\/ first-string :end length :from-end t))))
		  (return (values (subseq first-string 0 directory-prefix-length)
				  directory-prefix-length)))))

(defun non-wild-prefix (wild-pathname)
  (let* ((name (namestring wild-pathname))
	 (asterisk (position *mark-character* name))
	 (preceding-slash (position #\/ name :end asterisk :from-end t)))
    (values (subseq name 0 (1+ preceding-slash))
	    (subseq name (1+ preceding-slash)))))

(defun make-entries (pathnames)
  (multiple-value-bind (prefix prefix-length)
      (common-prefix pathnames)
    (let ((entries
	   (loop for pathname in pathnames
		 for namestring = (namestring pathname)
		 collecting (make-entry-from-stat
				 pathname
				 (subseq namestring prefix-length)))))
      (values entries prefix))))

(defun make-wild-entries (wild-form)
  (let ((pathnames (directory wild-form #+openmcl #+openmcl :directories t)))
    (when pathnames
      (multiple-value-bind (prefix suffix)
	  (non-wild-prefix wild-form)
	(let ((entries
	       (loop for pathname in pathnames
		     for namestring = (namestring pathname)
		     with prefix-length = (length prefix)
		     collecting (make-entry-from-stat
				     pathname
				     (subseq namestring prefix-length)))))
	  (values entries prefix suffix))))))

(defun concat-pathnames (file directory)
  (concatenate 'string (namestring directory) file))

(defun make-directory-entries (pathname)
  (let ((names (ftd-directory:directory-names (namestring pathname))))
    (loop for name in names
	  collecting (make-entry-from-stat
		      (concat-pathnames name pathname)
			  name))))

(defgeneric new-listing (container))

(defmethod new-listing ((dir ftd-directory))
  (make-directory-entries (directory-pathname dir)))

(defmethod new-listing ((dir ftd-file-list))
  (make-entries (pathnames dir)))

(defmethod new-listing ((dir ftd-wild-list))
  (make-wild-entries (wild-pathname dir)))

(defmethod new-listing ((dir ftd-system-list))
  (make-entries (get-system-pathnames (system-name dir))))

(defmethod new-listing ((dir ftd-find-name-list))
  (make-entries
   (get-find-name-matches (find-directory dir)
			  (find-name dir))))

(defmethod new-listing ((dir ftd-find-grep-list))
  (make-entries
   (get-find-grep-matches (find-directory dir)
			  (grep-regex dir))))

(defmethod new-listing ((dir ftd-find-list))
  (make-entries
   (get-find-matches (find-directory dir)
		     (find-args dir))))

(defun make-entry-from-stat (pathname name)
  (multiple-value-bind
      (type device inode mode link-count uid gid
	    special-device-type
	    atime mtime ctime
	    size block-count block-size
	    link-name)
      (ftd-directory:stat-la (#+sbcl namestring #+openmcl string pathname))
    (let ((entry
	   (make-instance (case type
			    (:file 'ftd-file-entry)
			    (:directory 'ftd-directory-entry)
			    (:link 'ftd-link-entry)
			    (t 'ftd-other-entry))
	      :name name :pathname pathname :device device :inode inode
	      :mode mode :link-count link-count :uid uid :gid gid
	      :special-device-type special-device-type
	      :atime atime :mtime mtime :ctime ctime :size size
	      :block-count block-count :block-size block-size)))
      (when (eq type :link)
	(setf (entry-linkname entry) link-name))
      entry)))

(defun update-entries (entries)
  (let ((new-entries
	 (make-entries (mapcar #'entry-pathname entries)))) 
    (loop for entry in entries
	  for new-entry = (find (entry-pathname entry) new-entries
				:key #'entry-pathname :test #'equal)
	  do (dolist (slot
		      '(mode links owner group size month day year/time))
	       (setf (slot-value entry slot)
		     (slot-value new-entry slot))))))

(defun add-entries (pane pathnames)
  (let* ((new-entries
	  (make-entries pathnames))
	 (dir (pane-directory pane))
	 (entries (entries dir))
	 (index (pane-cursor-line pane)))
    (loop for entry in new-entries
	  do (flexichain:insert* entries index entry))))

(defun remove-entries (pane entries)
  (let* ((dir (pane-directory pane))
	 (original-entries (entries dir)))
    (dolist (entry entries)
      (loop for i below (flexichain:nb-elements original-entries)
	    for original-entry = (flexichain:element* original-entries i)
	    when (eql original-entry entry)
	      do (flexichain:delete* original-entries i)
		 (loop-finish)))))

(defun maybe-adjust-cursor (pane)
  (let ((new-limit (1- (flexichain:nb-elements
			       (entries (pane-directory pane))))))
	   (when (> (pane-cursor-line pane) new-limit)
	     (setf (pane-cursor-line pane) new-limit))))

(defun delete-entries (pane entries)
   (when (accept 'boolean
		 :prompt (format nil "Delete ~{~A~^, ~}"
				 (mapcar #'entry-name entries)))
     (let ((deleted-entries ()))
       (dolist (entry entries)
	 (when
	     (handler-bind
		 ((file-error (lambda (e) (declare (ignore e)) (go loop-end))))
	       (delete-file (entry-pathname entry)))
	   (push entry deleted-entries))
	 loop-end)
       (when deleted-entries
	 (remove-entries pane deleted-entries)
	 (maybe-adjust-cursor pane)
	 (setf (pane-needs-redisplay pane) t))
       (display-message "Deleted ~D file~:P" (length deleted-entries)))))

(defun copy-entries (pane entries destination)
  (let* ((dir (pane-directory pane))
	 (dir-pathname (directory-pathname dir)))
    (run-shell-program "cp"
		       `(,@(mapcar (lambda (entry)
				     (namestring (entry-pathname entry)))
				   entries)
			   ,(namestring destination)))
    (when (and (not (cl-fad:directory-exists-p destination))
	       (equal (pathname-directory dir-pathname)
		      (pathname-directory destination)))
      (add-entries pane (list destination))
      (setf (pane-needs-redisplay pane) t))))

(defun rename-entries (pane entries destination)
  (let* ((dir (pane-directory pane))
	 (dir-pathname (directory-pathname dir)))
    (run-shell-program "mv"
		       `(,@(mapcar (lambda (entry)
				     (namestring (entry-pathname entry)))
				   entries)
			   ,(namestring destination)))
    (when (and (not (cl-fad:directory-exists-p destination))
	       (equal (pathname-directory dir-pathname)
		      (pathname-directory destination)))
      (add-entries pane (list destination)))
    (remove-entries pane entries)
    (setf (pane-needs-redisplay pane) t)))

(defun link-entries (pane entries destination &optional symlink)
  (let* ((dir (pane-directory pane))
	 (dir-pathname (directory-pathname dir))
	 (arg-strings (mapcar (lambda (entry)
				     (namestring (entry-pathname entry)))
				   entries)))
    (when symlink (push "-s" arg-strings))
    (run-shell-program "ln" arg-strings)
    (when (and (not (cl-fad:directory-exists-p destination))
	       (equal (pathname-directory dir-pathname)
		      (pathname-directory destination)))
      (add-entries pane (list destination))
      (setf (pane-needs-redisplay pane) t))))

(defun replace-last-element (entry new-element)
  (let ((pathname (entry-pathname entry)))
    (if (cl-fad:directory-pathname-p pathname)
	(make-pathname :directory (append (butlast (pathname-directory pathname))
					  (list new-element)))
	(merge-pathnames new-element pathname))))

(defun change-case-entries (pane entries change-function)
  (loop for entry in entries
	for pathname = (entry-pathname entry)
	for new-name = (funcall change-function
				(entry-name entry))
	for new-pathname = (replace-last-element entry new-name)
	when (ignore-errors (rename-file pathname new-pathname))
	  collect new-pathname into new-pathnames
	  and collect entry into old-entries
	finally (when new-pathnames
		  (add-entries pane new-pathnames)
		  (remove-entries pane old-entries)
		  (setf (pane-needs-redisplay pane) t))
		(display-message "Renamed ~D file~:P" (length old-entries))))

(defun regexp-rename-entries (pane entries from to)
  (loop with from-regexp = (cl-ppcre:create-scanner from)
	for entry in entries
	for pathname = (entry-pathname entry)
	for new-name = (cl-ppcre:regex-replace from-regexp (entry-name entry) to)
	for new-pathname = (replace-last-element entry new-name)
	when (ignore-errors (rename-file pathname new-pathname))
	  collect new-pathname into new-pathnames
	  and collect entry into old-entries
	finally (when new-pathnames
		  (add-entries pane new-pathnames)
		  (remove-entries pane old-entries)
		  (setf (pane-needs-redisplay pane) t))
		(display-message "Renamed ~D file~:P" (length old-entries))))

(defun regexp-link-entries (pane entries from to &optional symlink)
  (loop with from-regexp = (cl-ppcre:create-scanner from)
	for entry in entries
	for pathname = (entry-pathname entry)
	for new-name = (cl-ppcre:regex-replace from-regexp (entry-name entry) to)
	for new-pathname = (replace-last-element pathname new-name)
	for argstrings = (list (namestring pathname) new-pathname)
	when symlink do (push "-s" argstrings)
	when (and (ignore-errors (run-shell-program "ln" argstrings) t)
		  (and (not (cl-fad:directory-exists-p new-pathname))
			(equal (pathname-directory (directory-pathname
						    (pane-directory pane)))
			       (pathname-directory new-pathname))))
	  collect new-pathname into new-pathnames
	finally (when new-pathnames
		  (add-entries pane new-pathnames)
		  (setf (pane-needs-redisplay pane) t))))

(defun regexp-copy-entries (pane entries from to)
  (loop with from-regexp = (cl-ppcre:create-scanner from)
	for entry in entries
	for pathname = (entry-pathname entry)
	for new-name = (cl-ppcre:regex-replace from-regexp (entry-name entry) to)
	for new-pathname = (replace-last-element pathname new-name)
	for argstrings = (list (namestring pathname) new-pathname)
	when (and (ignore-errors (run-shell-program "cp" argstrings) t)
		  (and (not (cl-fad:directory-exists-p new-pathname))
		       (equal (pathname-directory (directory-pathname
						   (pane-directory pane)))
			      (pathname-directory new-pathname))))
	  collect new-pathname into new-pathnames
	finally (when new-pathnames
		  (add-entries pane new-pathnames)
		  (setf (pane-needs-redisplay pane) t))))

;; (defun parse-ls-line (line)
;;   (let ((length (length line))
;; 	(start 0)
;; 	(end 0)
;; 	(list ()))
;;     (flet  ((next-bit ()
;; 	      (loop until (char= (char line end) #\Space)
;; 		    while (< end length)
;; 		    do (incf end))
;; 	      (loop while (char= (char line end) #\Space)
;; 		    while (< end length)
;; 		    do (incf end))))
;;       (loop repeat 8
;; 	    do (next-bit)
;; 	       (push (string-trim " " (subseq line start (1- end))) list)
;; 	       (setf start end))
;;       (cons (subseq line end) (nreverse list)))))

(defun get-grep-matches (dir regex)
  (split-lines
   (run-shell-program "grep"
		      `("-l" ,regex
			     ,@(mapcar (lambda (entry)
					 (namestring
					  (entry-pathname entry)))
				       (all-entries dir))))))

(defun get-find-name-matches (dir name)
  (split-lines
   (run-shell-program
    "sh"
    (list "-c"
	  (format nil "find ~A -name \"~A\""
		  dir name)))))

(defun get-find-grep-matches (dir regex)
  (split-lines
   (run-shell-program
    "sh"
    (list "-c" (format nil "find ~A -print0 | xargs -0 grep -l ~A"
		       dir regex)))))

(defun get-find-matches (dir args)
  (mapcar (lambda (filename)
	    (expand-filename filename dir))
	  (split-lines
	   (run-shell-program
	    "sh"
	    (list "-c" (format nil "cd ~A; find ~A"
			       dir args))))))


(defun run-ch/touch (entries program flags arg)
  (run-shell-program
   "sh"
   (list "-c"
	 (format nil "~A ~A ~A ~{~A ~}"
		 program
		 flags
		 arg
		 (mapcar (lambda (entry)
			   (namestring (entry-pathname entry)))
			 entries)))))

(defun chmod-entries (entries mode)
  (run-ch/touch entries *chmod-program* "-f" mode))

(defun chgrp-entries (entries group)
  (run-ch/touch entries *chgrp-program* "-f" group))

(defun chown-entries (entries owner)
  (run-ch/touch entries *chown-program* "-f" owner))

(defun touch-time (entries time &optional (which :both))
  (run-ch/touch
   entries
   *touch-program*
   (format nil "-c~C" (ecase which
			(:both #\Space)
			(:modification #\m)
			(:access #\a)))
   time))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Helper functions

(defun last1 (list)
  (car (last list)))

(defun split-lines (string)
  (loop for start = 0 then (1+ end)
	for end = (position #\Newline string :start start)
	while end
	collect (subseq string start end)))

(defun all-entries (ftd-container)
  (let ((entries (entries ftd-container)))
    (when entries
      (loop for index below (flexichain:nb-elements entries)
	    collecting (flexichain:element* entries index)))))

(defun add-tab-for-directory (directory title &optional (enabled t))
  (multiple-value-bind (window pane)
	    (make-window directory)
	  (let ((tab-pane (find-pane-named *application-frame* 'tab-layout-pane))
		(new-tab
		 (make-tab-pane-from-list title
					  window)))
	    (add-pane new-tab tab-pane t)
	    (setf (pane-needs-redisplay pane) enabled))))

(defun close-tab (pane)
  (cond ((cdr (pane/window *application-frame*))
	 (let ((pane/window (find pane (pane/window *application-frame*)
				  :test #'equal :key #'car)))
	   (setf (pane/window *application-frame*)
		 (delete pane/window (pane/window *application-frame*)))
	 (remove-pane (cdr pane/window)
		      (find-pane-named *application-frame* 'tab-layout-pane))))
	(t
	 (beep)
	 (display-message "Can't close the last tab. Type C-x C-c to quit."))))

(defun maybe-scroll-pane (pane)
  (let* ((pvr (pane-viewport-region pane))
	 (x (rectangle-min-x pvr))
	 (min-y (rectangle-min-y pvr))
	 (max-y (rectangle-max-y pvr))
	 (cursor-line (pane-cursor-line pane))
	 (text-style (medium-text-style pane))
	 (ascent (text-style-ascent text-style pane))
	 (descent (text-style-descent text-style pane))
	 (spacing (stream-vertical-spacing pane))
	 (height (+ ascent descent spacing))
	 (cursor-top (* cursor-line height))
	 (cursor-bottom (+ cursor-top ascent descent)))
    (cond ((> cursor-bottom max-y)
	   (scroll-extent pane x (+ min-y (- cursor-bottom max-y))))
	  ((< cursor-top min-y)
	   (scroll-extent pane x (max 0 cursor-top))))))

(defun page-up/down (pane &optional down)
  (let* ((pvr (pane-viewport-region pane))
	 (x (rectangle-min-x pvr))
	 (min-y (rectangle-min-y pvr))
	 (max-y (rectangle-max-y pvr))
	 (viewport-height (- max-y min-y))
	 (cursor-line (pane-cursor-line pane))
	 (text-style (medium-text-style pane))
	 (ascent (text-style-ascent text-style pane))
	 (descent (text-style-descent text-style pane))
	 (spacing (stream-vertical-spacing pane))
	 (height (+ ascent descent spacing))
	 (viewport-lines (floor viewport-height height))
	 (last-cursor-line (flexichain:nb-elements
			    (entries (pane-directory pane)))))
    (cond (down
	   (scroll-extent pane x (min (+ min-y viewport-height)
				      (- (* last-cursor-line height) viewport-height)))
	   (goto-line pane
		 (min (1- last-cursor-line) (+ cursor-line viewport-lines))))
	  (t
	   (scroll-extent pane x (max (- min-y viewport-height)
				      0))
	   (goto-line pane
		 (max 0 (- cursor-line viewport-lines)))))))

(defun next-line (pane &optional (count 1)) 
  (let* ((dir (pane-directory pane))
	 (length (flexichain:nb-elements (entries dir))))
    (if (plusp count)
	(loop repeat count
	      while (< (pane-cursor-line pane) (1- length))
	      do (incf (pane-cursor-line pane)))
	(loop repeat (- count)
	      while (> (pane-cursor-line pane) 0)
	      do (decf (pane-cursor-line pane)))))
  (maybe-scroll-pane pane)
  (setf (pane-needs-redisplay pane) t))

(defun goto-line (pane &optional line-number)
  (let* ((dir (pane-directory pane))
	 (last (1- (flexichain:nb-elements (entries dir))))
	 (line (or line-number last)))
    (when (<= 0 line last)
      (setf (pane-cursor-line pane) line)
      (maybe-scroll-pane pane)
      (setf (pane-needs-redisplay pane) t))))

(defun mark-file (pane flag &optional (count 1))
  (let* ((dir (pane-directory pane))
	 (length (flexichain:nb-elements (entries dir))))
    (if (plusp count)
	(loop repeat count
	      do (setf (entry-flag (flexichain:element* (entries dir)
							(pane-cursor-line pane)))
		       flag)
	      while (< (pane-cursor-line pane) (1- length))
	      do (incf (pane-cursor-line pane)))
	(loop repeat (- count)
	      while (> (pane-cursor-line pane) 0)
	      do (decf (pane-cursor-line pane))
		 (setf (entry-flag (flexichain:element* (entries dir)
							(pane-cursor-line pane)))
		       flag))))
  (setf (pane-needs-redisplay pane) t))

(defun mark-files (pane entries flag)
  (dolist (entry entries)
    (setf (entry-flag entry) flag))
  (setf (pane-needs-redisplay pane) t))

(defun mark-excess-backups (pane flag)
  (let* ((dir (pane-directory pane))
	 (entries (entries dir))
	 (length (flexichain:nb-elements entries))
	 (regex (cl-ppcre:create-scanner "^(.*)\\.~([0-9]+)~$"))
	 (candidates (make-hash-table :test 'equal))
	 (changes 0))
    (dotimes (i length)
      (let ((entry (flexichain:element* entries i)))
	(multiple-value-bind (match name+number)
	    (cl-ppcre:scan-to-strings regex (entry-name entry))
	  (when match
	    (push (cons entry (parse-integer (aref name+number 1)))
		  (gethash (aref name+number 0) candidates))))))
    (loop with limit = (+ *kept-new-versions* *kept-old-versions*)
	  for candidate-list being each hash-value of candidates
	  for candidate-list-length = (length candidate-list)
	  when (> candidate-list-length limit)
	    do (let ((candidate-list (sort candidate-list #'< :key #'cdr)))
		 (dolist (candidate (subseq candidate-list
					    *kept-old-versions*
					    (- candidate-list-length
					       *kept-new-versions*)))
		   (setf (entry-flag (car candidate)) flag)
		   (incf changes))))
    (when (> changes 0)
      (setf (pane-needs-redisplay pane) t))
    (display-message "Flagged ~D file~:P" changes)))

(defun do-entries (pane test action &optional (message "Changed ~D entr~:@P"))
  (let* ((dir (pane-directory pane))
	 (entries (entries dir)))
    (loop with changes = 0
	  for i below (flexichain:nb-elements entries)
	  for entry = (flexichain:element* entries i)
	  when (funcall test entry)
	    do (funcall action entry)
	       (incf changes)
	  finally (funcall #'display-message message changes)
		  (when (> changes 0)
		    (setf (pane-needs-redisplay pane) t)))))

(defun mark-eval (pane string)
  (do-entries pane
    (lambda (entry)
      (let ((*package* (find-package :ftd))
	    (*entry* entry))
	(declare (special *entry*))
	(ignore-errors (eval (read-from-string string)))))
    (lambda (entry)
      (setf (entry-flag entry) *mark-character*))
    "Marked ~D file~:P"))

(defun entry-symlink-p (entry)
  (typep entry 'ftd-link-entry))

(defun entry-directory-p (entry)
  (typep entry 'ftd-directory-entry))

(defun entry-executable-p (entry)
  (ftd-directory:executablep (entry-mode entry)))

(defun next/prev-marked-file (pane &optional backward)
  (let* ((dir (pane-directory pane))
	 (entries (entries dir))
	 (end (1- (flexichain:nb-elements entries))))
    (loop for line = (+ (pane-cursor-line pane)
			(if backward -1 +1))
	    then (if backward
		     (1- line)
		     (1+ line))
	  while (<= 0 line end) 
	  unless (char= (entry-flag (flexichain:element* entries line)) #\Space)
	    do (goto-line pane line)
	       (loop-finish))))

(defun rotate-marks (pane mark-list)
  (let ((divisor (length mark-list)))
    (do-entries pane
      (lambda (entry)
	(member (entry-flag entry) mark-list))
      (lambda (entry)
	(setf (entry-flag entry)
	      (nth (mod (1+ (position (entry-flag entry) mark-list))
			divisor)
		   mark-list)))
      "Changed ~D mark~:P")))

(defun remove-all-marks (pane)
  (do-entries pane
    (lambda (entry)
      (char/= #\Space (entry-flag entry)))
    (lambda (entry)
      (setf (entry-flag entry) #\Space))
    "Removed ~D mark~:P"))

(defun read-character (prompt)
  (let ((pane (find-pane-named *application-frame* 'minibuffer)))
    (window-clear pane)
    (write-string prompt pane)
    (loop for gesture = (read-gesture)
	  until (characterp gesture)
	  finally (return gesture))))

(defun replace-marks (pane old-mark new-mark)
  (do-entries pane
    (lambda (entry)
      (char= (entry-flag entry) old-mark))
    (lambda (entry)
      (setf (entry-flag entry) new-mark))
    "Replaced ~D mark~:P"))

(defun y-or-n-or-just-do-it-p (prompt just-do-it)
  (or just-do-it
      (loop for answer = (read-character
			  (format nil "~&~A? " prompt))
	      then (read-character
			(format nil "~&A (y/n/!)? "))
	    until (member answer '(#\y #\n #\!))
	    finally (return (case answer
			      (#\y (values t just-do-it))
			      (#\n (values nil just-do-it))
			      (#\! (values t t)))))))

(defun query-loop (pane test prompt action message)
  (let* ((just-do-it nil)
	 (dir (pane-directory pane))
	 (entries (entries dir)))
    (loop with changes = 0
	  for i below (flexichain:nb-elements entries)
	  for entry = (flexichain:element* entries i)
	  when (and (funcall test entry)
		    (multiple-value-bind (answer jdi)
			(y-or-n-or-just-do-it-p (format nil "~A ~A"
							prompt
							(entry-name entry))
						just-do-it)
		      (setf just-do-it jdi)
		      answer))
	    do (funcall action entry)
	       (incf changes)
	  finally (funcall #'display-message message changes)
		  (when (> changes 0)
		    (setf (pane-needs-redisplay pane) t)))))

(defun marked-entries (pane &optional (flag *mark-character*))
  (loop with entries = (entries (pane-directory pane))
	for i below (flexichain:nb-elements entries)
	for entry = (flexichain:element* entries i)
	when (char= (entry-flag entry) flag)
	  collect entry))

(defun relevant-entries (pane count &optional (flag *mark-character*))
  (let* ((dir (pane-directory pane))
	 (entries (entries dir))
	 (last-offset (1- (flexichain:nb-elements entries)))
	 (cursor (pane-cursor-line pane)))
    (cond
      ((> count 1)
       (loop repeat count
	     for i from cursor to last-offset
	     collect (flexichain:element* entries i)))
      ((minusp count)
       (loop repeat (- count)
	     for i from cursor downto 0
	     collect (flexichain:element* entries i)))
      (t (or (marked-entries pane flag)
	     (list (flexichain:element* entries cursor)))))))

(defun short-directory-name (directory)
  (let ((last1 (last1 (pathname-directory directory))))
    (if (eql last1 :absolute)
	"/"
	last1)))

(defun trim-filename (name)
  (subseq
   name
   (1+ (max (or (search "//" name :from-end t :test #'string=) -1)
	    (or (search "/~" name :from-end t :test #'string=) -1)))))

(defun split-filename (name)
  (loop for start = 0 then (1+ end)
	for end = (position #\/ name :start start)
	collecting (subseq name start end)
	while end))

(defun normalize-path-list (list)
  (loop with finals = nil
	  for element in list
	  do (cond
	       ((string= element "."))
	       ((string= element "..")
		(pop finals))
	       (t (push element finals)))
	  finally (return (nreverse finals))))

(defun default-path-elements (element-list default-directory-name)
  (cond
    ((string= (car element-list) "")
     (rest element-list))
    ((string= (car element-list) "~")
     (append (rest (butlast (split-filename
			     (namestring (user-homedir-pathname)))))
	     (rest element-list)))
    (t
     (append (rest (butlast (split-filename default-directory-name)))
	     element-list))))

(defun expand-filename (name default-directory-name)
  (format nil "~{/~A~}"
	  (normalize-path-list
	   (default-path-elements
	       (split-filename (trim-filename name))
	       default-directory-name))))

(defun create-directory (name)
  (handler-bind
      ((file-error
	(lambda (e) (declare (ignore e))
		(beep)
		(display-message "Error creating directory ~A" name)
		(return-from create-directory nil))))
    (nth-value 1
	       (ensure-directories-exist
		(cl-fad:pathname-as-directory name)
		:verbose nil))))

(defun substitute-command-line (command-line name char)
  (let ((components (split-string command-line char)))
    (format nil "~{~A~}"
	    (loop for rest on components
		  collect (car rest)
		  when (cdr rest)
		    collect name))))

(defun shell-command-entries (pane entries command-line)
  (let ((dir (namestring (directory-pathname (pane-directory pane)))))
    (cond
      ((find #\* command-line)
       (let* ((names (format nil "~{~A~^ ~}"
			     (mapcar (lambda (entry)
				 (namestring (entry-pathname entry)))
			       entries))))
	 (ignore-errors
	   (run-shell-program
	    "sh"
	    (list "-c" (format nil "cd ~A; ~A"
			       dir
			       (substitute-command-line
				command-line
				names
				#\*)))))))
      ((find #\? command-line)
       (dolist (entry entries)
	 (ignore-errors
	   (run-shell-program
	    "sh"
	    (list "-c" (format nil "cd ~A; ~A"
			       dir
			       (substitute-command-line
				command-line
				(namestring (entry-pathname entry))
				#\?)))))))
      (t
       (dolist (entry entries)
	 (ignore-errors
	   (run-shell-program
	    "sh"
	    (list "-c" (format nil "cd ~A; ~A ~A"
			       dir
			       command-line
			       (namestring (entry-pathname entry)))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands and key bindings

(define-command-table global-ftd-table
    :inherit-from (global-esa-table keyboard-macro-table help-table))

(define-command (com-dired :name t :command-table global-ftd-table)
    ()
  (let* ((default-directory
	     (namestring (directory-pathname (pane-directory (current-pane)))))
	 (directory (expand-filename (accept 'string :prompt "Directory")
				     default-directory)))
    (cond
      ((wild-pathname-p directory)
       (multiple-value-bind (dir name)
	   (make-wild-list directory)
	 (if (null dir)
	     (display-message "No files match ~A" directory)
	     (add-tab-for-directory dir name))))
      (t
       (let ((directory (cl-fad:pathname-as-directory directory)))
	 (if (not (cl-fad:directory-exists-p directory))
	     (display-message "No such directory: ~A" (namestring directory))
	     (add-tab-for-directory (make-directory directory)
				    (short-directory-name directory))))))))

(set-key 'com-dired 'global-ftd-table '((#\x :control) (#\d)))

(define-command (com-system :name t :command-table global-ftd-table)
    ()
  (let* ((system (accept 'string :prompt "System"))
	 (dir (make-system-list system)))
    (if (null dir)
	(display-message "No files in system ~A" system)
	(add-tab-for-directory dir system))))

(set-key 'com-system 'global-ftd-table '((#\x :control) (#\s)))

(define-command (com-find-name :name t :command-table global-ftd-table)
    ()
  (let* ((default-directory
	     (namestring (directory-pathname (pane-directory (current-pane)))))
	 (directory (expand-filename (accept 'string :prompt "Directory")
				     default-directory))
	 (name (accept 'string :prompt "Name"))
	 (dir (make-find-name-list name directory)))
    (if (null dir)
	(display-message "No files found for find ~A -name ~A"
			 directory name)
	(add-tab-for-directory dir name))))

(set-key 'com-find-name 'global-ftd-table '((#\x :control) (#\n)))

(define-command (com-find-grep :name t :command-table global-ftd-table)
    ()
  (let* ((default-directory
	     (namestring (directory-pathname (pane-directory (current-pane)))))
	 (directory (expand-filename (accept 'string :prompt "Directory")
				     default-directory))
	 (regex (accept 'string :prompt "Regex"))
	 (dir (make-find-grep-list directory regex)))
    (if (null dir)
	(display-message "No files matching find ~A | grep ~A"
			 directory regex)
	(add-tab-for-directory dir regex))))

(set-key 'com-find-grep 'global-ftd-table '((#\x :control) (#\g)))

(define-command (com-find-dired :name t :command-table global-ftd-table)
    ()
  (let* ((default-directory
	     (namestring (directory-pathname (pane-directory (current-pane)))))
	 (directory (expand-filename (accept 'string :prompt "Directory")
				     default-directory))
	 (args (accept 'string :prompt "Find arguments"))
	 (dir (make-find-list directory args)))
    (if (null dir)
	(display-message "No files match find ~A" args)
	(add-tab-for-directory directory args))))

(set-key 'com-find-dired 'global-ftd-table '((#\x :control) (#\f)))

(define-command (com-new-tab :name t :command-table global-ftd-table)
    ((count 'integer))
  (let ((name (accept 'string :prompt "Tab name"))
	(entries (relevant-entries (current-pane) count)))
    (add-tab-for-directory
     (make-file-list name (mapcar #'entry-pathname entries))
     name)))

(set-key `(com-new-tab ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\x :control) (#\t)))

(define-command (com-add-files :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane/windows
	  (remove-if-not
	   (lambda (pane/window)
	     (typep (pane-directory (car pane/window)) 'ftd-file-list))
	   (pane/window *application-frame*)))
	 (names/panes (mapcar (lambda (pane/window)
			       (cons (list-name (pane-directory (car pane/window)))
				     (car pane/window)))
			     pane/windows))
	 (pane (accept `(member-alist
			 ,names/panes
			 :test #'string=)
		       :prompt "Tab"))
	 (dir (pane-directory pane))
	 (entries (relevant-entries (current-pane) count)))
    (unless (eql pane (current-pane))
      (setf (pathnames dir) (remove-duplicates
			     (append (pathnames dir) (mapcar #'entry-pathname entries))))
      (new-listing dir)
      (setf (pane-needs-redisplay pane) t))))

(set-key `(com-add-files ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\x :control) (#\a)))

(define-command (com-close-tab :name t :command-table global-ftd-table)
    ()
  (close-tab (current-pane)))

(set-key 'com-close-tab 'global-ftd-table '(#\q))

(define-command (com-next-line :name t :command-table global-ftd-table)
    ((count 'integer :prompt "Number of lines"))
  (next-line (current-pane) count))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\n))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\n :control)))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\Space))

(set-key `(com-next-line ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(:down))

(define-command (com-previous-line :name t :command-table global-ftd-table)
    ((count 'integer :prompt "Number of lines"))
  (next-line (current-pane) (- count)))

(set-key `(com-previous-line ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\p))

(set-key `(com-previous-line ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\p :control)))

(set-key `(com-previous-line ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(:up))

(define-command (com-page-down :name t :command-table global-ftd-table)
    ()
  (page-up/down (current-pane) t))

(set-key 'com-page-down 'global-ftd-table '((#\v :control)))

(define-command (com-page-up :name t :command-table global-ftd-table)
    ()
  (page-up/down (current-pane)))

(set-key 'com-page-up 'global-ftd-table '((#\v :meta)))

(define-command (com-last-line :name t :command-table global-ftd-table)
    ()
  (goto-line (current-pane)))

(set-key 'com-last-line 'global-ftd-table '((#\> :shift :meta)))

(define-command (com-first-line :name t :command-table global-ftd-table)
    ()
  (goto-line (current-pane) 0))

(set-key 'com-first-line 'global-ftd-table '((#\< :shift :meta)))

(define-command (com-goto-this-line :name nil :command-table global-ftd-table)
    ((pane 'pane) (y 'integer))
  (when (typep pane 'ftd-pane)
    (goto-line pane (floor y (stream-line-height pane)))))

(define-presentation-to-command-translator blank-area-to-goto-this-line
    (blank-area com-goto-this-line global-ftd-table)
    (window y)
  (list window y))

(define-command (com-delete-file :name t :command-table global-ftd-table)
    ((entries '(sequence ftd-entry) :prompt "File(s)"))
  (mark-files (current-pane) entries *flag-character*))

(define-command (com-keyboard-delete-file :name nil :command-table global-ftd-table)
    ((count 'integer))
  (mark-file (current-pane) *flag-character* count))

(set-key `(com-keyboard-delete-file ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\d))

(define-command (com-undelete-file :name t :command-table global-ftd-table)
    ((entries '(sequence ftd-entry) :prompt "File(s)"))
  (mark-files (current-pane) entries #\Space))

(define-command (com-keyboard-undelete-file :name nil :command-table global-ftd-table)
    ((count 'integer))
  (mark-file (current-pane) #\Space count))

(set-key `(com-keyboard-undelete-file ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\u))

(set-key `(com-keyboard-undelete-file ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\*) (#\u)))

(define-command (com-backward-undelete-file :name t :command-table global-ftd-table)
    ((count 'integer :prompt "Number of lines"))
  (mark-file (current-pane) #\Space (- count)))

(set-key `(com-backward-undelete-file ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\Backspace))

(set-key `(com-backward-undelete-file ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\*) (#\Backspace)))

(define-command (com-flag-backup-files :name t :command-table global-ftd-table)
    ()
  (do-entries (current-pane)
    (lambda (entry)
      (let* ((entry-name (entry-name entry))
	     (last-char (char entry-name (1- (length entry-name)))))
	(char= last-char #\~)))
    (lambda (entry)
      (setf (entry-flag entry) *flag-character*))
    "Flagged ~D file~:P"))

(set-key 'com-flag-backup-files 'global-ftd-table '(#\~))

(define-command (com-flag-auto-save-files :name t :command-table global-ftd-table)
    ()
  (do-entries (current-pane)
    (lambda (entry)
      (let* ((entry-name (entry-name entry))
	     (first-char (char entry-name 0))
	     (last-char (char entry-name (1- (length entry-name)))))
	(and (char= first-char #\#) (char= last-char #\#))))
    (lambda (entry)
      (setf (entry-flag entry) *flag-character*))
    "Flagged ~D file~:P"))

(set-key 'com-flag-auto-save-files 'global-ftd-table '(#\#))

(define-command (com-regexp-flag-files :name t :command-table global-ftd-table)
    ()
  (let* ((regexp (accept 'string :prompt "Regexp"))
	 (scanner (cl-ppcre:create-scanner regexp)))
    (do-entries (current-pane)
      (lambda (entry)
	(cl-ppcre:scan scanner (entry-name entry)))
      (lambda (entry)
	(setf (entry-flag entry) *flag-character*))
      "Flagged ~D file~:P")))

(set-key 'com-regexp-flag-files 'global-ftd-table '((#\%) (#\d)))

(define-command (com-flag-excess-backups :name t :command-table global-ftd-table)
    ()
  (mark-excess-backups (current-pane) *flag-character*))

(set-key 'com-flag-excess-backups 'global-ftd-table '(#\.))

(define-command (com-expunge :name t :command-table global-ftd-table)
    ()
  (let* ((pane (current-pane))
	 (entries (marked-entries pane *flag-character*)))
    (when entries (delete-entries pane entries))))

(set-key 'com-expunge 'global-ftd-table '(#\x))

(define-command (com-mark-file :name t :command-table global-ftd-table)
    ((entries '(sequence ftd-entry) :prompt "File(s)"))
  (mark-files (current-pane) entries *mark-character*))

(define-command (com-keyboard-mark-file :name nil :command-table global-ftd-table)
    ((count 'integer :prompt "Number of lines"))
  (mark-file (current-pane) *mark-character* count))

(set-key `(com-keyboard-mark-file ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\m))

(set-key `(com-keyboard-mark-file ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\*) (#\m)))

(define-command (com-mark-all-files :name t :command-table global-ftd-table)
    ()
  (do-entries (current-pane)
    (constantly t)
    (lambda (entry)
      (setf (entry-flag entry) *mark-character*))
    "Marked ~D file~:P"))

(set-key 'com-mark-all-files 'global-ftd-table '((#\*) (#\s)))

(define-command (com-regexp-mark-files :name t :command-table global-ftd-table)
    ()
  (let* ((regexp (accept 'string :prompt "Regexp")) 
	 (scanner (cl-ppcre:create-scanner regexp)))
    (do-entries (current-pane)
      (lambda (entry)
	(cl-ppcre:scan scanner (entry-name entry)))
      (lambda (entry)
	(setf (entry-flag entry) *mark-character*))
      "Marked ~D file~:P")))

(set-key 'com-regexp-mark-files 'global-ftd-table '((#\%) (#\m)))

(set-key 'com-regexp-mark-files 'global-ftd-table '((#\*) (#\%)))

(define-command (com-eval-mark-files :name t :command-table global-ftd-table)
    ()
  (let ((expression (accept 'string :prompt "Expression")))
    (mark-eval (current-pane) expression)))

(set-key 'com-eval-mark-files 'global-ftd-table '((#\*) (#\()))

(define-command (com-grep-mark-files :name t :command-table global-ftd-table)
    ()
  (let* ((regexp (accept 'string :prompt "Regexp"))
	 (pane (current-pane))
	 (dir (pane-directory pane))
	 (matches (get-grep-matches dir regexp)))
    (do-entries pane
      (lambda (entry)
	(find (namestring (entry-pathname entry)) matches :test #'string=))
      (lambda (entry)
	(setf (entry-flag entry) *mark-character*))
      "Marked ~D file~:P")))

(set-key 'com-grep-mark-files 'global-ftd-table '((#\%) (#\g)))

(define-command (com-mark-extension :name t :command-table global-ftd-table)
    ()
  (let* ((extension (accept 'string :prompt "Extension"))
	 (match (format nil ".~A" extension))
	 (match-length (length match)))
    (do-entries (current-pane)
      (lambda (entry)
	(let* ((name (entry-name entry))
	       (length (length name)))
	  (and (> length match-length)
	       (string= name match :start1 (- length match-length)))))
      (lambda (entry)
	(setf (entry-flag entry) *mark-character*))
      "Marked ~D file~:P")))

(set-key 'com-mark-extension 'global-ftd-table '((#\*) (#\.)))

(define-command (com-mark-executables :name t :command-table global-ftd-table)
    ()
  (do-entries (current-pane)
    #'entry-executable-p
    (lambda (entry)
      (setf (entry-flag entry) *mark-character*))
    "Marked ~D file~:P"))

(set-key 'com-mark-executables 'global-ftd-table '((#\*) (#\*)))

(define-command (com-mark-symlinks :name t :command-table global-ftd-table)
    ()
  (do-entries (current-pane)
    #'entry-symlink-p
    (lambda (entry)
      (setf (entry-flag entry) *mark-character*))
    "Marked ~D link~:P"))

(set-key 'com-mark-symlinks 'global-ftd-table '((#\*) (#\@)))

(define-command (com-mark-directories :name t :command-table global-ftd-table)
    ((count 'integer))
  (do-entries (current-pane)
    (lambda (entry)
      (and (entry-directory-p entry)
	   (not (or (string= (entry-name entry) ".")
		    (string= (entry-name entry) "..")))))
    (lambda (entry)
      (setf (entry-flag entry)
	    (if (plusp count)
		*mark-character*
		#\Space)))))

(set-key `(com-mark-directories ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\*) (#\/)))

(define-command (com-next-marked-file :name t :command-table global-ftd-table)
    ()
  (next/prev-marked-file (current-pane)))

(set-key 'com-next-marked-file 'global-ftd-table '((#\*) (#\n :control)))

(set-key 'com-next-marked-file 'global-ftd-table '((#\} :meta :shift)))

(define-command (com-previous-marked-file :name t :command-table global-ftd-table)
    ()
  (next/prev-marked-file (current-pane) t))

(set-key 'com-previous-marked-file 'global-ftd-table '((#\*) (#\p :control)))

(set-key 'com-previous-marked-file 'global-ftd-table '((#\{ :meta :shift)))

(define-command (com-toggle-marks :name t :command-table global-ftd-table)
    ()
  (rotate-marks (current-pane) (list *mark-character* #\Space)))

(set-key 'com-toggle-marks 'global-ftd-table '((#\*) (#\t)))

(define-command (com-unmark-all-marks :name t :command-table global-ftd-table)
    ()
  (remove-all-marks (current-pane)))

(set-key 'com-unmark-all-marks 'global-ftd-table '((#\*) (#\!)))

(set-key 'com-unmark-all-marks 'global-ftd-table '(#\U))

(define-command (com-unmark-all-files :name t :command-table global-ftd-table)
    ((query 'boolean))
  (let ((mark (read-character "Remove: ")))
    (if (graphic-char-p mark)
	(if query
	    (query-loop (current-pane)
			(lambda (entry) (char= (entry-flag entry) mark))
			"Unmark"
			(lambda (entry) (setf (entry-flag entry) #\Space))
			"Unmarked ~D entr~@:P")
	    (replace-marks (current-pane) mark #\Space))
	(display-message "Not a character"))))

(set-key `(com-unmark-all-files ,*numeric-argument-p*)
	 'global-ftd-table
	 '((#\*) (#\?)))

(define-command (com-change-marks :name t :command-table global-ftd-table)
    ()
  (let* ((old-mark (read-character "Replace: "))
	 (new-mark (read-character (format nil "Replace ~C with: " old-mark))))
    (if (and (graphic-char-p old-mark) (graphic-char-p new-mark))
	(replace-marks (current-pane) old-mark new-mark)
	(display-message "Not a valid mark"))))

(set-key 'com-change-marks 'global-ftd-table '((#\*) (#\c)))

(define-command (com-chmod :name t :command-table global-ftd-table)
    ((count 'integer))
  (let ((mode (accept 'string :prompt "Mode"))
	(entries (relevant-entries (current-pane) count)))
      (chmod-entries entries mode)
      (update-entries entries)
      (setf (pane-needs-redisplay (current-pane)) t)))

(set-key `(com-chmod ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\M))

(define-command (com-chgrp :name t :command-table global-ftd-table)
    ((count 'integer))
  (let ((group (accept 'string :prompt "Group"))
	(entries (relevant-entries (current-pane) count)))
      (chgrp-entries entries group)
      (update-entries entries)
      (setf (pane-needs-redisplay (current-pane)) t)))

(set-key `(com-chgrp ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\G))

(define-command (com-chown :name t :command-table global-ftd-table)
    ((count 'integer))
  (let ((owner (accept 'string :prompt "Owner"))
	(entries (relevant-entries (current-pane) count)))
    (chown-entries entries owner)
    (update-entries entries)
    (setf (pane-needs-redisplay (current-pane)) t)))

(set-key `(com-chown ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\O))

(define-command (com-touch :name t :command-table global-ftd-table)
    ((count 'integer))
  (let ((time (accept 'string :prompt "Time"))
	(entries (relevant-entries (current-pane) count)))
    (touch-time entries time)
    (update-entries entries)
    (setf (pane-needs-redisplay (current-pane)) t)))

(set-key `(com-touch ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\T))

(define-command (com-create-directory :name t :command-table global-ftd-table)
    ()
  (let* ((pane (current-pane))
	 (default-directory (namestring (directory-pathname
					 (pane-directory pane))))
	 (name (expand-filename (accept 'string :prompt "Create directory")
				default-directory)))
    (cond ((create-directory name)
	   (when (string= name default-directory :end1 (length default-directory))
	     (add-entries pane (list name))
	     (setf (pane-needs-redisplay pane) t)))
	  (t (display-message "~A already exists")))))

(set-key 'com-create-directory 'global-ftd-table '(#\+))

(define-command (com-visit-file :name t :command-table global-ftd-table)
    ()
  (let* ((pane (current-pane))
	 (cursor (pane-cursor-line pane))
	 (directory (pane-directory pane))
	 (entries (entries directory))
	 (current-entry (flexichain:element* entries cursor))
	 (current-pathname (entry-pathname current-entry)))
    (cond
      ((cl-fad:directory-pathname-p current-pathname)
       (unless (cl-fad:directory-exists-p current-pathname)
	 (beep)
	 (display-message "No such directory: ~A" (namestring current-pathname))
	 (return-from com-visit-file))
       (flet ((directory-match (pane/window)
		(equal (directory-pathname (pane-directory (car pane/window)))
		       current-pathname)))
	 (let ((existing-tab
		(find-if #'directory-match (pane/window *application-frame*))))
	   (if existing-tab
	       (switch-to-pane (cdr existing-tab)
			       'tab-layout-pane)
	       (add-tab-for-directory
		(make-instance 'ftd-directory :pathname current-pathname)
		(let ((last1 (last1 (pathname-directory current-pathname))))
		  (if (eql last1 :absolute) "/" last1)))))))
      (t
       (ed current-pathname)))))

(set-key 'com-visit-file 'global-ftd-table '(#\f))

(define-command (com-delete :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count)))
    (when entries (delete-entries pane entries))))

(set-key `(com-delete ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\D))

(define-command (com-copy :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count))
	 (default-directory (namestring (directory-pathname
					 (pane-directory pane))))
	 (destination (expand-filename
		       (accept 'string
			       :prompt "Destination")
		       default-directory)))
    (copy-entries pane entries destination)))

(set-key `(com-copy ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\C))

(define-command (com-rename :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count))
	 (default-directory (namestring (directory-pathname
					 (pane-directory pane))))
	 (destination (expand-filename
		       (accept 'string
			       :prompt "Destination")
		       default-directory)))
    (rename-entries pane entries destination)))

(set-key `(com-rename ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\R))

(define-command (com-hardlink :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count))
	 (default-directory (namestring (directory-pathname
					 (pane-directory pane))))
	 (destination (expand-filename
		       (accept 'string
			       :prompt "Destination")
		       default-directory)))
    (link-entries pane entries destination)))

(set-key `(com-hardlink ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\H))

(define-command (com-symlink :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count))
	 (default-directory (namestring (directory-pathname
					 (pane-directory pane))))
	 (destination (expand-filename
		       (accept 'string
			       :prompt "Destination")
		       default-directory)))
    (link-entries pane entries destination t)))

(set-key `(com-symlink ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\S))

(define-command (com-upcase :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count)))
    (change-case-entries pane entries #'string-upcase)))

(set-key `(com-upcase ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\%) (#\u)))

(define-command (com-downcase :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count)))
    (change-case-entries pane entries #'string-downcase)))

(set-key `(com-downcase ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\%) (#\l)))

(define-command (com-capitalize :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count)))
    (change-case-entries pane entries #'string-capitalize)))

(set-key `(com-capitalize ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\%) (#\k)))

(define-command (com-regexp-rename :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((from (accept 'string :prompt "Regexp rename from"))
	 (to (accept 'string :prompt (format nil "Rename from ~A to" from)))
	 (pane (current-pane))
	 (entries (relevant-entries pane count)))
    (regexp-rename-entries pane entries from to)))

(set-key `(com-regexp-rename ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\%) (#\R)))

(define-command (com-regexp-hardlink :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((from (accept 'string :prompt "Regexp hardlink from"))
	 (to (accept 'string :prompt (format nil "Hardlink from ~A to" from)))
	 (pane (current-pane))
	 (entries (relevant-entries pane count)))
    (regexp-link-entries pane entries from to)))

(set-key `(com-regexp-hardlink ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\%) (#\H)))

(define-command (com-regexp-symlink :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((from (accept 'string :prompt "Regexp symlink from"))
	 (to (accept 'string :prompt (format nil "Symlink from ~A to" from)))
	 (pane (current-pane))
	 (entries (relevant-entries pane count)))
    (regexp-link-entries pane entries from to t)))

(set-key `(com-regexp-symlink ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\%) (#\S)))

(define-command (com-regexp-copy :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((from (accept 'string :prompt "Regexp copy from"))
	 (to (accept 'string :prompt (format nil "Regexp copy from ~A to" from)))
	 (pane (current-pane))
	 (entries (relevant-entries pane count)))
    (regexp-link-entries pane entries from to t)))

(set-key `(com-regexp-copy ,*numeric-argument-marker*)
	 'global-ftd-table
	 '((#\%) (#\C)))

(define-command (com-redisplay :name t :command-table global-ftd-table)
    ((count 'integer))
  (let ((entries (relevant-entries (current-pane) count)))
    (update-entries entries)
    (setf (pane-needs-redisplay (current-pane)) t)))

(set-key `(com-redisplay ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\l))

(define-command (com-revert-tab :name t :command-table global-ftd-table)
    ()
  (let* ((pane (current-pane))
	 (dir (pane-directory pane))
	 (old-entries (all-entries dir))
	 (new-entries (new-listing dir)))
    (loop for new-entry in new-entries
	  for old-entry = (find (entry-pathname new-entry) old-entries
				:key #'entry-pathname
				:test #'equal)
	  when old-entry
	    do (setf (entry-flag new-entry) (entry-flag old-entry))
	  finally (setf (entries dir)
			(make-instance 'flexichain:standard-flexichain
			   :initial-contents new-entries))
		  (when (>= (pane-cursor-line pane) (length new-entries))
		    (setf (pane-cursor-line pane) (1- (length new-entries))))
		  (setf (pane-needs-redisplay pane) t))))

(set-key 'com-revert-tab 'global-ftd-table '(#\g))

(define-command (com-shell-command :name t :command-table global-ftd-table)
    ((count 'integer))
  (let* ((pane (current-pane))
	 (entries (relevant-entries pane count))
	 (command-line (accept 'string :prompt "Command line")))
    (shell-command-entries pane entries command-line)))

(set-key `(com-shell-command ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\!))

(set-key `(com-shell-command ,*numeric-argument-marker*)
	 'global-ftd-table
	 '(#\X))

(define-command (com-set-variable :name t :command-table global-ftd-table)
    ()
  (let* ((var (accept
	       `(member-alist ,*ftd-user-variables*)
	       :prompt "Variable"))
	 (p-type (cdr var))
	 (val (handler-bind
		  ((input-not-of-required-type
		    (lambda (e) (declare (ignore e))
			    (beep)
			    (display-message "We were looking for a ~A" p-type)
			    (return-from com-set-variable nil))))
		(accept (cdr var) :prompt "Value"))))
    (setf (symbol-value (car var)) val)))


;;;;;;;;; TESTING

;; (define-command (com-test :name t :command-table global-ftd-table)
;;     ()
;;   (let* ((var (accept
;; 	       `(member-alist ,*ftd-user-variables*)
;; 	       :prompt "Variable"))
;; 	 (val (accept (cdr var))))
;;     (setf (symbol-value (car var)) val)))

