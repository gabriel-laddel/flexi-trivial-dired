;; part of FTD

;;; See licence and disclaimer in application.lisp

(in-package #:ftd-directory)

(defun gid->name (gid)
  (group-entry-name (cffi-unix::getgrgid gid)))

(defun uid->name (uid)
  (password-entry-username (cffi-unix::getpwuid uid)))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defconstant +6months+ (* 180 24 60 60)
  "six months in seconds")

;; shut sbcl up
(defparameter +month-names+ '("January"
			      "February"
			      "March"
			      "April"
			      "May"
			      "June"
			      "July"
			      "August"
       			      "September"
			      "October"
			      "November"
			      "December"))

(defun short-month (number)
  (subseq (nth (1- number) +month-names+) 0 3))

(defun unix->universal (time)
  (+ time +unix-epoch+))

(defun universal->unix (time)
  (- time +unix-epoch+))

(defun ls-time-string (time now)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time)
    (declare (ignore second))
    (if (> (- now time) +6months+)
	(format nil "~A ~2D  ~4D" (short-month month) day year)
	(format nil "~A ~2D ~2,'0D:~2,'0D" (short-month month) day hour minute))))

(defun mode->string (mode)
  (let* ((type
	  (let ((fmt (logand stat-ifmt mode)))
	    (cond
	      ((= fmt stat-ifblk) #\b)
	      ((= fmt stat-ifchr) #\c)
	      ((= fmt stat-ififo) #\p)
	      ((= fmt stat-ifreg) #\-)
	      ((= fmt stat-ifdir) #\d)
	      ((= fmt stat-iflnk) #\l)
	      (t #\?))))
	 (owner-read (if (plusp (logand stat-irusr mode))
			 #\r #\-))
	 (owner-write (if (plusp (logand stat-iwusr mode))
			  #\w #\-))
	 (owner-execute (if (plusp (logand stat-ixusr mode))
			    (if (plusp (logand stat-isuid mode))
				#\s #\x)
			    (if (plusp (logand stat-isuid mode))
				#\S #\-)))
	 (group-read (if (plusp (logand stat-irgrp mode))
			 #\r #\-))
	 (group-write (if (plusp (logand stat-iwgrp mode))
			  #\w #\-))
	 (group-execute (if (plusp (logand stat-ixgrp mode))
			    (if (plusp (logand stat-isgid mode))
				#\s #\x)
			    (if (plusp (logand stat-isgid mode))
				#\S #\-)))
	 (other-read (if (plusp (logand stat-iroth mode))
			 #\r #\-))
	 (other-write (if (plusp (logand stat-iwoth mode))
			  #\w #\-))
	 (other-execute (let ((dirp (char= type #\d))
			      (execute/search-p
			       (plusp (logand stat-ixoth mode)))
			      (restricted-deletion-p
			       (plusp (logand stat-isvtx mode))))
			  (cond
			    ((and dirp
				  (not execute/search-p)
				  restricted-deletion-p) #\T)
			    ((and dirp
				  execute/search-p
				  restricted-deletion-p) #\t)
			    (execute/search-p #\x)
			    (t #\-)))))
    (coerce (list type
		  owner-read owner-write owner-execute
		  group-read group-write group-execute
		  other-read other-write other-execute
		  #\Space) ; optional alternate access method flag
	    'string)))

(defun executablep (mode)
  (logand (logior stat-ixusr stat-ixgrp stat-ixoth) mode))

(defun directory-names (dirname)
  (let (DIR names)
    (unwind-protect
	(progn
	  (setf DIR (cffi-unix::opendir dirname)
		names (loop for dirent = (ignore-errors (cffi-unix::readdir DIR))
			    while dirent
			    collect (directory-entry-name dirent))))
      (progn
	(when DIR (cffi-unix::closedir DIR))
	names))))

(cffi-unix::defforeign ("readlink" %readlink) :int
  "Put the contents of symbolic link PATH into BUF of size BUFSIZ"
  (path :string)
  (buf :pointer)
  (bufsiz :int))

(defconstant PATH-MAX 255)

(defun readlink (path)
  (with-foreign-pointer (buf PATH-MAX)	; we don't need no stinkin' nulls
    (let ((length (%readlink path buf PATH-MAX)))
      (foreign-string-to-lisp buf length nil))))

(defun stat-la (path)
  (let* ((stat (stat path nil))
	 (fmt (logand stat-ifmt (stat-mode stat)))
	 (type (cond
		 ((= fmt stat-ifreg) :file)
		 ((= fmt stat-ifdir) :directory)
		 ((= fmt stat-iflnk) :link)
		 (t :other)))
	 (link-name (when (eq type :link)
		      (readlink path))))
    (with-slots (device inode mode link-count uid gid
			special-device-type
			atime mtime ctime
			size block-count block-size) stat
       (values type
	       device inode mode link-count uid gid
	       special-device-type
	       atime mtime ctime
	       size block-count block-size
	       link-name))))

(cffi-unix::defforeign "chmod" :int
  "Set file permission bits of PATH to MODE"
  (path :string)
  (mode :mode))

(defun posix-date->time (date-string)
  "[[CC]YY]mmddhhmm[.ss] to universal-time
If CCYY not specified, use current year.
If YY specified, CC = 19 for YY >= 68"
  (let* (year month date hour minute second
	      (total-length (length date-string))
	      (dot (position #\. date-string))
	      (mmddhhmm-start (- (or dot total-length) 8))
	      (mmddhhmm (subseq date-string mmddhhmm-start dot))
	      (ss (when dot (subseq date-string (1+ dot))))
	      (CCYY (subseq date-string 0 mmddhhmm-start))
	      (CCYY-length (length CCYY)))
    (when (and ss (/= 2 (length ss)))
      (error "Bad date format - second digits: ~A" date-string))
    (setq second (if ss (parse-integer ss) 0)
	  month (parse-integer (subseq mmddhhmm 0 2))
	  date (parse-integer (subseq mmddhhmm 2 4))
	  hour (parse-integer (subseq mmddhhmm 4 6))
	  minute (parse-integer (subseq mmddhhmm 6 8)))
    (cond ((= 4 CCYY-length)
	   (setq year (parse-integer CCYY)))
	  ((= 2 CCYY-length)
	   (let ((YY (parse-integer CCYY)))
	     (setq year (if (>= YY 68)
			    (+ YY 1900)
			    (+ YY 2000)))))
	  ((= 0 CCYY-length)
	   (multiple-value-bind (s m h d mo this-year)
	       (get-decoded-time)
	     (declare (ignore s m h d mo))
	     (setq year this-year)))
	  (t (error "Bad date format: ~A" date-string)))
    (encode-universal-time second minute hour date month year)))

(defun print-entry (stat filename
		    &key
		    (link-count-width 4)
		    (size-width 8)
		    (now (get-universal-time))
		    )
  (with-slots (mode link-count uid gid size
			 mtime) stat
     (format t "~&~A ~vD ~8A ~8A ~vD ~A ~A"
	     (mode->string mode)
	     link-count-width
	     link-count
	     (uid->name uid)
	     (gid->name gid)
	     size-width
	     size
	     (ls-time-string (unix->universal mtime) now)
	     filename)))

(defun integer-string-length (num)
  (length (princ-to-string num)))

(defun test (dirname)
  (loop for name in (directory-names dirname)
	for stat = (stat (concatenate 'string dirname name) nil)
	collecting (cons name stat) into entries
	maximizing (stat-link-count stat) into max-link-count
	maximizing (stat-size stat) into max-size
	finally (loop for (name . stat) in entries
		      with link-count-width = (max 4 (integer-string-length max-link-count))
		      with size-width = (integer-string-length max-size)
		      with now = (get-universal-time)
		      with link-name = nil
		      when (= stat-iflnk (logand stat-ifmt (stat-mode stat)))
			do (setq link-name (readlink (concatenate 'string dirname name)))
		      else
			do (setq link-name nil)
		      do (print-entry stat name :link-count-width link-count-width
			       :size-width size-width :now now)
		      when link-name
			do (format t " -> ~A" link-name))))

