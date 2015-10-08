;; package definitions for FTD

;;; See licence and disclaimer in application.lisp

(cl:defpackage :ftd
  (:use :clim :clim-lisp :esa :tab-layout)
  (:export #:ftd))

(cl:defpackage #:ftd-directory
  (:use #:cl #:cffi)
  (:export #:directory-names
	   #:stat-la
	   #:gid->name
	   #:uid->name
	   #:unix->universal
	   #:universal->unix
	   #:mode->string
	   #:executablep
	   #:ls-time-string)
  (:import-from #:cffi-unix-internal
		#:stat-irwxu
		#:stat-irusr
		#:stat-iwusr
		#:stat-ixusr
		#:stat-irwxg
		#:stat-irgrp
		#:stat-iwgrp
		#:stat-ixgrp
		#:stat-irwxo
		#:stat-iroth
		#:stat-iwoth
		#:stat-ixoth
		#:stat-isuid
		#:stat-isgid
		#:stat-isvtx
		#:stat-ifmt
		#:stat-ififo
		#:stat-ifchr
		#:stat-ifdir
		#:stat-ifblk
		#:stat-ifreg
		#:stat-iflnk
		;; #:stat-ifsock
		;; #:stat-ifwht
		#:stat
		#:device
		#:inode
		#:mode
		#:link-count
		#:uid
		#:gid
		#:special-device-type
		#:atime
		#:mtime
		#:ctime
		#:size
		#:block-count
		#:block-size
		#:make-stat-from-pointer
		#:stat-mode
		#:stat-device
		#:stat-inode
		#:stat-mode
		#:stat-link-count
		#:stat-uid
		#:stat-gid
		#:stat-special-device-type
		#:stat-atime
		#:stat-mtime
		#:stat-ctime
		#:stat-size
		#:stat-block-count
		#:stat-block-size
		#:password-entry
		#:username
		#:password
		#:uid
		#:gid
		#:full-name
		#:home-directory
		#:shell
		#:make-password-entry-from-pointer
		#:password-entry-username
		#:password-entry-password
		#:password-entry-uid
		#:password-entry-gid
		#:password-entry-full-name
		#:password-entry-home-directory
		#:password-entry-shell
		#:group-entry
		#:name
		#:password
		#:gid
		#:make-group-entry-from-pointer
		#:group-entry-name
		#:group-entry-password
		#:group-entry-gid
		#:directory-entry
;; 		#:inode
;; 		#:file-type
;; 		#:name
		#:make-directory-entry-from-pointer
;; 		#:directory-entry-inode
;; 		#:directory-entry-file-type
		#:directory-entry-name))
