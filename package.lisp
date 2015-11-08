(cl:defpackage :ftd
  (:use :clim :clim-lisp :esa :clim-tab-layout)
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
  (:import-from #:nix
		#:s-irwxu 
		#:s-irusr
		#:s-iwusr
		#:s-ixusr
		#:s-irwxg
		#:s-irgrp
		#:s-iwgrp
		#:s-ixgrp
		#:s-irwxo
		#:s-iroth
		#:s-iwoth
		#:s-ixoth
		#:s-isuid
		#:s-isgid
		#:s-isvtx
		#:s-ifmt
		#:s-ififo
		#:s-ifchr
		#:s-ifdir
		#:s-ifblk
		#:s-ifreg
		#:s-iflnk
		#:stat
		#:lstat
		#:dev ;; #:device
		#:ino ;; #:inode
		#:mode
		#:nlink ;; #:link-count
		#:uid
		#:gid
		;; I can't find this one... related to `nix::stat-rdev' possibly? 
		;; #:special-device-type 
		#:atime
		#:mtime
		#:ctime
		#:size
		#:blocks  ;; #:block-count
		#:blksize ;; #:block-size
		;; XXX, can't find this one
		;; #:make-stat-from-pointer
		#:stat-mode
		#:stat-dev
		#:stat-ino
		#:stat-mode
		#:stat-nlink
		#:stat-uid
		#:stat-gid
		;; #:stat-special-device-type
		#:stat-atime
		#:stat-mtime
		#:stat-ctime
		#:stat-size
		#:stat-blocks
		#:stat-blksize
		;; #:password-entry
		;; #:username
		;; `osicat::password' #:password
		#:uid
		#:gid
		;; #:full-name
		;; #:home-directory
		;; nix::sc-shell ?
		#:shell
		;; #:make-password-entry-from-pointer
		#:getpwnam
		;; #:password-entry-password
		;; #:password-entry-uid
		;; #:password-entry-gid
		;; #:password-entry-full-name
		;; #:password-entry-home-directory
		;; #:password-entry-shell
		;; #:group-entry
		;; #:name
		;; #:password
		;; #:make-group-entry-from-pointer
		#:getgrnam
		;; #:group-entry-password
		;; #:group-entry-gid
		;; #:directory-entry
		;; #:make-directory-entry-from-pointer
		;; #:directory-entry-name
		))
