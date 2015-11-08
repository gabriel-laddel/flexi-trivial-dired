;;; system definition for ftd. -*- lisp -*-

;;; See licence and disclaimer in application.lisp

(cl:defpackage :ftd.system
  (:use :cl :asdf))

(cl:in-package :ftd.system)

(defsystem :ftd
  :description "Flexi-Trivial Directory editor"
  :version "0.0.3"
  :author "John Splittist <splittist at yahoo.com>"
  :depends-on (:mcclim :clim-examples :esa :cl-fad :cl-ppcre :osicat :flexichain)
  :components ((:file "package")
	       (:file "variables" :depends-on ("package"))
	       (:file "application" :depends-on ("package" "variables"))
	       (:file "ftd-directory" :depends-on ("package"))
               (:file "monkey-patches" :depends-on ("ftd-directory"))))
