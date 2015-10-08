;;; system definition for ftd. -*- lisp -*-

;;; See licence and disclaimer in application.lisp

(cl:defpackage :ftd.system
  (:use :cl :asdf))

(cl:in-package :ftd.system)

(defsystem :ftd
    :description "Flexi-Trivial Directory editor"
    :version "0.0.3"
    :author "John Splittist <splittist at yahoo.com>"
    :depends-on (:mcclim :tab-layout :esa :cl-fad :cl-ppcre :cffi-unix :flexichain)
    :components ((:file "package")
		 (:file "variables" :depends-on ("package"))
		 (:file "user-variables" :depends-on ("package" "variables"))
		 (:file "application" :depends-on ("package" "variables" "user-variables"))
		 (:file "ftd-directory" :depends-on ("package"))))
