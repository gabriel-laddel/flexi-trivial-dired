;; user variables for FTD

;;; See licence and disclaimer in application.lisp

(in-package #:ftd)

(define-variable *flag-character* #\D
  "Character used to flag entries for deletion."
  character)

(define-variable *mark-character* #\*
  "Characer used to mark entries."
  character)

(define-variable *kept-new-versions* 2
  "Number of most recent numbered backups to keep."
  integer)

(define-variable *kept-old-versions* 2
  "Number of oldest numbered backups to keep."
  integer)

(define-variable *chown-program* "chown"
  "Program to use as 'chown'"
  string)

(define-variable *chgrp-program* "chgrp"
  "Program to use as 'chgrp'"
  string)

(define-variable *chmod-program* "chmod"
  "Program to use as 'chmod'"
  string)

(define-variable *touch-program* "touch"
  "Program to use as 'touch'"
  string)

(define-variable *cursor-ink* +red+
  "Colour of the cursor"
  named-color)

(define-variable *flag-ink* +dark-green+
  "Colour of the flags"
  named-color)

(define-variable *deleted-files-ink* +dark-red+
  "Colour of filenames for files flagged for deletion"
  named-color)

(define-variable *marked-files-ink* +dark-blue+
  "Colour of filenames for marked files"
  named-color)
