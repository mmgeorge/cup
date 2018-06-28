(defpackage :cup/core/main
  (:use :cl )
  (:import-from :unix-opts)
  (:import-from :cl-ppcre)
  ;(:import-from :cup/core/traits/persist #:save-instance)
  (:import-from :cup/core/shadow-system #:make-shadow-system #:save-instance #:register)
  (:import-from :cup/core/shadow-package #:make-shadow-package)
  (:import-from :cup/core/parser #:parse-source)
  (:export #:main))

(in-package :cup/core/main)

(opts:define-opts
  (:name :version :description "Get the current version" :long "version" :short #\v)
  ;(:name :version :description "Get the current version" :long "version" :short #\v :arg-parser #'identity)
  )

(defun main (argv)
  (multiple-value-bind (options free-vars)
      (opts:get-opts argv)
    (cond ((getf options :version) (format t "Version: ~a~%" (asdf:component-version (asdf:find-system 'cup))))
          ((not (null (cdr free-vars))) (handle-command (cadr free-vars) (cddr free-vars) options))
          (t (prompt-make-system)))))

(defun handle-command (command free-vars options)
  (cond ((string-equal command "add")(handle-package-add free-vars options))))

(defun handle-package-add (free-vars options)
  (declare (ignore options))
  (let ((package-name (if (null free-vars)
                          (prompt "PLEASE SPECIFY A PACKAGE NAME: ")
                          (car free-vars))))

    (format t "Adding package ~a" package-name)
    (register (make-shadow-package package-name) (bound-system))))

(defvar *bound-system* nil)

(defun find-system ()
  (let ((path "test.asd"))
    (with-open-file (infile path)
      (parse-source infile))))

(defun bound-system ()
  (or *bound-system* (setf *bound-system* (find-system))))


(defun prompt (description)
  (format *query-io* "~a " description)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-make-system ()
  (let ((name (prompt "System name? [default=unset]"))
        (author (prompt "author? [default=unset]"))
        (maintainer (prompt "maintainer? [default=unset]"))
        (license (prompt "license? [default=unset]"))
        (description (prompt "description? [default=unset]")))
    (save-instance (make-shadow-system name author maintainer license description)
                   (directory-namestring (merge-pathnames *default-pathname-defaults*)))))
