(defpackage :cup/core/main
  (:use :cl )
  (:import-from :unix-opts)
  (:import-from :cl-ppcre)
  ;(:import-from :cup/core/traits/persist #:save-instance)q
  (:import-from :cup/core/system #:make-system #:save-instance)
  (:export #:main))

(in-package :cup/core/main)

(opts:define-opts
  (:name :version :description "Get the current version" :long "version" :short #\v)
  ;(:name :version :description "Get the current version" :long "version" :short #\v :arg-parser #'identity)
  )

(defun main (argv)
  (multiple-value-bind (options)
      (opts:get-opts argv)
    (cond ((getf options :version) (format t "Version: ~a~%" (asdf:component-version (asdf:find-system 'cup))))
           ;(getf options :version) (format t "Found an arg!: ~a" (getf options :version))
          (t (prompt-make-system (car argv))))))

(defun prompt (description)
  (format *query-io* "~a " description)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-make-system (path)
  (let ((name (prompt "System name? [default=unset]"))
        (author (prompt "author? [default=unset]"))
        (maintainer (prompt "maintainer? [default=unset]"))
        (license (prompt "license? [default=unset]"))
        (description (prompt "description? [default=unset]")))
    (save-instance (make-system name author maintainer license description)
                   (directory-namestring (merge-pathnames path)))))
