(defpackage :cup/core/main
  (:use :cl :cup/core/shadow-system :cup/core/shadow-package)
  (:import-from :unix-opts)
  (:import-from :cl-ppcre)
  ;(:import-from :cup/core/traits/persist #:save-instance)
  (:import-from :cup/core/shadow-system)
  (:import-from :cup/core/shadow-package)
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
          (t (prompt-make-system)))
    (and *bound-system* (write-bound-system))))

(defun handle-command (command free-vars options)
  (cond ((string-equal command "add")(handle-package-add free-vars options))))

(defun handle-package-add (free-vars options)
  (declare (ignore options))
  (let ((package-name (if (null free-vars)
                          (error "Must specify a package name")
                          (read (make-string-input-stream (car free-vars)))
                          )))

    (format t "Adding package ~a~%" package-name)
    (register (make-shadow-package package-name `'(,package-name)) (bound-system))))

(defvar *bound-system* nil)

(defvar *bound-path* "./test.asd")

(defun get-bound-system ()
  (let ((path *bound-path*))
    (with-open-file (infile path)
      (parse-system infile))))

(defun write-bound-system ()
  (let ((path "./test.asd"))
    (with-open-file (outfile path
                             :direction :output
                             :if-exists :supersede)
      (write-shadow-system (bound-system) outfile))))



(defun defsystem-p (sexp)
  (eq 'asdf/parse-defsystem:defsystem (car sexp)))

(defun parse-system (stream)
  (let ((system nil)
        (packages nil))
    (loop for sexp = (read stream nil) while sexp do
      (cond ((defsystem-p sexp)(setf system sexp))
            (t (setf packages (cons sexp packages)))))
    (parse-defsystem system packages)))

(defun to-shadow-system (system)
  (let ((name (asdf:component-name system))
        (author (asdf:system-author system))
        (maintainer (asdf:system-maintainer system))
        (license (asdf:system-licence system))
        (description (asdf:system-description system)))
    (make-shadow-system name author maintainer license description)))

(defun to-shadow-package (package-sexp)
  (destructuring-bind (fun name symbols) package-sexp
    (declare (ignore fun))
    (make-shadow-package name symbols)))

(defun parse-defsystem (defsystem packages)
  (let* ((system (eval defsystem))
         (shadow-system (to-shadow-system system))
         (shadow-packages (mapcar 'to-shadow-package packages)))
    (loop for shadow-package in shadow-packages do
      (register shadow-package shadow-system))
    shadow-system))

(defun write-shadow-system (shadow-system stream)
  (let ((name (read (make-string-input-stream (concatenate 'string ":" (shadow-system-name shadow-system)))))
        (depends-on
          (read (make-string-input-stream
                 (concatenate 'string ":" (shadow-system-name shadow-system) "/exports"))))
        (author (shadow-system-author shadow-system))
        (maintainer (shadow-system-maintainer shadow-system))
        (license (shadow-system-license shadow-system))
        (description (shadow-system-description shadow-system))
        (packages (shadow-system-packages shadow-system)))
    (pprint (list 'asdf:defsystem name
                 :author author
                 :maintainer maintainer
                 :license license
                 :description description
                 :class :package-inferred-system
                 :depends-on (list depends-on)
                 ) stream)
    (loop for package in packages do
      (pprint (list 'asdf:register-system-packages
                   (shadow-package-name package)
                   (shadow-package-symbols package)) stream))))

(defun bound-system ()
  (or *bound-system* (setf *bound-system* (get-bound-system))))


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
