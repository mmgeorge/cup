(defpackage :cup/core/main
  (:use :cl)
  (:import-from :unix-opts)
  (:import-from :cl-ppcre)
  (:export #:main))

(in-package :cup/core/main)

(opts:define-opts
  (:name :test :description "hello args!" :long "test" :short #\t :arg-parser #'identity))

(defun main (argv)
  (multiple-value-bind (options)
      (opts:get-opts argv)
    (if (getf options :test)
        (format t "Found an arg!: ~a" (getf options :test)))))
