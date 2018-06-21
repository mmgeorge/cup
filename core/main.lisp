(defpackage :cup/core/main
  (:use :cl)
  (:export #:main))

(in-package :cup/core/main)

(defun main (argv)
  (declare (ignore argv))
  (write-line "Hello from build-app!"))
