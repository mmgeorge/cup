(defpackage :cup/core/shadow-package
  (:use :cl)
  (:export #:make-shadow-package #:shadow-package))

(in-package :cup/core/shadow-package)

(defclass shadow-package ()
  ((name :reader shadow-package-name :initarg :name)))

(defun make-shadow-package (name)
  (make-instance 'shadow-package :name name))
