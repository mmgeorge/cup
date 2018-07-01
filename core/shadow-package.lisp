(defpackage :cup/core/shadow-package
  (:use :cl)
  (:export #:make-shadow-package
           #:shadow-package
           #:shadow-package-name
           #:shadow-package-symbols))

(in-package :cup/core/shadow-package)

(defclass shadow-package ()
  ((name :reader shadow-package-name :initarg :name)
   (symbols :reader shadow-package-symbols :initarg :symbols)))

(defun make-shadow-package (name symbols)
  (make-instance 'shadow-package :name name :symbols symbols))
