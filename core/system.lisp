(defpackage :cup/core/system
  (:use :cl)
  (:export #:make-system #:save-instance))

(in-package :cup/core/system)

(defclass system ()
  ((name :reader system-name :initarg :name)
   (author :reader system-author :initarg :author)
   (maintainer :reader system-maintainer :initarg :maintainer)
   (license :reader system-license :initarg :license)
   (description :reader system-description :initarg :description)))

(defun make-system (name author maintainer license description)
  (make-instance 'system :name name
                         :author author
                         :maintainer maintainer
                         :license license
                         :description description))

(defmethod save-instance ((system system) directory)
  (with-open-file (stream (ensure-directories-exist (make-pathname :directory directory
                                                                   :name (system-name system)
                                                                   :type "asd"))
                          :direction :output)
    (progn (princ directory)
           system)))
