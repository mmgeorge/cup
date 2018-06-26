(defpackage :cup/core/shadow-system
  (:use :cl)
  (:import-from :esrap)
  (:import-from :cup/core/shadow-package #:shadow-package)
  (:export #:make-shadow-system #:save-instance))

(in-package :cup/core/shadow-system)

(defclass shadow-system ()
  ((name :reader shadow-system-name :initarg :name)
   (author :reader shadow-system-author :initarg :author)
   (maintainer :reader shadow-system-maintainer :initarg :maintainer)
   (license :reader shadow-system-license :initarg :license)
   (description :reader shadow-system-description :initarg :description)
   (packages :accessor shadow-system-packages :initarg :packages))
  (:default-initargs :packages '()))

(defun shadow-system-package-add (package system)
  (setf (shadow-system-packages system)
        (cons package (shadow-system-packages system))))

(defun make-shadow-system (name author maintainer license description)
  (make-instance 'shadow-system :name name
                         :author author
                         :maintainer maintainer
                         :license license
                         :description description))

(defmethod save-instance ((shadow-system shadow-system) directory)
  (with-open-file (stream (ensure-directories-exist
                           (make-pathname :directory directory
                                          :name (shadow-system-name shadow-system)
                                          :type "asd"))
                          :direction :output)
    (progn (princ directory)
           ;; Maybe use a macro instead?
           (format stream "(asdf:defshadow-system :~a
  :version \"0.1.0\"
  :author \"~a\"
  :maintainer \"~a\"
  :license \"~a\"
  :description \"~a\"
  :class :package-inferred-shadow-system
  :depends-on (:~a/exports))
" (shadow-system-name shadow-system)
(shadow-system-author shadow-system)
(shadow-system-maintainer shadow-system)
(shadow-system-license shadow-system)
(shadow-system-description shadow-system)
(shadow-system-name shadow-system)
))))


(defmethod register ((package shadow-package) (system shadow-system))
  (shadow-system-package-add package system)
  (describe system)
  )


(esrap:defrule whitespace (+ (or #\space #\tab #\newline)))

(esrap:defrule sexp-begin (and (esrap:? whitespace) "(")
  (:identity t))
(esrap:defrule sexp-end (and (esrap:? whitespace) ")")
  (:identity t))

(esrap:defrule token (+ (alpha-char-p character))
  (:text t))

(esrap:defrule key (and (esrap:? whitespace) ":" token)
  (:destructure (whitespace colon match)
                (declare (ignore whitespace)
                         (ignore colon))
                match))

(esrap:defrule keys (+ key)
  (:identity t))

(esrap:defrule sexp (and sexp-begin token keys sexp-end)
  (:destructure (s-begin token keys s-end)
                (declare (ignore s-begin)
                         (ignore s-end))
                (list token keys)))



(esrap:parse 'key "dog")
