

(defpackage :cup/core/shadow-system
  (:use :cl)
  (:import-from :esrap)
  (:import-from :cup/core/shadow-package #:shadow-package #:make-shadow-package)
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

(defun package-add (package system)
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
  (describe system))


(defun not-string-term-p (char)
  (not (eql #\" char)))

(defun not-space-or-end-p (char)
  (and (not (eql #\space char))
       (not (eql #\) char))))


(esrap:defrule whitespace (+ (or #\space #\tab #\newline)))

(esrap:defrule sexp-begin (and (esrap:? whitespace) "(") (:identity t))

(esrap:defrule sexp-end (and (esrap:? whitespace) ")") (:identity t))

(esrap:defrule token (+ (alpha-char-p character)) (:text t))

(esrap:defrule string-inner (+ (not-string-term-p character)) (:text t))

(esrap:defrule string-term (and (esrap:? whitespace) "\"")(:constant ""))

(esrap:defrule string (and string-term (+ (and (esrap:? whitespace) string-inner)) string-term) (:text t))

(esrap:defrule key-token (+ (not-space-or-end-p character)) (:text t))

(esrap:defrule keyed-arg (and (esrap:? whitespace) ":" token whitespace string)
  (:destructure (whitespace colon match whitespace2 token)
                (declare (ignore whitespace)
                         (ignore whitespace2)
                         (ignore colon))
                (list match token)))

(esrap:defrule key (and (esrap:? whitespace) ":" key-token)
  (:destructure (whitespace colon key)
                (declare (ignore whitespace)
                         (ignore colon))
                (concatenate 'string ":" key)))

(esrap:defrule keyed-args (+ keyed-arg) (:identity t))

(esrap:defrule defsystem (and sexp-begin "asdf:defsystem" keyed-args sexp-end)
  (:destructure (s-begin token keys s-end)
                (declare (ignore s-begin)
                         (ignore s-end))
                (list token keys)))

(esrap:defrule keys-list (and (esrap:? whitespace) "'(" (+ key) (esrap:? whitespace) ")")
  (:destructure (w1 start keys w2 end)
                (declare (ignore w1)
                         (ignore start)
                         (ignore w2)
                         (ignore end))
                keys))

;; e.g. (asdf:register-system-packages :unix-opts '(:unix-opts))
(esrap:defrule defpackage (and sexp-begin "asdf:register-system-packages" key keys-list sexp-end)
  (:destructure (s-begin token key keys s-end)
                (declare (ignore s-begin)
                         (ignore s-end))
                (list token key keys)))

(esrap:defrule sexp (or defsystem defpackage)(:identity t))
(esrap:defrule sexps (+ sexp)(:identity t))


(defun get-value (key list)
  (car (assoc key list :test #'string=)))

(defun extract-system (sexps)
  (or (car (remove-if-not (lambda (x) (string-equal "asdf:defsystem" (car x))) sexps))
      (error "I CAN'T EXTRACT A DEFINED SYSTEM!")))
    
(defun extract-packages (sexps)
  (remove-if-not (lambda (x) (string-equal "asdf:register-system-packages" (car x))) sexps))
  
(defun parse-source (source)
  (let* ((result (esrap:parse 'sexps source))
         (system (read-system (extract-system result)))
         (packages (read-packages (extract-packages result))))
    (describe system)
    packages))
    ;(dolist (package packages)
     ; (package-add package system))
    ;system))

(defun read-packages (sexps)
  (mapcar #'read-package sexps))

(defun read-package (sexp)
  (let ((name (cadr sexp)))
    (make-shadow-package name)))

(defun read-system (result)
  (let* ((key-args (cadr result)))
    (let ((name (get-value "name" key-args))
          (author (get-value "author" key-args))
          (maintainer (get-value "maintainer" key-args))
          (license (get-value "license" key-args))
          (description (get-value "description" key-args)))
      (make-shadow-system name author maintainer license description))))

;;(describe (parse-system "(asdf:defsystem :name \"funny\" :author \"bunny\" :maintainer \"stole\" :license \"all of my\"    :description \"MONEY. \")"))

(defvar *test2* "(asdf:defsystem :name \"funny\" :author \"bunny\" :maintainer \"stole\" :license \"all of my\"    :description \"MONEY. \") (asdf:register-system-packages :some-pak '(:arg1 :arg2)) (asdf:register-system-packages :some-pak '(:arg1 :arg2))")
