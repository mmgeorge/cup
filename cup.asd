(asdf:defsystem :cup
    :version "0.1.0"
    :author "Matt George"
    :maintainer "Matt George"
    :license "none"
    :description "Project starter for common lisp"
    :class :package-inferred-system
    :depends-on (:cup/exports))

(asdf:register-system-packages :unix-opts '(:unix-opts))
(asdf:register-system-packages :cl-ppcre '(:cl-ppcre))
