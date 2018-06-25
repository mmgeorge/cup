(defpackage :cup/core/traits/persist
  (:use :cl)
  (:export #:save-instance #:load-instance))

(in-package :cup/core/traits/persist)

(defgeneric save-instance (instance path)
  (:documentation "Save an instance to disk"))

(defgeneric load-instance (instance path)
  (:documentation "Load an instance from disk"))
