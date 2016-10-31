;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:constantia
  :description "Constantia general-purpose utilities"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("constantia/all"))
