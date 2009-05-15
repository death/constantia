;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:constantia
  :depends-on ()
  :components
  ((:file "packages")
   (:file "misc")))
