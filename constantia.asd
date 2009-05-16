;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:constantia
  :depends-on (#:alexandria #:bordeaux-threads #:closer-mop)
  :components
  ((:file "packages")
   (:file "misc" :depends-on ("packages"))
   (:file "event" :depends-on ("packages" "misc"))))
