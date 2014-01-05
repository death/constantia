;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:constantia
  :depends-on (#:alexandria #:bordeaux-threads #:flexi-streams
               #:trivial-gray-streams)
  :components
  ((:file "packages")
   (:file "misc" :depends-on ("packages"))
   (:file "event" :depends-on ("packages" "misc"))
   (:file "scan" :depends-on ("packages" "misc" "event"))
   (:file "forwarding-character-output-stream" :depends-on ("packages"))
   (:file "case-translating-stream"
          :depends-on ("packages" "forwarding-character-output-stream"))
   (:file "out"
          :depends-on ("packages"
                       "case-translating-stream"
                       "forwarding-character-output-stream"))))
