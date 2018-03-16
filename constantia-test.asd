;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:constantia-test
  :description "Tests for Constantia"
  :license "MIT"
  :depends-on (#:parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :constantia-test :run-tests))
  :components
  ((:file "test")))
