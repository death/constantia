;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

;;;; Package definitions

(in-package #:cl-user)

(defpackage #:constantia
  (:use #:cl #:alexandria)
  (:documentation
   "Contains and exports all public symbols of Constantia.")
  (:export
   ;; Miscellaneous utilities
   #:u8
   #:u16
   #:u32
   #:defsubst
   #:type-eq
   #:square
   #:abs-
   #:singlep
   #:in-range-p
   #:as-keyword
   #:slurp-file
   #:map-lines-in-file
   #:default
   #:gethash/i
   #:doseq
   #:best-element
   #:agetf
   #:with-options
   #:kahan
   #:make-kahan
   #:kahan-p
   #:copy-kahan
   #:k+
   #:float-sum
   #:moment
   #:moment-mean
   #:moment-adev
   #:moment-sdev
   #:moment-svar
   #:moment-skew
   #:moment-kurt
   #:make-queue
   #:queue-push
   #:queue-top
   #:queue-pop
   #:queue-empty-p
   #:queue-size
   ;; Event system
   #:event
   #:speaker
   #:define-event
   #:define-events
   #:on-event
   #:add-listener
   #:remove-listener
   #:clear-listeners
   #:fire
   #:tracing-listener
   #:tracing-listener-output
   #:tracing-listener-describe-p
   #:anonymous-listener))
