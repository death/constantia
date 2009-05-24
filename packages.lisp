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
   #:kahan-sum
   #:kahan-c
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
   #:make-tracing-listener
   ;; Scanners
   #:scan-object-available
   #:scan-object
   #:scan-source
   #:scanner
   #:object-scanned
   #:continue-scanning
   #:reset-scanner
   #:fixlen-message-scanner
   #:varlen-message-scanner
   #:*delimited-message-max-length*
   #:*delimited-message-delimiter*
   #:message-too-big
   #:message-too-big-scanner
   #:delimited-message-scanner
   #:store-max-length
   #:ignore-message))
