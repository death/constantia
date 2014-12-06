;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

;;;; Package definitions

(in-package #:cl-user)

(defpackage #:constantia
  (:use #:cl #:alexandria #:trivial-gray-streams)
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
   #:as-list
   #:as-vector
   #:make-octet-vector
   #:concat-octet-vectors
   #:plist-get
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
   #:queue-clear
   #:wrap-string
   #:print-table
   ;; Event system
   #:event
   #:speaker
   #:define-event
   #:define-events
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
   #:ignore-message
   ;; Forwarding character output stream
   #:forwarding-character-output-stream
   #:forwarding-character-output-stream-stream
   ;; Case-translating stream
   #:case-translating-stream
   #:make-case-translating-stream
   #:ensure-case-translating-stream
   #:with-stream-case
   ;; OUT macro
   #:out
   #:outs
   #:define-out-user-op
   ;; Parse-Duration
   #:invalid-duration
   #:invalid-duration-string
   #:invalid-time-unit
   #:invalid-time-unit-string
   #:parse-duration))
