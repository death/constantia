;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/stream/forwarding
  (:use #:cl #:trivial-gray-streams)
  (:export
   #:forwarding-character-output-stream
   #:forwarding-character-output-stream-stream))

(in-package #:constantia/stream/forwarding)


;;;; Forwarding character output stream

(defclass forwarding-character-output-stream (trivial-gray-stream-mixin
                                              fundamental-character-output-stream)
  ((stream :initarg :stream :accessor forwarding-character-output-stream-stream)))

(defmethod stream-write-char ((stream forwarding-character-output-stream) char)
  (write-char char (forwarding-character-output-stream-stream stream)))

(defmethod stream-write-string ((stream forwarding-character-output-stream) string &optional (start 0) end)
  (write-string string (forwarding-character-output-stream-stream stream) :start start :end end))

(defmethod stream-terpri ((stream forwarding-character-output-stream))
  (terpri (forwarding-character-output-stream-stream stream)))

(defmethod stream-fresh-line ((stream forwarding-character-output-stream))
  (fresh-line (forwarding-character-output-stream-stream stream)))

(defmethod stream-finish-output ((stream forwarding-character-output-stream))
  (finish-output (forwarding-character-output-stream-stream stream)))

(defmethod stream-force-output ((stream forwarding-character-output-stream))
  (force-output (forwarding-character-output-stream-stream stream)))

(defmethod stream-clear-output ((stream forwarding-character-output-stream))
  (clear-output (forwarding-character-output-stream-stream stream)))
