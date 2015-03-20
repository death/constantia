;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/stream/case-translating
  (:use #:cl #:trivial-gray-streams #:constantia/stream/forwarding)
  (:import-from #:alexandria #:with-gensyms #:once-only)
  (:export
   #:case-translating-stream
   #:make-case-translating-stream
   #:ensure-case-translating-stream
   #:with-stream-case))

(in-package #:constantia/stream/case-translating)


;;;; Case-translating stream

(defclass case-translating-stream (forwarding-character-output-stream)
  ((last-char :initform #\Space :accessor case-translating-stream-last-char)
   (case :initarg :case :accessor case-translating-stream-case)))

(defun make-case-translating-stream (stream &key (case :preserve))
  (make-instance 'case-translating-stream
                 :stream stream
                 :case case))

(defun ensure-case-translating-stream (stream)
  (if (typep stream 'case-translating-stream)
      stream
      (make-case-translating-stream stream)))

(defmacro with-stream-case ((stream case) &body forms)
  (with-gensyms (old-case)
    (once-only (stream)
      `(let ((,old-case (case-translating-stream-case ,stream)))
         (setf (case-translating-stream-case ,stream) ,case)
         (unwind-protect
              (progn ,@forms)
           (setf (case-translating-stream-case ,stream) ,old-case))))))

(defmethod stream-write-char ((stream case-translating-stream) char)
  (setf char
        (ecase (case-translating-stream-case stream)
          (:preserve char)
          (:upcase (char-upcase char))
          (:downcase (char-downcase char))
          (:capitalize
           (if (alphanumericp (case-translating-stream-last-char stream))
               (char-downcase char)
               (char-upcase char)))))
  (call-next-method stream char)
  (setf (case-translating-stream-last-char stream) char))

(defmethod stream-write-string ((stream case-translating-stream) string &optional (start 0) end)
  (loop with output-stream = (forwarding-character-output-stream-stream stream)
        with case = (case-translating-stream-case stream)
        for last-char = (case-translating-stream-last-char stream) then char
        for index from start below (or end (length string))
        for char = (char string index)
        do (write-char
            (ecase case
              (:preserve char)
              (:upcase (char-upcase char))
              (:downcase (char-downcase char))
              (:capitalize
               (if (alphanumericp last-char)
                   (char-downcase char)
                   (char-upcase char))))
            output-stream)
        finally (setf (case-translating-stream-last-char stream) last-char)))

(defmethod stream-terpri :after ((stream case-translating-stream))
  (setf (case-translating-stream-last-char stream) #\Newline))

(defmethod stream-fresh-line :after ((stream case-translating-stream))
  (setf (case-translating-stream-last-char stream) #\Newline))
