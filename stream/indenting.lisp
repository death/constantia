;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/stream/indenting
  (:use #:cl #:trivial-gray-streams)
  (:import-from #:alexandria
                #:once-only)
  (:export
   #:indenting-stream
   #:indenting-stream-stream
   #:indenting-stream-fill-char
   #:indenting-stream-indentation
   #:indenting-stream-delta
   #:ensure-indenting-stream
   #:indent
   #:outdent
   #:with-indent))

(in-package #:constantia/stream/indenting)

(defclass indenting-stream (trivial-gray-stream-mixin
                            fundamental-character-output-stream)
  ((stream :initarg :stream :accessor indenting-stream-stream)
   (last-char :initform #\Newline :accessor indenting-stream-last-char)
   (fill-char :initarg :fill-char :accessor indenting-stream-fill-char)
   (indentation :initarg :indentation :accessor indenting-stream-indentation)
   (delta :initarg :delta :accessor indenting-stream-delta))
  (:default-initargs :fill-char #\Space :indentation 0 :delta 2))

(defmethod stream-write-char ((stream indenting-stream) char)
  (when (and (eql (indenting-stream-last-char stream) #\Newline)
             (not (eql char #\Newline)))
    (loop repeat (max 0 (indenting-stream-indentation stream))
          do (write-char (indenting-stream-fill-char stream)
                         (indenting-stream-stream stream))))
  (write-char char (indenting-stream-stream stream))
  (setf (indenting-stream-last-char stream) char))

(defmethod stream-write-string ((stream indenting-stream) string &optional start end)
  (do ((i (or start 0) (1+ i)))
      ((>= i (or end (length string))))
    (stream-write-char stream (char string i))))

(defmethod stream-terpri :after ((stream indenting-stream))
  (setf (indenting-stream-last-char stream) #\Newline))

(defmethod stream-fresh-line :after ((stream indenting-stream))
  (setf (indenting-stream-last-char stream) #\Newline))

(defmethod close ((stream indenting-stream) &key abort)
  (close (indenting-stream-stream stream) :abort abort))

(defun ensure-indenting-stream (stream &key (initial-fill-char #\Space)
                                            (initial-indentation 0)
                                            (initial-delta 2))
  (if (typep stream 'indenting-stream)
      stream
      (make-instance 'indenting-stream
                     :stream stream
                     :fill-char initial-fill-char
                     :indentation initial-indentation
                     :delta initial-delta)))

(defun indent (stream &optional delta)
  (check-type stream indenting-stream)
  (incf (indenting-stream-indentation stream)
        (or delta (indenting-stream-delta stream))))

(defun outdent (stream &optional delta)
  (check-type stream indenting-stream)
  (decf (indenting-stream-indentation stream)
        (or delta (indenting-stream-delta stream))))

(defmacro with-indent ((stream &optional delta) &body forms)
  (once-only (stream delta)
    `(progn
       (indent ,stream ,delta)
       (unwind-protect (progn ,@forms)
         (outdent ,stream ,delta)))))
