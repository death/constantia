;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/scan
  (:use #:cl #:constantia/misc #:constantia/event)
  (:import-from #:flexi-streams
                #:make-in-memory-output-stream
                #:get-output-stream-sequence)
  (:export
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

(in-package #:constantia/scan)


;;;; Scanner protocol

(define-event scan-object-available
    "Fired when the scanner has recognized a complete object
designation."
  scan-object
  scan-source)

(defclass scanner (speaker)
  ()
  (:documentation "The base class of all scanners.

A scanner takes pieces of object designations and attempts to assemble
them and construct the corresponding objects.  The pieces are octet
vectors, or subsequences of such vectors.  When an object is
constructed, the scanner will fire a SCAN-OBJECT-AVAILABLE event
containing that object and reset itself."))

(defgeneric object-scanned (object scanner)
  (:documentation "Called when a complete object designation was
recognized and an object was constructed.  Fires the
SCAN-OBJECT-AVAILABLE event and resets the scanner."))

(defgeneric continue-scanning (buffer start end scanner)
  (:documentation "Continue to assemble object designations and
construct objects from the buffer subsequence supplied.  When an
object is constructed, call OBJECT-SCANNED with that object and return
to the caller.  The current offset within the subsequence is
returned."))

(defgeneric reset-scanner (scanner)
  (:documentation "Reset the scanner's state, i.e. dump any partial
object designation."))

(defmethod object-scanned (object (scanner scanner))
  (fire scanner 'scan-object-available :scan-object object :scan-source scanner)
  (reset-scanner scanner))


;;;; Fixed-length message scanner

(defclass fixlen-message-scanner (scanner)
  ((length :initarg :length :reader scan-message-length)
   (message :accessor scan-message))
  (:documentation "Construct fixed-length octet vectors from parts.
The resulting vectors are owned by the scanner, and receivers
shouldn't attempt to keep or modify them."))

(defmethod initialize-instance :after ((scanner fixlen-message-scanner)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (scan-message scanner)
        (make-array (scan-message-length scanner)
                    :element-type 'u8 :fill-pointer 0)))

(defmethod continue-scanning (buffer start end (scanner fixlen-message-scanner))
  (let ((message (scan-message scanner)))
    (flet ((maybe-scanned ()
             (when (= (scan-message-length scanner) (length message))
               (object-scanned message scanner)
               (return-from continue-scanning start))))
      (loop repeat (- end start) doing
            (maybe-scanned)
            (vector-push (aref buffer start) message)
            (incf start))
      (maybe-scanned)))
  start)

(defmethod reset-scanner ((scanner fixlen-message-scanner))
  (setf (fill-pointer (scan-message scanner)) 0))


;;;; Variable-length message scanner

(defclass varlen-message-scanner (scanner)
  ((size-scanner
    :initform (make-instance 'fixlen-message-scanner :length 4)
    :reader scan-size-scanner)
   (message-scanner :initform nil :accessor scan-message-scanner)
   (active-scanner :initform nil :accessor scan-active-scanner))
  (:documentation "Construct variable-length octet vectors from parts.
Each such octet vector is prefixed with a u32 integer specifying its
length.  This integer is represented as 4 octets with little-endian
order.  The resulting vectors are owned by the scanner, and receivers
shouldn't attempt to keep or modify them."))

(defmethod initialize-instance :after ((scanner varlen-message-scanner) &rest initargs)
  (declare (ignore initargs))
  (let ((size-scanner (scan-size-scanner scanner)))
    (add-listener (lambda (ev) (varlen-scanned-size ev scanner)) size-scanner)
    (setf (scan-active-scanner scanner) size-scanner)))

(defmethod continue-scanning (buffer start end (scanner varlen-message-scanner))
  (with-accessors ((active scan-active-scanner) (ms scan-message-scanner)
                   (ss scan-size-scanner)) scanner
    (let ((offset (if (eq active ss)
                      (continue-scanning buffer start end ss)
                      start)))
      (if (= offset end)
          end
          (continue-scanning buffer offset end ms)))))

(defun varlen-scanned-size (ev scanner)
  (when (typep ev 'scan-object-available)
    (with-accessors ((active scan-active-scanner)
                     (ms scan-message-scanner)) scanner
      (setf ms (make-instance 'fixlen-message-scanner
                              :length (octets-to-u32 (scan-object ev))))
      (setf active ms)
      (add-listener (lambda (ev)
                      (when (typep ev 'scan-object-available)
                        (object-scanned (scan-object ev) scanner)))
                    ms))))

(defun octets-to-u32 (vector)
  (let ((u32 0))
    (setf (ldb (byte 8  0) u32) (aref vector 0))
    (setf (ldb (byte 8  8) u32) (aref vector 1))
    (setf (ldb (byte 8 16) u32) (aref vector 2))
    (setf (ldb (byte 8 24) u32) (aref vector 3))
    u32))

(defmethod reset-scanner ((scanner varlen-message-scanner))
  (with-accessors ((active scan-active-scanner) (ms scan-message-scanner)
                   (ss scan-size-scanner)) scanner
    (reset-scanner ss)
    (setf active ss)
    (setf ms nil)))


;;;; Delimited message scanner

(defvar *delimited-message-max-length* 1024
  "The default maximum message length in octets which, if reached,
will cause the scanner to signal a MESSAGE-TOO-BIG condition.")

(defvar *delimited-message-delimiter* 0
  "The default value for the delimiter octet separating the
messages.")

(define-condition message-too-big (error)
  ((scanner :initarg :scanner :reader message-too-big-scanner))
  (:documentation "A condition that is signaled if the message length
exceeds the scanner's maximum message length."))

(defclass delimited-message-scanner (scanner)
  ((max-length :initarg :max-length :accessor scan-message-max-length)
   (delimiter :initarg :delimiter :reader scan-message-delimiter)
   (ignoring-message :initform nil :accessor scan-ignoring-message-p)
   (message-stream
    :initform (make-in-memory-output-stream)
    :accessor scan-message)
   (message-length :initform 0 :accessor scan-message-length))
  (:default-initargs :max-length *delimited-message-max-length*
    :delimiter *delimited-message-delimiter*)
  (:documentation "Construct variable-length octet vectors from parts.
Each such octet vector is terminated by a single octet with a specific
value, the delimiter.  The resulting vectors are owned by the scanner,
and receivers shouldn't attempt to keep or modify them."))

(defmethod continue-scanning (buffer start end (scanner delimited-message-scanner))
  (with-accessors ((delimiter scan-message-delimiter)
                   (max-length scan-message-max-length)
                   (ignoring scan-ignoring-message-p)
                   (stream scan-message)
                   (length scan-message-length))
      scanner
    (do ((offset start (1+ offset)))
        ((= offset end) offset)
      (do () ((or (<= length max-length) ignoring))
        (restart-case
            (error 'message-too-big :scanner scanner)
          (store-max-length (new-max-length)
            :report "Store a new maximum length for messages."
            :interactive prompt-max-length
            (setf max-length new-max-length))
          (ignore-message ()
            :report "Ignore octets until delimiter."
            (setf ignoring t))))
      (let ((octet (aref buffer offset)))
        (cond ((and ignoring (= octet delimiter))
               (reset-scanner scanner))
              ((= octet delimiter)
               (object-scanned
                (get-output-stream-sequence stream)
                scanner)
               (return-from continue-scanning (1+ offset)))
              ((not ignoring)
               (write-byte octet stream)
               (incf length)))))))

(defmethod reset-scanner ((scanner delimited-message-scanner))
  (setf (scan-ignoring-message-p scanner) nil)
  (get-output-stream-sequence (scan-message scanner))
  (setf (scan-message-length scanner) 0))

(defun prompt-max-length ()
  (format *query-io* "~&Enter new max length: ")
  (list (read *query-io*)))

(defun store-max-length (value &optional condition)
  (when (find-restart 'store-max-length condition)
    (invoke-restart 'store-max-length value)))

(defun ignore-message (&optional condition)
  (when (find-restart 'ignore-message condition)
    (invoke-restart 'ignore-message)))
