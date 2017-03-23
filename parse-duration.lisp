;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/parse-duration
  (:use #:cl)
  (:import-from #:alexandria #:plist-hash-table #:emptyp)
  (:export
   #:invalid-duration
   #:invalid-duration-string
   #:invalid-time-unit
   #:invalid-time-unit-string
   #:parse-duration))

(in-package #:constantia/parse-duration)

;; Stolen from (and almost as ugly as) the Go time.ParseDuration
;; implementation: http://golang.org/pkg/time/#ParseDuration
;;
;; Returns a rational instead of float. Uses "d" to specify days.

(define-condition invalid-duration (error)
  ((s :initarg :string :reader invalid-duration-string))
  (:report report-invalid-duration))

(defun report-invalid-duration (condition stream)
  (format stream "Invalid duration ~S." (invalid-duration-string condition)))

(define-condition invalid-time-unit (error)
  ((u :initarg :unit :reader invalid-time-unit-string))
  (:report report-invalid-time-unit))

(defun report-invalid-time-unit (condition stream)
  (format stream "Invalid time unit ~S." (invalid-time-unit-string condition)))

(defparameter *unit-map*
  (plist-hash-table
   '("ns" 1
     "us" 1000
     "ms" 1000000
     "s"  1000000000
     "m"  60000000000
     "h"  3600000000000
     "d"  86400000000000)
   :test 'equal))

(defun unit-map (u)
  (or (gethash u *unit-map*)
      (error 'invalid-time-unit :unit u)))

(defun leading-int (s)
  (do ((i 0 (1+ i))
       (x 0 (+ (* x 10) (digit-char-p (char s i)))))
      ((or (= i (length s))
           (not (digit-char-p (char s i))))
       (values x (subseq s i)))))

(define-compiler-macro parse-duration-1 (&whole form s)
  (if (stringp s)
      (parse-duration-1 s)
      form))

(define-compiler-macro parse-duration (s &optional (g "1ns"))
  `(/ (parse-duration-1 ,s)
      (parse-duration-1 ,g)))

(defun parse-duration (s &optional (g "1ns"))
  "Parse a duration string into a number of time units (G)."
  (/ (parse-duration-1 s)
     (parse-duration-1 g)))

(defun parse-duration-1 (s)
  "Parse a duration string into a number of nanoseconds."
  (let ((orig s)
        (f 0)
        (neg nil))

    ;; Consume [-+]?
    (when (not (emptyp s))
      (let ((c (char s 0)))
        (when (or (char= c #\-) (char= c #\+))
          (setf neg (char= c #\-))
          (setf s (subseq s 1)))))

    ;; Special case: if all that is left is "0", this is zero.
    (when (equal s "0")
      (return-from parse-duration-1 0))
    (when (emptyp s)
      (error 'invalid-duration :string orig))
    (loop while (not (emptyp s))
          do (let ((g 0)                ;this element of the sequence
                   (x 0))

               ;; The next character must be [0-9.]
               (when (not (or (char= (char s 0) #\.)
                              (digit-char-p (char s 0))))
                 (error 'invalid-duration :string orig))

               ;; Consume [0-9]*
               (let ((pl (length s)))
                 (multiple-value-setq (x s) (leading-int s))
                 (setf g x)
                 (let ((pre (/= pl (length s)))) ;whether we consume anything before a period

                   ;; Consume (\.[0-9]*)?
                   (let ((post nil))
                     (when (and (not (emptyp s))
                                (char= (char s 0) #\.))
                       (setf s (subseq s 1))
                       (setf pl (length s))
                       (multiple-value-setq (x s) (leading-int s))
                       (do ((scale 1 (* scale 10))
                            (n (- pl (length s)) (- n 1)))
                           ((= n 0) (incf g (/ x scale))))
                       (setf post (/= pl (length s))))
                     (when (and (not pre) (not post))
                       ;; No digits (e.g., ".s" or "-.s")
                       (error 'invalid-duration :string orig))

                     (let ((i (do ((i 0 (1+ i)))
                                  ((= i (length s)) i)
                                (let ((c (char s i)))
                                  (when (or (char= c #\.) (digit-char-p c))
                                    (return i))))))
                       (when (zerop i)
                         (error 'invalid-duration :string orig))
                       (incf f (* g (unit-map (subseq s 0 i))))
                       (setf s (subseq s i))))))))
    (when neg
      (setf f (- f)))
    f))
