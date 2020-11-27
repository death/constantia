;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/side-by-side
  (:documentation
   "Show line-based output of multiple printers side-by-side.")
  (:use #:cl)
  (:import-from #:constantia/out #:out)
  (:export #:side-by-side
           #:sxs))

(in-package #:constantia/side-by-side)

(defmacro output-as-string (printer-form)
  "Evaluate PRINTER-FORM, returning whatever was output to the
standard output as a string."
  `(with-output-to-string (*standard-output*)
     ,printer-form))

(defun split-lines (string)
  "Split STRING into a list of strings, each representing a line."
  (with-input-from-string (stream string)
    (loop for line = (read-line stream nil nil)
          while line collect line)))

(defmacro output-as-lines (printer-form)
  "Evaluate PRINTER-FORM, returning whatever was output to the
standard output as a list of strings, each representing a line."
  `(split-lines (output-as-string ,printer-form)))

(defun max-length (sequences)
  "Return the length of the longest sequence in SEQUENCES."
  (reduce #'max sequences :key #'length :initial-value 0))

(defun side-by-side (printers &key (test #'equal)
                                   (nonmatching-indicator nil)
                                   (side-separator " | ")
                                   (number-lines t)
                                   (line-number-separator ": ")
                                   (sides-prefix "")
                                   (sides-suffix "")
                                   (side-prefix "")
                                   (side-suffix "")
                                   ((:stream *standard-output*) *standard-output*))
  "Show the (line-based) output of the printer functions
side-by-side."
  (assert (not (null printers)) (printers) "Need at least one side.")
  (let* ((sides (mapcar (lambda (printer) (output-as-lines (funcall printer))) printers))
         (side-length (reduce #'max sides :key #'max-length :initial-value 0))
         (line-number-width (and number-lines (ceiling (log (1+ (max-length sides)) 10)))))
    (loop for line-number from 1
          while (some #'identity sides)
          do (let ((lines (mapcar (lambda (side) (or (first side) "")) sides)))
               (map-into sides #'rest sides)
               (out (:q (number-lines
                         (:d line-number :width line-number-width)
                         line-number-separator))
                    sides-prefix)
               (loop for first = t then nil
                     for line in lines
                     do (out (:q ((not first)
                                  side-separator))
                             side-prefix
                             (:a line :pad-char #\Space :width side-length)
                             side-suffix))
               (out sides-suffix
                    (:q ((and nonmatching-indicator
                              (notevery (lambda (line) (funcall test line (first lines))) (rest lines)))
                         nonmatching-indicator))
                    (:%))))))

(defmacro sxs ((&rest options) &body forms)
  "Syntactic sugar for SIDE-BY-SIDE whose primary function is to take
forms instead of printer functions.  Aesthetic considerations led me
to prefer a known OPTIONS list rather than an evaluable form."
  `(side-by-side (list ,@(mapcar (lambda (form) `(lambda () ,form)) forms))
                 ,@options))

;; Example of use

(defun print-list (list)
  "Print each item in LIST on its own line."
  (dolist (item list)
    (princ item)
    (terpri)))

(defun example ()
  "Just a simple example for SXS."
  (sxs (:nonmatching-indicator " <- nonmatching")
    (print-list '(a b c))
    (print-list '(a x c))
    (print-list '(a b c d))))
