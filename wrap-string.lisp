;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/wrap-string
  (:use #:cl)
  (:export
   #:wrap-string))

(in-package #:constantia/wrap-string)

;; Need to check out Knuth's line-breaking algorithm sometime.

(defgeneric wrap-string (string width mode))

(defmethod wrap-string (string width (mode (eql :cut)))
  (if (<= (length string) width)
      (list string)
      (list (subseq string 0 width))))

(defmethod wrap-string (string width (mode (eql :ellipsis)))
  (let ((ellipsis "..."))
    (if (<= (length string) width)
        (list string)
        (list (concatenate 'string
                           (subseq string 0 (- width (length ellipsis)))
                           ellipsis)))))

(defmethod wrap-string (string width (mode (eql :character)))
  ;; For proper wrapping, width must be at least 2 if string length is
  ;; greater than 1.
  (if (<= (length string) width)
      (list string)
      (let ((lines '()))
        (block collect-lines
          (loop
           (let ((add-dash (not (or (eql #\Space (char string (- width 2)))
                                    (eql #\Space (char string (- width 1)))))))
             (push (with-output-to-string (out)
                     (write-string string out :end (- width 1))
                     (when add-dash
                       (write-char #\- out)))
                   lines)
             (setf string (string-trim '(#\Space) (subseq string (- width 1))))
             (cond ((= 0 (length string))
                    (return-from collect-lines))
                   ((<= (length string) width)
                    (push string lines)
                    (return-from collect-lines))))))
        (nreverse lines))))

(defun map-words (function string)
  (with-input-from-string (in string)
    (let ((out (make-string-output-stream)))
      (flet ((flush ()
               (let ((word (get-output-stream-string out)))
                 (unless (= 0 (length word))
                   (funcall function word)))))
        (do ((c (read-char in nil nil)
                (read-char in nil nil)))
            ((null c) (flush))
          (if (member c '(#\Space #\Tab #\Newline))
              (flush)
              (write-char c out))))))
  (values))

(defmethod wrap-string (string width (mode (eql :word)))
  (if (<= (length string) width)
      (list string)
      (let ((lines '())
            (out (make-string-output-stream))
            (out-n 0))
        (flet ((flush ()
                 (when (> out-n 0)
                   (push (get-output-stream-string out) lines)
                   (setf out-n 0))))
          (map-words
           (lambda (word)
             (cond ((<= (+ out-n (length word) 1) width)
                    (write-string word out)
                    (write-char #\Space out)
                    (incf out-n (1+ (length word))))
                   (t
                    (flush)
                    (loop
                     (when (<= (length word) width)
                       (return))
                     ;; Need to handle lines that end with spaces,
                     ;; multiple spaces, etc. correctly.
                     (push (concatenate 'string
                                        (subseq word 0 (- width 1))
                                        "-")
                           lines)
                     (setf word (subseq word (- width 1))))
                    (unless (= 0 (length word))
                      (write-string word out)
                      (when (< (length word) width)
                        (write-char #\Space out))
                      (incf out-n (1+ (length word)))))))
           string)
          (flush))
        (nreverse lines))))
