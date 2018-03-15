;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/chained-hash-table
  (:documentation
   "A hierarchical hash-table that can have a single parent.")
  (:use #:cl)
  (:import-from #:alexandria
                #:ensure-functionf)
  (:export
   #:chained-hash-table
   #:chash-table-p
   #:chash-table-parent
   #:chash-table-test
   #:chash-table-count
   #:cgethash
   #:cremhash
   #:cclrhash
   #:cmaphash))

(in-package #:constantia/chained-hash-table)

(defclass chained-hash-table ()
  ((parent :initarg :parent :reader chash-table-parent)
   (contents :initarg :contents :reader chash-table-contents))
  (:default-initargs :parent nil :contents nil))

(defmethod initialize-instance :after ((hash-table chained-hash-table) &key (test :parent))
  (with-slots (contents) hash-table
    (when (null contents)
      (let ((test (if (eq test :parent)
                      (if (chash-table-parent hash-table)
                          (chash-table-test (chash-table-parent hash-table))
                          'eql)
                      test)))
        (setf contents (make-hash-table :test test))))))

(defun chash-table-p (object)
  (typep object 'chained-hash-table))

(defun chash-table-test (hash-table)
  (check-type hash-table chained-hash-table)
  (hash-table-test (chash-table-contents hash-table)))

(defun chash-table-count (hash-table &key (mode :shallow))
  (check-type mode (member :deep :shallow))
  (check-type hash-table chained-hash-table)
  (+ (hash-table-count (chash-table-contents hash-table))
     (if (and (eq mode :deep)
              (chash-table-parent hash-table))
         (chash-table-count (chash-table-parent hash-table) :mode :deep)
         0)))

(defun cgethash (key hash-table &optional default)
  (check-type hash-table chained-hash-table)
  (multiple-value-bind (value exists)
      (gethash key (chash-table-contents hash-table))
    (cond (exists
           (values value exists t))
          ((chash-table-parent hash-table)
           (multiple-value-bind (value exists)
               (cgethash key (chash-table-parent hash-table) default)
             (values value exists nil)))
          (t
           (values default nil nil)))))

(defun (setf cgethash) (new-value key hash-table &optional default)
  (declare (ignore default))
  (check-type hash-table chained-hash-table)
  (setf (gethash key (chash-table-contents hash-table)) new-value))

(defun cremhash (key hash-table &key (mode :shallow))
  (check-type mode (member :deep :shallow))
  (check-type hash-table chained-hash-table)
  (let ((result (remhash key (chash-table-contents hash-table))))
    (values
     (if (and (eq mode :deep) (chash-table-parent hash-table))
         (or (cremhash key (chash-table-parent hash-table) :mode :deep)
             result)
         result)
     (if result t nil))))

(defun cclrhash (hash-table &key (mode :shallow))
  (check-type mode (member :deep :shallow))
  (check-type hash-table chained-hash-table)
  (clrhash (chash-table-contents hash-table))
  (when (and (eq mode :deep)
             (chash-table-parent hash-table))
    (cclrhash (chash-table-parent hash-table) :mode :deep))
  hash-table)

(defun cmaphash (function-designator hash-table &key (mode :shallow))
  (check-type mode (member :deep :shallow))
  (check-type hash-table chained-hash-table)
  (ensure-functionf function-designator)
  (maphash function-designator (chash-table-contents hash-table))
  (when (and (eq mode :deep)
             (chash-table-parent hash-table))
    (cmaphash function-designator (chash-table-parent hash-table)))
  nil)
