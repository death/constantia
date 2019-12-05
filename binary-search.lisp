;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/binary-search
  (:documentation
   "Binary search and related operators for vectors.")
  (:use #:cl)
  (:export
   #:binary-search
   #:lower-bound
   #:upper-bound))

(in-package #:constantia/binary-search)

(defun ensure-function (designator &optional default)
  "Return the function designated by the designator.

If it is NIL, return the default; if it is a symbol, return the
function in the symbol's function cell; if it is a function, return it
as-is, and otherwise signal an error."
  (cond ((null designator)
         default)
        ((symbolp designator)
         (symbol-function designator))
        ((functionp designator)
         designator)
        (t
         (error "~S is not a function designator." designator))))

(defun binary-search (item vector &key (start 0) end key less)
  "Search for item in a sorted vector.

If found, return the element and its position in the vector.
Otherwise, return two values of NIL."
  (setf key (ensure-function key #'identity))
  (setf less (ensure-function less #'<))
  (setf end (or end (length vector)))
  (let ((pos (lower-bound item vector :start start :end end :key key :less less)))
    (cond ((= pos end)
           (values nil nil))
          ((funcall less item (funcall key (aref vector pos)))
           (values nil nil))
          (t
           (values (aref vector pos) pos)))))

;; The implementation for these algorithms was modeled after the one
;; presented in the cppreference.com page for std::lower_bound and
;; std::upper_bound.

(defun lower-bound (item vector &key (start 0) end key less)
  "Return the position of the first element in the sorted vector that
is greater than or equal to item.

If no such element exists, return the end position."
  (setf key (ensure-function key #'identity))
  (setf less (ensure-function less #'<))
  (setf end (or end (length vector)))
  (loop with count = (- end start)
        while (plusp count)
        for step = (truncate count 2)
        for it = (+ start step)
        do (cond ((funcall less (funcall key (aref vector it)) item)
                  (setf start (1+ it))
                  (decf count (1+ step)))
                 (t
                  (setf count step))))
  start)

(defun upper-bound (item vector &key (start 0) end key less)
  "Return the position of the first element in the sorted vector that
is greater than item.

If no such element exists, return the end position."
  (setf key (ensure-function key #'identity))
  (setf less (ensure-function less #'<))
  (setf end (or end (length vector)))
  (loop with count = (- end start)
        while (plusp count)
        for step = (truncate count 2)
        for it = (+ start step)
        do (cond ((not (funcall less item (funcall key (aref vector it))))
                  (setf start (1+ it))
                  (decf count (1+ step)))
                 (t
                  (setf count step))))
  start)
