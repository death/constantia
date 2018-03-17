;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/print-table
  (:use #:cl #:constantia/wrap-string)
  (:export
   #:print-table))

(in-package #:constantia/print-table)

(defstruct column
  name
  alignment
  key
  width
  wrap)

;; NAME is a string.
;; ALIGNMENT is one of :LEFT (default), :RIGHT, :CENTER.
;; KEY is a function designator (identity by default).
;; WIDTH is one of :AUTO (default), (:FIXED <n>/:HEADING), (:MAX <n>)
;; WRAP is one of :CUT (default), :ELLIPSIS, :WORD, :CHARACTER

(defun parse-column-spec (spec)
  (etypecase spec
    (string (parse-column-spec (list spec)))
    (cons (destructuring-bind (name &key align key width wrap) spec
            (make-column
             :name name
             :alignment (or align :left)
             :key (or key #'identity)
             :width (or width :auto)
             :wrap (or wrap :cut))))
    (column spec)))

(defun print-table (column-specs rows &key (stream *standard-output*) (condensed t))
  (let* ((columns (mapcar #'parse-column-spec column-specs))
         (column-widths (column-widths columns rows)))
    (labels ((sep ()
               (write-char #\+ stream)
               (dolist (width column-widths)
                 (loop repeat (+ width 2) doing (write-char #\- stream))
                 (write-char #\+ stream))
               (write-char #\Newline stream))
             (row (row kind)
               (let* ((cells (mapcar (lambda (c w column)
                                       (let ((s (if (eq kind :headings)
                                                    (frugal-princ-to-string c)
                                                    (format-column-object c column))))
                                         (wrap-string s w (column-wrap column))))
                                     row column-widths columns))
                      (max-lines (reduce #'max cells :key #'length)))
                 (dotimes (i max-lines)
                   (row-line (mapcar (lambda (c) (nth i c)) cells) kind))
                 (unless (and condensed (= max-lines 1) (eq kind :normal))
                   (sep))))
             (row-line (parts kind)
               (write-char #\| stream)
               (loop for s in parts
                     for column in columns
                     for w in column-widths
                     for alignment = (if (eq kind :headings)
                                         :center
                                         (column-alignment column))
                     do (write-char #\Space stream)
                     do (print-aligned (or s "") w alignment stream)
                     do (write-char #\Space stream)
                     do (write-char #\| stream))
               (write-char #\Newline stream)))
      (sep)
      (row (mapcar #'column-name columns) :headings)
      (loop for (row . more) on rows
            do (row row (if more :normal :last)))
      (values))))

(defun column-widths (columns rows)
  (let ((widths (make-array (length columns))))
    ;; Initial column widths are determined by column names.
    (loop for i from 0
          for column in columns
          for width = (column-width column)
          do (setf (aref widths i)
                   (cond ((eq :auto width)
                          (length (column-name column)))
                         ((and (consp width) (eq (first width) :fixed))
                          (if (eq (second width) :heading)
                              (length (column-name column))
                              (second width)))
                         ((and (consp width) (eq (first width) :max))
                          (length (column-name column)))
                         (t (error "Bad column width: ~S." width)))))
    ;; The column widths are then adjusted for row data.
    (dolist (r rows)
      (loop for i from 0
            for c in r
            for column in columns
            for width = (column-width column)
            do (cond ((eq width :auto)
                      (let ((s (format-column-object c column)))
                        (setf (aref widths i)
                              (max (aref widths i) (length s)))))
                     ((and (consp width) (eq (first width) :max))
                      (let ((s (format-column-object c column)))
                        (setf (aref widths i)
                              (min (second width)
                                   (max (aref widths i) (length s)))))))))
    (loop for w across widths collecting w)))

(defun print-aligned (string width alignment stream)
  (let ((end (min width (length string))))
    (multiple-value-bind (before after)
        (compute-spaces width end alignment)
      (loop repeat before do (write-char #\Space stream))
      (write-string string stream :end end)
      (loop repeat after do (write-char #\Space stream)))))

(defun compute-spaces (width end alignment)
  (ecase alignment
    (:left
     (values 0 (- width end)))
    (:right
     (values (- width end) 0))
    (:center
     (multiple-value-bind (a b) (floor (- width end) 2)
       (values (+ a b) a)))))

(defun format-column-object (object column)
  (frugal-princ-to-string
   (funcall (column-key column) object)))

(defun frugal-princ-to-string (object)
  (if (stringp object)
      object
      (princ-to-string object)))
