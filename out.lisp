;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/out
  (:use #:cl
        #:constantia/stream/case-translating
        #:constantia/stream/forwarding
        #:constantia/chained-hash-table)
  (:import-from #:constantia/misc
                #:with-options)
  (:import-from #:alexandria
                #:appendf
                #:emptyp
                #:ensure-list)
  (:export
   #:out
   #:outs
   #:define-out-dispatch-table
   #:define-out-op))

(in-package #:constantia/out)


;;;; OUT - A convenient way to print stuff

;; This macro was inspired by Drew McDermott's YTools OUT.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *out-dispatch-table-form*
    '*out-dispatch-table*)
  (defvar *out-dispatch-table*
    (make-instance 'chained-hash-table :test 'eq)))

(defmacro out-stream-passer () 'nil)

(defmacro out-dispatch-table-passer () 'nil)

(defmacro out (&rest args &environment env)

  (when (endp args)
    (return-from out `(values)))

  (let ((stream-form '*standard-output*)
        (stream (gensym))
        (format-string-stream (make-string-output-stream))
        (format-bindings '())
        (format-args '())
        (forms '())
        (to-string nil)
        (case-translation nil)
        (*out-dispatch-table-form* *out-dispatch-table-form*)
        (*out-dispatch-table* *out-dispatch-table*))
    (declare (special case-translation))

    (when (macroexpand '(out-stream-passer) env)
      (setf stream-form (macroexpand '(out-stream-passer) env)))
    (when (macroexpand '(out-dispatch-table-passer) env)
      (setf *out-dispatch-table-form* (macroexpand '(out-dispatch-table-passer) env)))

    (loop
     (cond ((and (consp args) (consp (car args)) (eq (caar args) :to))
            (setf stream-form (cadar args))
            (when (eq stream-form :string)
              (setf stream-form `(make-string-output-stream))
              (setf to-string t))
            (pop args))
           ((and (consp args) (consp (car args)) (eq (caar args) :use))
            (setf *out-dispatch-table-form* (cadar args))
            (pop args))
           (t
            (return))))

    (check-type *out-dispatch-table-form* symbol)
    (setf *out-dispatch-table* (symbol-value *out-dispatch-table-form*))
    (check-type *out-dispatch-table* chained-hash-table)

    (labels ((add-to-format (format-string &optional bindings args)
               (write-string format-string format-string-stream)
               (appendf format-bindings bindings)
               (appendf format-args args))

             (add-to-forms (body)
               (finish-format)
               (appendf forms
                        (list `(macrolet ((out-stream-passer () ',stream)
                                          (out-dispatch-table-passer () ',*out-dispatch-table-form*))
                                 ,@body))))

             (finish-format ()
               (let ((format-string (get-output-stream-string format-string-stream)))
                 (unless (emptyp format-string)
                   (add-to-forms
                    `((let ,format-bindings
                        (format ,stream ,format-string ,@format-args))))
                   (setf format-bindings '())
                   (setf format-args '()))))

             (simple-output (x)
               (add-to-format "~A" '() (list x))))

      (loop for (arg . more-args) on args do
            (typecase arg
              ((cons (member :to :use))
               (add-to-forms `((out ,arg ,@more-args)))
               (return))
              ((cons keyword)
               (destructuring-bind (type &rest data)
                   (apply (out-op (car arg)) stream (cdr arg))
                 (ecase type
                   (:format (apply #'add-to-format data))
                   (:forms (add-to-forms data)))))
              (otherwise
               (simple-output arg))))

      (add-to-forms
       (if to-string
           (if case-translation
               `((get-output-stream-string (forwarding-character-output-stream-stream ,stream)))
               `((get-output-stream-string ,stream)))
           `((values)))))

    (when case-translation
      (setf stream-form `(ensure-case-translating-stream ,stream-form)))

    `(let ((,stream ,stream-form))
       (declare (ignorable ,stream))
       ,@forms)))

(defmacro outs (&rest args)
  `(out (:to :string) ,@args))


;;;; Built-in operators infrastructure

(defmacro define-out-dispatch-table (name &rest options)
  (with-options (&optional parent documentation) options
    (if (null parent)
        (setf parent '*out-dispatch-table*)
        (setf parent (first parent)))
    `(defvar ,name
       (make-instance 'chained-hash-table
                      :parent ,parent)
       ,@documentation)))

(defun out-op (name)
  (or (cgethash name *out-dispatch-table*)
      (error "Unable to find an out operator with the name ~S." name)))

(defmacro define-out-op (name-and-options (&rest args) &body body)
  (destructuring-bind (name &key (table '*out-dispatch-table*))
      (ensure-list name-and-options)
    `(progn
       (setf (cgethash ',name ,table)
             (lambda ,args
               (declare (ignorable ,(car args)))
               ,@body))
       ',name)))

(defun format-data (control-char directive-plist ordering colon at)
  (let ((args '())
        (mapping '())
        (first-time t))
    (values
     (with-output-to-string (*standard-output*)
       (write-char #\~)
       ;; Bindings order is determined by DIRECTIVE-PLIST, and args
       ;; order is determined by ORDERING.  First we determine args,
       ;; and while doing that we set up key<->var-name mapping for
       ;; determining bindings in the correct order.
       (dolist (x ordering)
         (if first-time
             (setf first-time nil)
             (write-char #\,))
         ;; It's OK here to coalesce check for null form and inexistence
         ;; of X in DIRECTIVE-PLIST.
         (let ((form (getf directive-plist x)))
           (cond ((null form))
                 ((integerp form)
                  (princ form))
                 ((characterp form)
                  (write-char #\')
                  (write-char form))
                 (t
                  ;; We got something that needs to be evaluated at runtime.
                  (write-char #\V)
                  (let ((var (gensym)))
                    (push var args)
                    (push (cons x var) mapping))))))
       (when colon (write-char #\:))
       (when at (write-char #\@))
       (write-char control-char))
     (loop for (key form) on directive-plist by #'cddr
           for entry = (assoc key mapping)
           when entry collect (list (cdr entry) form))
     (nreverse args))))

(defun format-op (control-char directive-plist ordered-keys &key colon at (required nil required-supplied))
  (multiple-value-bind (string bindings args)
      (format-data control-char directive-plist ordered-keys colon at)
    (when required-supplied
      ;; Add binding and arg for the required form.  Binding comes
      ;; before other bindings, and arg comes after other args.
      (if (constantp required)
          (setf args (append args (list required)))
          (let ((var (gensym)))
            (push (list var required) bindings)
            (setf args (append args (list var))))))
    `(:format ,string ,bindings ,args)))


;;;; Operators

;; Note that some of the arguments will be evaluated at runtime and
;; others are used in macroexpansion time.  For example, the colon/at
;; modifiers for FORMAT don't have something like using V for prefix
;; parameters, and no, I don't want recursive processing.

(define-out-op :% (stream &optional times)
  (format-op #\% (list :times times) '(:times)))

(define-out-op :f (stream x &rest args &key mode width digits-after-point scale-factor overflow-char pad-char plus-sign digits-of-exponent exponent-char)
  (declare (ignore width digits-after-point scale-factor overflow-char pad-char))
  (when (null mode)
    (setf mode
          (if (and (null digits-of-exponent) (null exponent-char))
              :fixed
              :exponential)))
  (ecase mode
    (:fixed
     (assert (null digits-of-exponent))
     (assert (null exponent-char))
     (format-op #\F args '(:width :digits-after-point :scale-factor :overflow-char :pad-char)
                :colon nil :at plus-sign :required x))
    (:exponential
     (format-op #\E args '(:width :digits-after-point :digits-of-exponent :scale-factor :overflow-char :pad-char :exponent-char)
                :colon nil :at plus-sign :required x))))

(define-out-op :d (stream x &rest args &key width pad-char comma-char comma-interval commas sign base)
  (declare (ignore width pad-char comma-char comma-interval))
  (assert (constantp commas))
  (assert (constantp sign))
  (when (null base)
    (setf args (append '(:base 10) args)))
  (format-op #\R args '(:base :width :pad-char :comma-char :comma-interval) :colon commas :at sign :required x))

(define-out-op :r (stream x &optional (mode :cardinal))
  (multiple-value-bind (colon at)
      (ecase mode
        (:cardinal (values nil nil))
        (:ordinal (values t nil))
        (:roman (values nil t))
        (:old-roman (values t t)))
    (format-op #\R '() '() :colon colon :at at :required x)))

(define-out-op :& (stream &optional times)
  (format-op #\& (list :times times) '(:times)))

(define-out-op :c (stream x &optional mode)
  (multiple-value-bind (colon at)
      (ecase mode
        ((nil) (values nil nil))
        (:pretty (values t nil))
        (:readable (values nil t))
        (:pretty+ (values t t)))
    (format-op #\C '() '() :colon colon :at at :required x)))

(define-out-op :a (stream x &rest args &key escape as-list (align :left) width column-increment min-pad pad-char)
  (declare (ignore width column-increment min-pad pad-char))
  (assert (constantp escape))
  (assert (constantp as-list))
  (check-type align (member :left :right))
  (format-op (if escape #\S #\A) args '(:width :column-increment :min-pad :pad-char)
             :colon as-list :at (eq align :right) :required x))

(defun out-seq (stream sequence prefix suffix separator start end key)
  (unless separator (setf separator (if (or prefix suffix) "" #\Space)))
  (unless key (setf key #'identity))
  (etypecase sequence
    (list
     (loop for first-time = t then nil
           for index from (or start 0) below (or end most-positive-fixnum)
           for x in (nthcdr (or start 0) sequence) do
           (unless first-time
             (out (:to stream) separator))
           (when prefix
             (out (:to stream) prefix))
           (out (:to stream) (funcall key x))
           (when suffix
             (out (:to stream) suffix))))
    (vector
     (loop for first-time = t then nil
           for index from (or start 0) below (or end (length sequence))
           for x = (aref sequence index) do
           (unless first-time
             (out (:to stream) separator))
           (when prefix
             (out (:to stream) prefix))
           (out (:to stream) (funcall key x))
           (when suffix
             (out (:to stream) suffix))))))

(define-out-op :s (stream sequence &key prefix suffix separator start end key)
  `(:forms
    (out-seq ,stream ,sequence ,prefix ,suffix ,separator ,start ,end ,key)))

;; This construct is useful, but kinda reinvents COND.  McDermott's
;; OUT has something like it, too.  Note that if no consequent is
;; provided, nothing is output if the test for it passes (COND
;; returns the value(s) of the test expression).

(define-out-op :q (stream &rest clauses)
  `(:forms
    (cond
      ,@(loop for (test . consequent) in clauses
              collect `(,test (out ,@consequent))))))

;; Note that we still don't support the behavior of ~@(, which
;; "capitalizes just the first word and forces the rest to lower
;; case."

;; Unlike FORMAT, we provide proper nesting semantics for case
;; translation (the "outer conversion" does not dominate).

(macrolet ((define-case-op (name case)
             `(define-out-op ,name (stream &rest subforms)
                (declare (special case-translation))
                (setf case-translation t)
                `(:forms (with-stream-case (,stream ,',case)
                           (out (:to ,stream) ,@subforms))))))
  (define-case-op :dc :downcase)
  (define-case-op :uc :upcase)
  (define-case-op :cc :capitalize)
  (define-case-op :pc :preserve))

(define-out-op :h (stream hash-table k/v &rest subforms)
  (destructuring-bind (k v &key separator) k/v
    (let ((first (gensym))
          (separator-value (gensym)))
      `(:forms (let ((,first t)
                     (,separator-value (or ,separator #\Space)))
                 (maphash (lambda (,k ,v)
                            (declare (ignorable ,k ,v))
                            (cond (,first (setf ,first nil))
                                  (,separator-value (out ,separator-value)))
                            (out ,@subforms))
                          ,hash-table))))))

(define-out-op :n (stream n &rest subforms)
  `(:forms (loop repeat ,n do (out ,@subforms))))

;; This construct resembles the one in McDermott's OUT, except that we
;; use a local macro OUT to "return" to OUT mode.

(define-out-op :e (stream &rest forms)
  `(:forms ,@forms))

;; This construct flushes the stream.

(define-out-op :fo (stream &optional (wait nil))
  (case wait
    ((nil)
     `(:forms (force-output ,stream)))
    ((t)
     `(:forms (finish-output ,stream)))
    (t
     `(:forms (if ,wait
                  (finish-output ,stream)
                  (force-output ,stream))))))
