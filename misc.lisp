;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/misc
  (:use #:cl)
  (:import-from #:alexandria #:once-only #:with-gensyms #:if-let)
  (:export
   #:u8
   #:s8
   #:u16
   #:s16
   #:u32
   #:s32
   #:u64
   #:s64
   #:defsubst
   #:type-eq
   #:square
   #:abs-
   #:singlep
   #:in-range-p
   #:as-keyword
   #:slurp-file
   #:map-lines-in-file
   #:default
   #:gethash/i
   #:doseq
   #:best-element
   #:as-list
   #:as-vector
   #:make-octet-vector
   #:concat-octet-vectors
   #:plist-get
   #:agetf
   #:with-options
   #:kahan
   #:make-kahan
   #:kahan-p
   #:copy-kahan
   #:kahan-sum
   #:kahan-c
   #:k+
   #:float-sum
   #:moment
   #:moment-mean
   #:moment-adev
   #:moment-sdev
   #:moment-svar
   #:moment-skew
   #:moment-kurt
   #:make-queue
   #:queue-push
   #:queue-top
   #:queue-pop
   #:queue-empty-p
   #:queue-size
   #:queue-clear
   #:call-every-n
   #:call-every-ms
   #:add-hook
   #:run-hook
   #:remove-hook))

(in-package #:constantia/misc)


;;;; Miscellaneous utilities

(deftype u8 () '(unsigned-byte 8))
(deftype s8 () '(signed-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype s16 () '(signed-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype s32 () '(signed-byte 32))
(deftype u64 () '(unsigned-byte 64))
(deftype s64 () '(signed-byte 64))

(defmacro defsubst (name lambda-list &body forms)
  "Define an inline function at top level."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@forms)))

(defmacro default (place form &environment env)
  "Set up a default value for an optional or key parameter.  If PLACE
evaluates to nil, evaluate FORM and assign the result to PLACE,
otherwise do nothing.  Use of this macro is problematic when the
parameter is boolean."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((store-var (first store-vars)))
      `(let* (,@(mapcar #'list vars vals)
              (,store-var ,reader-form))
         (unless ,store-var
           (setq ,store-var ,form)
           ,writer-form)))))

(defsubst type-eq (type1 type2)
  "Return true if TYPE1 and TYPE2 are type equivalent; return false
otherwise."
  (and (subtypep type1 type2)
       (subtypep type2 type1)))

(defsubst square (x)
  "Return the square of number X."
  (* x x))

(defsubst abs- (a b)
  "Return the absolute difference of numbers A and B."
  (abs (- a b)))

(defsubst singlep (list)
  "Return true if LIST is a list consisting of a single element;
return false otherwise."
  (and (consp list)
       (endp (rest list))))

(defsubst random-in-range (min max &optional state)
  "Return a random integer in the range [MIN..MAX]."
  (default state *random-state*)
  (+ min (random (1+ (- max min)) state)))

(defsubst in-range-p (x start end)
  "Return true if X is within the range [START..END), and false
otherwise."
  (and (>= x start) (< x end)))

(defun as-keyword (symbol)
  "Return a symbol with the same name, but in the KEYWORD package."
  (values
   (intern (symbol-name symbol) (load-time-value (find-package "KEYWORD")))))

(defun slurp-file (filename)
  "Read the whole file denoted by FILENAME and return its contents as
an octet vector."
  (with-open-file (in filename :direction :input :element-type 'u8)
    (let ((buffer (make-array (file-length in) :element-type 'u8)))
      (read-sequence buffer in)
      buffer)))

(defun map-lines-in-file (function filename)
  "Call FUNCTION with each of the lines in the file denoted by
FILENAME and return all non-NIL values returned by it."
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          when (funcall function line)
          collect it)))

(defmacro gethash/i (key hash-table &optional default-form)
  "Get value corresponding to KEY, setting one if needed.

If the hash-table contains an association for key, return it.
Otherwise, evaluate the default-form, associate the key with the
resulting value, and return it."
  (once-only (key hash-table)
    (with-gensyms (value present)
      `(multiple-value-bind (,value ,present)
           (gethash ,key ,hash-table)
         (if ,present
             ,value
             (values (setf (gethash ,key ,hash-table) ,default-form)))))))

(defun map-sequence (function sequence start end)
  (default start 0)
  (etypecase sequence
    (list
     (let ((n 0))
       (dolist (x sequence)
         (cond ((and end (>= n end))
                (return-from map-sequence))
               ((>= n start)
                (funcall function x)))
         (incf n))))
    (vector
     (default end (length sequence))
     (loop for n from start below end
           do (funcall function (aref sequence n))))))

(defmacro doseq ((var sequence &key start end) &body forms)
  `(map-sequence (lambda (,var) ,@forms) ,sequence ,start ,end))

(defun best-element (sequence &key key better-p start end)
  "Return the best element in a sequence and its key, or score.  If a
BETTER-P function is not supplied, then the best element is the one
with the minimum score \(minimum because the score is usually an error
from some reference value)."
  (default key #'identity)
  (default better-p #'<)
  (let ((best-score nil)
        (best-element nil))
    (doseq (x sequence :start start :end end)
      (let ((score (funcall key x)))
        (when (or (null best-score) (funcall better-p score best-score))
          (setf best-score score)
          (setf best-element x))))
    (values best-element best-score)))

(defun as-list (sequence)
  "Return a list representation of SEQUENCE."
  (coerce sequence 'list))

(defun as-vector (sequence)
  "Return a vector representation of SEQUENCE."
  (coerce sequence 'vector))

(defun make-octet-vector (size &rest array-options)
  "Return a fresh vector whose element type is (unsigned-byte 8)."
  (apply #'make-array size :element-type 'u8 array-options))

(defun concat-octet-vectors (&rest vectors)
  "Return the concatenation of a bunch of octet vectors into one."
  (let* ((total-length (reduce #'+ vectors :key #'length))
         (result (make-octet-vector total-length))
         (offset 0))
    (dolist (vector vectors)
      (replace result vector :start1 offset :end1 (+ offset (length vector)))
      (incf offset (length vector)))
    result))

(defun plist-get (key plist)
  "Return the value corresponding to KEY in the PLIST, or NIL if
absent.  Keys are compared using EQUAL."
  (cond ((null plist) nil)
        ((equal (car plist) key) (cadr plist))
        (t (plist-get key (cddr plist)))))


;;;; AGETF - GETF for association lists

(defun agetf (alist key &optional default)
  "Like CL:GETF, but for association lists."
  (if-let (entry (assoc key alist))
    (cdr entry)
    default))

(defun aputf (place key new-value)
  ;; Analogous to SBCL's %putf
  (do ((alist place (cdr alist)))
      ((endp alist) (acons key new-value place))
    (declare (type list alist))
    (when (eq (caar alist) key)
      (setf (cdar alist) new-value)
      (return place))))

(define-setf-expander agetf (place key &optional default &environment env)
  "SETF expander for AGETF, which is like GETF, but for association
lists.  Copied almost verbatim from SBCL's implementation of GETF
expander."
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion place env)
    (let ((newval (gensym))
          (ptemp (gensym))
          (def-temp (if default (gensym))))
      (values `(,@temps ,ptemp ,@(if default `(,def-temp)))
              `(,@values ,key ,@(if default `(,default)))
              `(,newval)
              `(let ((,(car stores) (aputf ,get ,ptemp ,newval)))
                 ,set
                 ,newval)
              `(agetf ,get ,ptemp ,@(if default `(,def-temp)))))))


;;;; Declarative-style macros options

(defun missing-option (name)
  (error "Option ~S is required, but was not provided." name))

(defmacro with-options ((&rest names) options-list &body body)
  (let* ((optional-names (member '&optional names))
         (required-names (ldiff names optional-names)))
    (pop optional-names)
    (once-only (options-list)
      `(let ,(loop for name in (append required-names optional-names)
                   collect `(,name (assoc ,(as-keyword name) ,options-list :test #'eq)))
        ,@(loop for name in required-names
                collect `(if ,name
                          (pop ,name)
                          (missing-option ',name)))
        ,@(loop for name in optional-names
                collect `(when ,name
                          (pop ,name)))
        ,@body))))


;;;; Statistics

(defstruct kahan
  (sum 0.0)
  (c 0.0))

(defun k+ (kahan x)
  "Add X to the Kahan sum."
  (let* ((y (- x (kahan-c kahan)))
         (z (+ (kahan-sum kahan) y))
         (c (- (- z (kahan-sum kahan)) y)))
    (setf (kahan-c kahan) c)
    (setf (kahan-sum kahan) z)
    kahan))

(defun float-sum (sequence &key key start end)
  "Return the floating-point sum of elements in SEQUENCE."
  (default start 0)
  (kahan-sum
   (reduce #'k+ sequence :initial-value (make-kahan)
           :key key :start start :end end)))

(defclass moment ()
  ((mean :initarg :mean :accessor moment-mean :documentation "Mean average")
   (adev :initarg :adev :accessor moment-adev :documentation "Average deviation")
   (sdev :initarg :sdev :accessor moment-sdev :documentation "Standard deviation")
   (svar :initarg :svar :accessor moment-svar :documentation "Variance")
   (skew :initarg :skew :accessor moment-skew :documentation "Skewness")
   (kurt :initarg :kurt :accessor moment-kurt :documentation "Kurtosis")))

(defmethod print-object ((moment moment) stream)
  (print-unreadable-object (moment stream :type t)
    (format stream "~F (~F)" (moment-mean moment) (moment-sdev moment)))
  moment)

(defun moment (data &key key start end (sample t))
  (default start 0)
  (default end (length data))
  (default key #'identity)
  (let ((n (- end start)))
    (when (< n 2)
      (error "Data for moment must consist of at least 2 elements."))
    (let* ((s (float-sum data :key key :start start :end end))
           (ave (/ s n))
           (adev (make-kahan))
           (svar (make-kahan))
           (skew (make-kahan))
           (curt (make-kahan))
           (sdev 0.0))
      (doseq (x data :start start :end end)
        (let* ((d1 (- (funcall key x) ave))
               (d2 (* d1 d1))
               (d3 (* d1 d2))
               (d4 (* d1 d3)))
          (k+ adev (abs d1))
          (k+ svar d2)
          (k+ skew d3)
          (k+ curt d4)))
      (setf adev (/ (kahan-sum adev) n))
      (setf svar (/ (kahan-sum svar) (if sample (- n 1) n)))
      (setf sdev (sqrt svar))
      (cond ((zerop svar)
             (setf skew nil)
             (setf curt nil))
            (t
             (setf skew (/ (kahan-sum skew) (* n sdev sdev sdev)))
             (setf curt (- (/ (kahan-sum curt) (* n svar svar)) 3.0))))
      (make-instance 'moment :mean ave :adev adev :sdev sdev :svar svar :skew skew :kurt curt))))


;;;; Queue

(defun make-queue ()
  "Return a fresh queue object."
  (cons nil nil))

(defun queue-head (q)
  "Return the list of queued objects, positioned at the head of the
queue."
  (car q))

(defun (setf queue-head) (x q)
  (setf (car q) x))

(defun queue-tail (q)
  "Return the list of queued objects, positioned at the tail of the
queue."
  (cdr q))

(defun (setf queue-tail) (x q)
  (setf (cdr q) x))

(defun queue-push (x q)
  "Add X to the tail of Q."
  (cond ((queue-empty-p q)
         (setf (queue-head q) (setf (queue-tail q) (cons x nil))))
        (t
         (setf (cdr (queue-tail q)) (cons x nil))
         (pop (queue-tail q))))
  (values))

(defun queue-top (q)
  "Return the first object in Q."
  (car (queue-head q)))

(defun queue-pop (q)
  "Remove an object from the head of Q and return it."
  (unless (queue-empty-p q)
    (prog1 (car (queue-head q))
      (when (eq (queue-head q) (queue-tail q))
        (setf (queue-tail q) nil))
      (pop (queue-head q)))))

(defun queue-empty-p (q)
  "Return true if Q is empty, and false otherwise."
  (null (queue-head q)))

(defun queue-size (q)
  "Return the number of objects in Q."
  (length (queue-head q)))

(defun queue-clear (q)
  "Clear contents of Q."
  (setf (queue-head q) nil)
  (setf (queue-tail q) nil))


;;;; Functions useful for progress reporting

;; Found in Xach's cloudfront-log-processor system.
(defun call-every-n (function n)
  "Return a function to call FUNCTION and return its result once every
N calls, and just return NIL otherwise."
  (let ((counter 0))
    (lambda ()
      (when (<= n (incf counter))
        (setf counter 0)
        (funcall function)))))

(defun call-every-ms (function interval)
  "Return a function to call FUNCTION and return its result once every
INTERVAL milliseconds, and just return NIL otherwise."
  (let ((last-called nil)
        (interval-internal (ceiling (* (/ interval 1000) internal-time-units-per-second))))
    (lambda ()
      (let ((now (get-internal-real-time)))
        (when (or (null last-called)
                  (>= (- now last-called) interval-internal))
          (setf last-called now)
          (funcall function))))))


;;;; Simple hook management

(defun add-hook (hook function-designator &key (append nil))
  "Add the function designator to the hook.  If there is an EQUAL
function designator already, does nothing.  If APPEND is true, adds to
the end of the list; otherwise, adds to the beginning of the list."
  (let ((list (symbol-value hook)))
    (unless (member function-designator list :test #'equal)
      (set hook
           (if append
               (append list (list function-designator))
               (cons function-designator list)))))
  (values))

(defun run-hook (hook &rest args)
  "Run a hook, passing arguments to each of the hook functions."
  (let ((list (copy-list (symbol-value hook))))
    (dolist (function list)
      (with-simple-restart (continue "Continue running hook")
        (apply function args))))
  (values))

(defun remove-hook (hook function-designator)
  "Remove the function designator from the hook, if there is one."
  (let ((list (symbol-value hook)))
    (set hook (remove function-designator list :count 1 :test #'equal))
    (values)))
