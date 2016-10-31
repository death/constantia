;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/with-progress-indicator
  (:use #:cl)
  (:import-from #:alexandria #:with-gensyms)
  (:export
   #:with-progress-indicator
   #:conditional-caller))

(in-package #:constantia/with-progress-indicator)

(defmacro with-progress-indicator ((progress form when &rest when-args) &body forms)
  "Evaluate FORMS within the context of a function named PROGRESS
that, when called, evaluates FORM given the conditions specified by
WHEN, a conditional caller designator, and WHEN-ARGS, the arguments to
pass to the conditional caller.

For example:

  (with-progress-indicator (progress (out #\. (:fo)) :every-ms 200)
    (loop until (work-done-p)
          do (progress)
             (some-work)))

Should write a dot once every 200 milliseconds if possible."
  (with-gensyms (progress-fn)
    `(let ((,progress-fn (funcall (conditional-caller ,when)
                                  (lambda () ,form)
                                  ,@when-args)))
       (flet ((,progress () (funcall ,progress-fn)))
         ,@forms))))

;; A /conditional caller/ is a function that takes a function and
;; returns a function that, when called, delegates if certain
;; conditions are satisfied.

(defgeneric conditional-caller (designator)
  (:documentation "Return the conditional caller appropriate for
DESIGNATOR.")
  (:method ((designator function)) designator))

(defmethod documentation (x (doc-type (eql 'conditional-caller)))
  (documentation (conditional-caller x) 'function))

;; :EVERY-MS conditional caller

(defun call-every-ms (function interval)
  "Call FUNCTION (at most) once every INTERVAL milliseconds."
  (let ((last-called nil)
        (interval-internal (ceiling (* (/ interval 1000) internal-time-units-per-second))))
    (lambda ()
      (let ((now (get-internal-real-time)))
        (when (or (null last-called)
                  (>= (- now last-called) interval-internal))
          (setf last-called now)
          (funcall function))))))

(defmethod conditional-caller ((designator (eql :every-ms))) #'call-every-ms)

;; :EVERY-N conditional caller

;; This design of WITH-PROGRESS-INDICATOR was actually inspired by
;; this little function, found in Xach's cloudfront-log-processor
;; system.

(defun call-every-n (function n)
  "Call FUNCTION once every N calls."
  (let ((counter 0))
    (lambda ()
      (when (<= n (incf counter))
        (funcall function)
        (setf counter 0)))))

(defmethod conditional-caller ((designator (eql :every-n))) #'call-every-n)
