;;;; +----------------------------------------------------------------+
;;;; | Constantia                                                     |
;;;; +----------------------------------------------------------------+

(defpackage #:constantia/event
  (:use #:cl #:constantia/misc)
  (:import-from #:alexandria
                #:ensure-list
                #:deletef)
  (:import-from #:bordeaux-threads
                #:make-recursive-lock
                #:with-lock-held)
  (:export
   #:event
   #:speaker
   #:define-event
   #:define-events
   #:add-listener
   #:remove-listener
   #:clear-listeners
   #:fire
   #:make-tracing-listener))

(in-package #:constantia/event)


;;;; Event system

(defclass event ()
  ()
  (:documentation
   "The base class of all events.

Events can be defined using DEFINE-EVENT or DEFINE-EVENTS.  It is
sometimes useful to directly use DEFCLASS to define them, e.g. for
subclassing purposes.

Events are fired using FIRE by speaker objects, and are received by
listeners.

Listeners are functions taking one parameter, the event."))

(defmacro define-event (name-and-options &rest slot-names)
  "Define a new event class.  The first element of SLOT-NAMES may
actually be a docstring for the class, in which case the slot names
are the rest of the list.

Syntax: define-event name-and-options [documentation] {slot-name}*

name-and-options::= event-name | (event-name [[options]])

options::= (:super {superclass-name}*)"
  (destructuring-bind (name &rest options) (ensure-list name-and-options)
    (with-options (&optional super) options
      (when (null super) (setf super '(event)))
      (flet ((initarg (slot-name) (as-keyword slot-name))
             (accessor (slot-name) slot-name))
        (let ((doc-string (when (stringp (first slot-names))
                            (pop slot-names))))
          `(defclass ,name ,super
             ,(loop for slot-name in slot-names
                    collect `(,slot-name
                              :initarg ,(initarg slot-name)
                              :reader ,(accessor slot-name)))
             ,@(when doc-string `((:documentation ,doc-string)))))))))

(defmacro define-events (&body event-specs)
  "Define a number of event classes.  An event class specification is
a list of the form \(name . slot-names)}, i.e. a list that
DEFINE-EVENT can be applied to.  The first argument may be a list of
options to append to each event's options."
  (let ((options (when (or (null (car event-specs))
                           (and (consp (car event-specs))
                                (consp (caar event-specs))))
                   (pop event-specs))))
    `(progn
       ,@(loop for event-spec in event-specs
               for name-and-options = (append (ensure-list (first event-spec)) options)
               for slot-names = (rest event-spec)
               collect `(define-event ,name-and-options ,@slot-names)))))

(defmethod documentation ((x symbol) (doc-type (eql 'event)))
  (documentation (find-class x) 't))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'event)))
  (setf (documentation (find-class x) 't) new-value))

(defclass speaker ()
  ((listeners :initform '())
   (listeners-lock :initform (make-recursive-lock)))
  (:documentation
   "The base class for all speakers.

A speaker maintains a list of listeners, which it can notify \(using
FIRE) about certain events (objects of class EVENT).

Listeners can be added to the speaker's list using ADD-LISTENER, and
removed from the speaker's list using REMOVE-LISTENER or
CLEAR-LISTENERS."))

(defmacro with-listeners ((var speaker) &body forms)
  (let ((lock (gensym)))
    `(with-slots ((,var listeners) (,lock listeners-lock)) ,speaker
       (with-lock-held (,lock)
         ,@forms))))

(defun add-listener (listener &rest speakers)
  "Add LISTENER to the list of listeners of each speaker in SPEAKERS.
A listener is only added once per speaker.  This function is
thread-safe."
  (dolist (speaker speakers)
    (with-listeners (listeners speaker)
      (pushnew listener listeners :test #'eq)))
  (values))

(defun remove-listener (listener &rest speakers)
  "Remove LISTENER from the list of listeners of each speaker in
SPEAKERS.  This function is thread-safe."
  (dolist (speaker speakers)
    (with-listeners (listeners speaker)
      (deletef listeners listener :test #'eq)))
  (values))

(defun clear-listeners (&rest speakers)
  "Remove all listeners from each speaker in SPEAKERS.  This function
is thread-safe."
  (dolist (speaker speakers)
    (with-listeners (listeners speaker)
      (setf listeners '())))
  (values))

(defun fire (speaker datum &rest arguments)
  "Fire an event to all listeners of SPEAKER.  If DATUM is an event
object, then it will be the event fired.  If it is the name of an
event class, MAKE-INSTANCE will be called to create an event of this
type, and ARGUMENTS will be passed as initargs.  This function is
thread-safe."
  (unless (typep datum 'event)
    (setf datum (apply #'make-instance datum arguments)))
  (dolist (listener (listeners-list speaker))
    (funcall listener datum))
  (values))

(define-compiler-macro fire (&whole form speaker datum &rest arguments &environment env)
  (if (and (constantp datum env) (not (typep datum 'event)))
      `(fire ,speaker (make-instance ,datum ,@arguments))
      form))

(defun listeners-list (speaker)
  (with-listeners (listeners speaker)
    (copy-list listeners)))


;;;; Tracing listener

(defun make-tracing-listener (&key output (describe nil))
  "Return a listener that dumps information about any event received."
  (default output *standard-output*)
  (lambda (event)
    (format output "~&E: ~S~%" (type-of event))
    (when describe
      (describe event output))))
