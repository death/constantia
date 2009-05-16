;;;; +----------------------------------------------------------------+
;;;; | Constantia                                         DEATH, 2009 |
;;;; +----------------------------------------------------------------+

(in-package #:constantia)


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

In order to do something useful with the events, the listeners have to
add their own specialized methods to ON-EVENT."))

(defmacro define-event (name &rest slot-names)
  "Define a new event class.  The first element of SLOT-NAMES may
actually be a docstring for the class, in which case the slot names
are the rest of the list."
  (flet ((initarg (slot-name) (as-keyword slot-name))
         (accessor (slot-name) slot-name))
    (let ((doc-string (when (stringp (first slot-names))
                        (pop slot-names))))
      `(defclass ,name (event)
         ,(loop for slot-name in slot-names
                collect `(,slot-name
                          :initarg ,(initarg slot-name)
                          :reader ,(accessor slot-name)))
         ,@(when doc-string `((:documentation ,doc-string)))))))

(defmacro define-events (&body event-specs)
  "Define a number of event classes.  An event class specification is
a list of the form \(name . slot-names)}, i.e. a list that
DEFINE-EVENT can be applied to."
  `(progn
     ,@(loop for event-spec in event-specs
             collect `(define-event ,@event-spec))))

(defgeneric on-event (event listener)
  (:method (event listener)
    (declare (ignore event listener)))
  (:documentation
   "Called when an event is fired.  Listeners can add methods to this
generic function in order to do something useful with the events they
receive \(events are sent by speaker objects using FIRE)."))

(defmethod documentation ((x symbol) (doc-type (eql 'event)))
  (documentation (find-class x) 't))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'event)))
  (setf (documentation (find-class x) 't) new-value))

(defclass speaker ()
  ((listeners :initform '())
   (listeners-lock :initform (bt:make-recursive-lock)))
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
       (bt:with-lock-held (,lock)
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
    (on-event datum listener))
  (values))

(define-compiler-macro fire (&whole form speaker datum &rest arguments &environment env)
  (if (and (constantp datum env) (not (typep datum 'event)))
      `(fire ,speaker (make-instance ,datum ,@arguments))
      form))

(defun listeners-list (speaker)
  (with-listeners (listeners speaker)
    (copy-list listeners)))


;;;; Tracing listener

(defclass tracing-listener ()
  ((output :initarg :output :reader tracing-listener-output)
   (describe :initarg :describe :reader tracing-listener-describe-p))
  (:default-initargs :output *standard-output* :describe nil)
  (:documentation
   "Listens for all events and writes to a stream when given one."))

(defmethod on-event (ev (listener tracing-listener))
  (format (tracing-listener-output listener) "~&E: ~S~%" (type-of ev))
  (when (tracing-listener-describe-p listener)
    (describe ev (tracing-listener-output listener))))


;;;; Anonymous listener

(defclass anonymous-listener ()
  ((dispatch-table :initform (make-hash-table :test #'eq)))
  (:documentation
   "Represents an anonymous listener.

Sometimes it is more convenient to specify a listener in-line rather
than define a class and ON-EVENT methods.  When that is the case, use
the lambda of listeners: an anonymous listener."))

(defun make-anonymous-listener (event-classes callbacks)
  (let ((listener (make-instance 'anonymous-listener)))
    (with-slots (dispatch-table) listener
      (loop for event-class in event-classes
            for callback in callbacks
            do (setf (gethash (find-class event-class) dispatch-table) callback)))
    listener))

(defmethod on-event ((ev event) (listener anonymous-listener))
  (let* ((dispatch-table (slot-value listener 'dispatch-table))
         (classes (closer-mop:class-precedence-list (class-of ev)))
         (callback (some (lambda (class) (gethash class dispatch-table)) classes)))
    (when callback
      (funcall callback ev))))

(defmacro anonymous-listener (&body clauses)
  "Syntactic sugar for MAKE-ANONYMOUS-LISTENER.

clause::= ((event-var event-class) . {form}*)
event-var::= variable name
event-class::= event class name"
  (loop for ((var event-class) . forms) in clauses
        collect event-class into event-classes
        collect `(lambda (,var)
                   (declare (ignorable ,var))
                   ,@forms)
        into callbacks
        finally (return `(make-anonymous-listener ',event-classes (list ,@callbacks)))))
