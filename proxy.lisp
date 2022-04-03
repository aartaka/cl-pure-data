;;;; proxy.lisp

(in-package #:cl-pure-data)

(defvar *object-id* 0 "The last ID assigned to the object.")

(defvar *connections* '() "The list of all the connections in the currently parsed patch.")

(defvar *pd* nil "The stream to serialize the objects to.")

(defvar *proxies* (make-hash-table)
  "The map of all the proxies, from their name to their meta-data.")

(defstruct proxy
  (active? nil :type boolean :read-only t)
  (file nil :type (or pathname null) :read-only t))

(defun (setf proxy-active?) (activate proxy)
  (check-type proxy proxy)
  (unless (eq activate (slot-value proxy 'active?))
    (if activate
        (pd:open-patch (proxy-file proxy))
        (pd:close-patch (proxy-file proxy))))
  (setf (slot-value proxy 'active?) activate))

(defun (setf proxy-file) (new-file proxy)
  (check-type proxy proxy)
  (unless (equalp new-file (slot-value proxy 'file))
    (pd:close-patch (proxy-file proxy))
    (setf (slot-value proxy 'file) new-file)
    (pd:open-patch (proxy-file proxy))))

(defun get-id ()
  (prog1
      *object-id*
    (incf *object-id*)))

(defstruct line
  (id (get-id) :type integer))

(defstruct (object-line (:include line))
  (name (alexandria:required-argument "name") :type string)
  (args '() :type list)
  (incoming '() :type list))

(defstruct toplevel
  (input nil :type line))

(defstruct (variable-line (:include line))
  (name (alexandria:required-argument "name") :type string))

(defmethod pd-compile ((value list))
  (loop for el in (rest value)
        when (or (symbolp el) (listp el))
          collect el into connections
        else
          collect el into args
        finally (return (make-object-line
                         :name (pd::low-princ (first value))
                         :args args
                         :incoming (mapcar #'pd-compile connections)))))

(defmethod pd-compile ((value symbol))
  (make-variable-line :name (pd::low-princ value)))

(defmethod pd-serialize ((value toplevel))
  (format *pd* "#N canvas 50 50 2000 8000 14;~%")
  (pd-serialize (toplevel-input value))
  (loop for (out outlet in inlet) in *connections*
        do (format *pd* "#X connect ~d ~d ~d ~d;~%"
                   out outlet in inlet)))

(defmethod pd-serialize ((value variable-line))
  (format *pd* "#X floatatom 100 100 5 0 0 0 - ~a -;~%"
          (variable-line-name value)))

(defmethod pd-serialize ((value object-line))
  (dolist (in (object-line-incoming value))
    (pd-serialize in)
    (push (list (line-id in) 0 (line-id value) (position in (object-line-incoming value))) *connections*))
  (format *pd* "#X obj 100 100 ~a~{ ~a~};~%"
          (object-line-name value) (object-line-args value)))

(defmacro defproxy (name args &body body)
  "Create a proxy -- a set of functions binding Lisp code to a PD file loadable into the image.

ARGS can be an arbitrary complexity ordinary lambda list, and it
should work fine sending non-nil values to PD. No guarantees, though.

Process BODY (Lisp code) into a PD code (parsing, documentation, and
code generation yet to be improved). Create two functions:

- NAME -- enables the proxy by loading it into the image. the next
  `process' call you make will happen in the environment with this
  proxy patch loaded. Also sends messages to the objects receiving
  from the arguments you provided to the NAME-d function.
- NAME-OFF (e.g. mix-off) -- Temporarily disables the proxy. The
  processing happens in the environment without this proxy after you
  call this function, but you can call NAME and it will be loaded
  back.

Example:

;; Initialize CLPD.
\(pd:init-default-hooks)
\(pd:init-audio 0 1 44100)
\(pd:message \"pd\" \"dsp\" 1)

;; Create a simple proxy emitting stable yet chaotic sound.
\(pd:defproxy bloopy ()
  (dac~ (osc~ (+~ (*~ (samphold~ (noise~) (phasor~ 6)) 200) 440)))

;; Initialize it.
\(bloopy)

;; Make it emit sound. Ensure you have cl-pure-data/asla or an equivalent loaded!
\(loop repeat 1000 do (pd:process #()))

;; Disable bloopy.
\(bloopy-off)

;; Create an even simpler proxy, yet having an argument.
\(pd::defproxy osc (&optional (freq 440))
  (dac~ (osc~ freq)))

;; Initialize it and make it play sound. Use the default frequency.
\(osc)
\(loop repeat 500 do (pd:process #()))

;; Now change the frequency to something audibly higher.
\(osc 600)
\(loop repeat 500 do (pd:process #()))

;; Don't run this -- for the safety of your ears!
\(osc 4000)
\(loop repeat 1000 do (pd:process #()))"
  (let* ((*object-id* 0)
         (*connections* '())
         (compiled (make-toplevel :input (pd-compile (first body))))
         (parsed-args (multiple-value-list (alexandria:parse-ordinary-lambda-list args)))
         (arg-names (destructuring-bind (required optional rest keywords &rest _)
                        parsed-args
                      (declare (ignorable _))
                      (append required (mapcar #'first optional)
                              (uiop:ensure-list rest) (mapcar #'cadar keywords)))))
    (uiop:with-temporary-file (:pathname path
                               :stream *pd*
                               :type "pd"
                               :direction :output
                               :keep t)
      (pd-serialize compiled)
      (let ((old-proxy (gethash name *proxies*))
            (new-proxy (make-proxy :file path)))
        (setf (gethash name *proxies*) new-proxy)
        (when old-proxy
          (setf (proxy-active? new-proxy) (proxy-active? old-proxy))))
      `(progn
         (defun ,name ,args
           (setf (proxy-file (gethash (quote ,name) *proxies*)) ,path)
           (setf (proxy-active? (gethash (quote ,name) *proxies*)) t)
           ,@(loop for arg in arg-names
                   collect `(when ,arg (pd:message ,(pd::low-princ arg) ,arg))))
         (defun ,(intern (format nil "~a-OFF" (symbol-name name))) ()
           (setf (proxy-active? (gethash (quote ,name) *proxies*)) nil))))))
