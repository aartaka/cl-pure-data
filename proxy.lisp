;;;; proxy.lisp

(in-package #:cl-pure-data)

(defvar *object-id* 0 "The last ID assigned to the object.")

(defvar *connections* '() "The list of all the connections in the currently parsed patch.")

(defvar *pd* nil "The stream to serialize the objects to.")

(defvar *proxies* (make-hash-table)
  "The map of all the proxies, from their name to their current patch file.")

(defvar *last-proxy-messages*
  (make-hash-table)
  "The map of the last messages sent by every proxy out there.

A symbol->alist hash-table.")

(defun get-id ()
  (prog1
      *object-id*
    (incf *object-id*)))

(defstruct line
  (id (get-id) :type integer))

(defstruct (generic-object-line (:include line))
  (args '() :type list)
  (incoming '() :type list))

(defstruct (object-line (:include generic-object-line))
  (name (alexandria:required-argument "name") :type string)
  (incoming1 '() :type list)
  (incoming2 '() :type list))

(defstruct (message-line (:include generic-object-line)))

(defstruct toplevel
  (inputs nil :type list))

(defstruct (variable-line (:include line))
  (name (alexandria:required-argument "name") :type string))

(defmethod pd-compile ((value list))
  (loop for el in (rest value)
        when (or (symbolp el) (listp el))
          collect el into connections
        else
          collect el into args
        finally (return (if (eq :msg (first value))
                            (make-message-line
                             :args args
                             :incoming (mapcar #'pd-compile connections))
                            (make-object-line
                             :name (low-princ (first value))
                             :args args
                             :incoming (mapcar #'pd-compile
                                               (subseq connections 0 (position :1 connections)))
                             :incoming1 (when (find :1 connections)
                                          (mapcar #'pd-compile
                                                  (subseq connections (1+ (position :1 connections))
                                                          (position :2 connections))))
                             :incoming2 (when (find :2 connections)
                                          (mapcar #'pd-compile
                                                  (subseq connections (1+ (position :2 connections))))))))))

(defmethod pd-compile ((value symbol))
  (make-variable-line :name (low-princ value)))

(defmethod pd-serialize ((value toplevel))
  (format *pd* "#N canvas 50 50 2000 8000 14;~%")
  (mapcar #'pd-serialize (toplevel-inputs value))
  (loop for (out outlet in inlet) in *connections*
        do (format *pd* "#X connect ~d ~d ~d ~d;~%"
                   out outlet in inlet)))

(defmethod pd-serialize ((value variable-line))
  (format *pd* "#X floatatom 100 100 5 0 0 0 - ~a -;~%"
          (variable-line-name value)))

(defmethod pd-serialize ((value object-line))
  (dolist (in (object-line-incoming value))
    (pd-serialize in)
    (push (list (line-id in) 0 (line-id value) 0) *connections*))
  (dolist (in (object-line-incoming1 value))
    (pd-serialize in)
    (push (list (line-id in) 0 (line-id value) 1) *connections*))
  (dolist (in (object-line-incoming2 value))
    (pd-serialize in)
    (push (list (line-id in) 0 (line-id value) 2) *connections*))
  (format *pd* "#X obj 100 100 ~a~{ ~a~};~%"
          (object-line-name value) (object-line-args value)))

(defmethod pd-serialize ((value message-line))
  (dolist (in (message-line-incoming value))
    (pd-serialize in)
    (push (list (line-id in) 0 (line-id value) 0) *connections*))
  (format *pd* "#X msg 100 100~{ ~a~};~%" (message-line-args value)))

(defun proxy-on (name path)
  (unless (equal (gethash name *proxies*) path)
    (when (gethash name *proxies*)
      (proxy-off name))
    (open-patch (setf (gethash name *proxies*) path))))

(defun proxy-off (name)
  (alexandria:when-let ((proxy-file (gethash name *proxies*)))
    (remhash name *proxies*)
    (close-patch proxy-file)))

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
\(pd:defproxy osc (&optional (freq 440))
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
         (compiled (make-toplevel :inputs (mapcar #'pd-compile body)))
         (arg-names (multiple-value-bind (required optional rest keywords)
                        (alexandria:parse-ordinary-lambda-list args)
                      (append required (mapcar #'first optional)
                              (uiop:ensure-list rest) (mapcar #'cadar keywords))))
         (path (uiop:with-temporary-file (:pathname path
                                          :stream *pd*
                                          :type "pd"
                                          :keep t)
                 (pd-serialize compiled)
                 path)))
    (when (gethash name *proxies*)
      (proxy-on name path)
      (loop for (receiver . value) in (gethash name *last-proxy-messages*)
            do (pd:message receiver value)))
    `(progn
       (defun ,name ,args
         (proxy-on (quote ,name) ,path)
         (setf (gethash (quote ,name) *last-proxy-messages*) '())
         ,@(loop for arg in arg-names
                 collect `(when ,arg
                            (push (cons ,(low-princ arg) ,arg)
                                  (gethash (quote ,name) *last-proxy-messages*))
                            (pd:message ,(low-princ arg) ,arg))))
       (defun ,(alexandria:symbolicate name '-off) ()
         (proxy-off (quote ,name))))))
