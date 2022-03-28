;;;; hooks.lisp

(in-package #:cl-pure-data)

(defvar *print-hook* nil
  "List of handlers for libpd_printhook.

All of the handlers should be (function (string) *).
The first argument is the receiver.")

(defvar *bang-hook* nil
  "List of handlers for libpd_banghook.

All of the handlers should be (function (string) *).
The first argument is the receiver.")

(defvar *float-hook* nil
  "List of handlers for libpd_floathook.

All of the handlers should be (function (string number) *).
The first argument is the receiver, the second argument is the float.")

(defvar *symbol-hook* nil
  "List of handlers for libpd_symbolhook.

All of the handlers should be (function (string string) *).
The first argument is the receiver, the second argument is the symbol received.")

(defvar *list-hook* nil
  "List of handlers for libpd_listhook.

All of the handlers should be (function (string list) *).
The first argument is the receiver, the second argument is the list received.")

(defvar *message-hook* nil
  "List of handlers for libpd_messagehook.

All of the handlers should be (function (string string list) *).
- The first argument is the receiver.
- The second argument is the message.
- The third argument is the list of values received.")

(cffi:defcallback print-hook-callback :void ((receiver :string))
  (dolist (handler *print-hook*)
    (when (typep handler 'function)
      (funcall handler receiver))))

(cffi:defcallback bang-hook-callback :void ((receiver :string))
  (dolist (handler *bang-hook*)
    (when (typep handler 'function)
      (funcall handler receiver))))

(cffi:defcallback float-hook-callback :void ((receiver :string) (float :float))
  (dolist (handler *float-hook*)
    (when (typep handler 'function)
      (funcall handler receiver float))))

(cffi:defcallback symbol-hook-callback :void ((receiver :string) (symbol :string))
  (dolist (handler *float-hook*)
    (when (typep handler 'function)
      (funcall handler receiver symbol))))

(defun atom-to-lisp (argc argv)
  (loop for head = argv then (libpd:libpd-next-atom head)
        for counter = argc then (decf argc)
        until (zerop counter)
        when (libpd:libpd-is-float head)
          collect (libpd:libpd-get-float head)
        else when (libpd:libpd-is-symbol head)
               collect (libpd:libpd-get-symbol head)
        else collect nil))

(cffi:defcallback list-hook-callback :void
    ((receiver :string) (argc :int) (argv :pointer))
  (let ((list (atom-to-lisp argc argv)))
    (dolist (handler *list-hook*)
      (when (typep handler 'function)
        (funcall handler receiver list)))))

(cffi:defcallback message-hook-callback :void
    ((receiver :string) (message :string) (argc :int) (argv :pointer))
  (let ((list (atom-to-lisp argc argv)))
    (dolist (handler *message-hook*)
      (when (typep handler 'function)
        (funcall handler receiver message list)))))

(defun init-hooks ()
  "The helper to set all the PD hooks.

NOTE: Only run it after PD is initiated! In most cases, this is taken
care of by any function from `cl-pure-data' that you use, so you don't
have to call this function directly. However, if you use `libpd'
directly, take care to first initialize PD with `libpd:libpd-init'."
  (libpd:libpd-set-printhook (cffi:callback print-hook-callback))
  (libpd:libpd-set-banghook (cffi:callback bang-hook-callback))
  (libpd:libpd-set-floathook (cffi:callback float-hook-callback))
  (libpd:libpd-set-symbolhook (cffi:callback symbol-hook-callback))
  (libpd:libpd-set-listhook (cffi:callback list-hook-callback))
  (libpd:libpd-set-messagehook (cffi:callback message-hook-callback)))

(defun init-default-hooks ()
  (setf *print-hook* (list (lambda (receiver)
                             (format t "~a" receiver)))
        *bang-hook* (list (lambda (receiver)
                            (format t "~&bang: ~s~%" receiver)))
        *symbol-hook* (list (lambda (receiver symbol)
                              (format t "~&symbol (in ~s): ~s~%" receiver symbol)))
        *float-hook* (list (lambda (receiver float)
                             (format t "~&float (in ~s): ~s~%" receiver float)))
        *list-hook* (list (lambda (receiver list)
                            (format t "~&list (in ~s): ~s~%" receiver list)))
        *message-hook* (list (lambda (receiver message list)
                               (format t "~&message (~s to ~s): ~s~%" message receiver list))))
  t)
