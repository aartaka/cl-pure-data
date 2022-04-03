;;;; pure-data.lisp

(in-package #:cl-pure-data)

(defvar *instance* nil
  "Current PD instance.
If nil, PD is not yet initialized.")

(defvar *verbose* 1
  "Verbose print state.")

(defvar *patches* (make-hash-table :test #'equal)
  "A table of all the opened patches (as path -> raw pointers hash table).")

(defvar *subscriptions* '()
  "A list of the subscribed events (as raw pointers)")

(defvar *search-path* '()
  "List of paths to search for external patches.")

(defmethod init ()
  "Unexported initialization sequence that happens before every defpdfun-ed function call."
  (if *instance*
      (libpd:libpd-set-instance *instance*)
      (progn
        (libpd:libpd-init)
        (setf *instance* (libpd:libpd-this-instance))))
  (init-hooks)
  (libpd:libpd-clear-search-path)
  (mapcar (alexandria:compose #'libpd:libpd-add-to-search-path #'uiop:native-namestring)
          *search-path*)
  (libpd:libpd-set-verbose *verbose*))

(defmacro defpdfun (name args &body body)
  "Define a new external API function.

A thin shim around `defmethod'.

This macro will manage all the PD internals and initialization for the user of the function."
  (multiple-value-bind (body declarations documentation)
      (alexandria:parse-body body :documentation t)
    `(defmethod ,name ,args
       ,documentation
       ,@declarations
       (init)
       ,@body)))

(defpdfun open-patch ((pathname pathname))
  "Open the patch at PATHNAME.

Returns either t if it's open or nil if something's wrong."
  (unless (uiop:directory-pathname-p pathname)
    (let* ((pathname (if (uiop:relative-pathname-p pathname)
                         (uiop:merge-pathnames* pathname (uiop:getcwd))
                         pathname))
           (patch (unless (gethash (uiop:native-namestring pathname) *patches*)
                    (libpd:libpd-openfile
                     (uiop:strcat (pathname-name pathname)
                                  "." (pathname-type pathname))
                     (uiop:native-namestring
                      (uiop:pathname-directory-pathname pathname))))))
      (unless (or (null patch)
                  (and (cffi:pointerp patch)
                       (cffi:null-pointer-p patch)))
        (setf (gethash (uiop:native-namestring pathname) *patches*) patch)
        t))))

(defmethod open-patch ((pathname string))
  (open-patch (uiop:parse-native-namestring pathname)))

(defpdfun close-patch ((pathname pathname))
  "Close patch from PATHNAME.

Return T in case of success, nil otherwise. If it hangs -- something's terribly wrong."
  (when (and pathname
             (uiop:absolute-pathname-p pathname)
             (gethash (uiop:native-namestring pathname) *patches*))
    (libpd:libpd-closefile (gethash (uiop:native-namestring pathname) *patches*))
    t))

(defmethod close-patch ((pathname string))
  (close-patch (uiop:parse-native-namestring pathname)))

(defmacro with-patch (pathname &body body)
  "Open the patch from PATHNAME in the scope of the BODY.

Return T in case of success, nil otherwise. If it hangs -- something's terribly wrong.

Close the patch after BODY returns."
  (let ((var (gensym "result")))
    (alexandria:once-only (pathname)
      `(alexandria:when-let ((,var (open-patch ,pathname)))
         (unwind-protect
              (progn ,@body)
           (close-patch ,pathname))))))

(defpdfun subscribe ((sender string))
  (push (libpd:libpd-bind sender) *subscriptions*)
  t)

(defpdfun release ()
  (mapcar #'close-patch (alexandria:hash-table-keys *patches*))
  (mapcar #'libpd:libpd-unbind *subscriptions*)
  t)

(defun low-princ (object)
  (let ((*print-case* :downcase))
    (princ-to-string object)))

;; TODO: Throw exceptions if non-zero result.
(defpdfun message ((receiver string) &rest message-and-content)
  "Send a message to the RECEIVER object.

Return T in case of success, nil otherwise. If it hangs -- something's terribly wrong.

Depending on the length of MESSAGE-AND-CONTENT, it is split in different ways:
0 - Send a simple bang to the RECEIVER.
1 - Send the first element as an unnamed message with float/symbol contents.
else - send the message named by first element with the content being the rest."
  (zerop
   (case (length message-and-content)
     (0 (libpd:libpd-bang receiver))
     (1 (let ((content (first message-and-content)))
          (typecase content
            ((or integer double-float single-float)
             (libpd:libpd-float receiver (coerce content 'single-float)))
            (string (libpd:libpd-symbol receiver content))
            (null (libpd:libpd-float receiver 0.0))
            (symbol (libpd:libpd-symbol receiver (low-princ content))))))
     (otherwise
      (destructuring-bind (message &rest content)
          message-and-content
        (libpd:libpd-start-message (length content))
        (labels ((add-content (content)
                   (typecase content
                     ((or integer double-float)
                      (libpd:libpd-add-float (coerce content 'single-float)))
                     (single-float (libpd:libpd-add-float content))
                     (string (libpd:libpd-add-symbol content))
                     (null (libpd:libpd-add-float 0.0))
                     (symbol (libpd:libpd-add-symbol (low-princ content)))
                     (sequence (mapc #'add-content content)))))
          (add-content content))
        (libpd:libpd-finish-message receiver message))))))

(defmethod message ((receiver symbol) &rest message-and-content)
  (apply #'call-next-method (low-princ receiver) message-and-content))
