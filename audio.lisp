;;;; audio.lisp

(in-package #:cl-pure-data)

(defvar *audio-in-channels* nil
  "Number of channels to input data to PD.")

(defvar *audio-out-channels* nil
  "Number of output channels to output PD data at.")

(defvar *audio-sample-rate* nil
  "Sample rate.")

(defpdfun init-audio (ins outs sample-rate)
  (setf *audio-in-channels* ins
        *audio-out-channels* outs
        *audio-sample-rate* sample-rate)
  (zerop (libpd:libpd-init-audio ins outs sample-rate)))

(defpdfun process (in-buffer &optional (ticks 1))
  (let* ((block-size (libpd:libpd-blocksize))
         (out-size (* ticks block-size *audio-out-channels*))
         (array-type (cffi::ensure-parsed-base-type
                      (list :array :float out-size))))
    (assert (= (length in-buffer)
               (* ticks block-size *audio-in-channels*)))
    (cffi:with-foreign-array
        (out-buffer (make-array out-size :initial-element (coerce 0 'single-float))
                    array-type)
      (typecase in-buffer
        ((array single-float)
         (libpd:libpd-process-float
          ticks (cffi:foreign-alloc
                 :float
                 :initial-contents in-buffer
                 :count (array-dimension in-buffer 0))
          out-buffer))
        ((or (array double-float)
             (array integer))
         (libpd:libpd-process-double
          ticks (cffi:foreign-alloc
                 :double
                 :initial-contents (map 'vector (lambda (e) (coerce e 'double-float)) in-buffer)
                 :count (array-dimension in-buffer 0))
          out-buffer)))
      (cffi:foreign-array-to-lisp out-buffer array-type))))
