;;;; audio.lisp

(in-package #:cl-pure-data)

(defvar *audio-in-channels* nil
  "Number of channels to input data to PD.")

(defvar *audio-out-channels* nil
  "Number of output channels to output PD data at.")

(defvar *audio-sample-rate* nil
  "Sample rate.")

(defvar *audio-block-size* nil
  "Block size.")

(defpdfun init-audio (ins outs sample-rate)
  "Initialize the audio-processing side of PD.

INS is the number of input channels (can be zero).
OUTS is the number of output channels (can be zero?)
SAMPLE-RATE is the sample rate at which to play (44100, 48K etc.)

Sets `*audio-in-channels*', `*audio-out-channels*',
`*audio-sample-rate*', and `*audio-block-size*' when ran."
  (prog1
      (zerop (libpd:libpd-init-audio ins outs sample-rate))
    (setf *audio-in-channels* ins
          *audio-out-channels* outs
          *audio-sample-rate* sample-rate
          *audio-block-size* (libpd:libpd-blocksize))))

(defpdfun process (in-buffer &optional (ticks 1))
  "Process the IN-BUFFER data in PD for TICKS and return the result of the processing.

The result is either single-float array of size
(* TICKS *AUDIO-BLOCK-SIZE* *AUDIO-OUT-CHANNELS*)
or signaled condition.

Ensure that IN-BUFFER has length of
(* TICKS *AUDIO-BLOCK-SIZE* *AUDIO-IN-CHANNELS*)
before calling this. "
  (let* ((out-size (* ticks *audio-block-size* *audio-out-channels*))
         ;; FIXME: A dirty hack with an unexported API.
         (array-type (cffi::ensure-parsed-base-type
                      (list :array :float out-size))))
    (assert (= (length in-buffer)
               (* ticks *audio-block-size* *audio-in-channels*)))
    (cffi:with-foreign-array
        (out-buffer (make-array out-size :initial-element (coerce 0 'single-float)) array-type)
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
