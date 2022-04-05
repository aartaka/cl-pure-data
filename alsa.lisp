;;;; alsa.lisp

(defpackage #:cl-pure-data-alsa
  (:nicknames #:pda #:pure-data-alsa)
  (:use #:cl)
  (:export #:*alsa-device*))

(in-package #:cl-pure-data-alsa)

(defvar *alsa-device* "sysdefault"
  "The device to use for output.")

(defvar *alsa-out* nil
  "The stream to output data for ALSA to.")

(defmethod pd:init-audio :around (ins outs sample-rate)
  (prog1
      (call-next-method)
    (unless *alsa-out*
      (setf *alsa-out*
            (also-alsa:alsa-open
             *alsa-device* pd:*audio-block-size* 'single-float
             :direction :output
             :sample-rate pd:*audio-sample-rate*
             :channels-count pd:*audio-out-channels*))
      (also-alsa:alsa-start *alsa-out*))))

(defmethod pd:process :around (in-buffer &optional (ticks 1))
  (let ((result (call-next-method))
        (buffer-size (also-alsa:buffer-size *alsa-out*)))
    (loop for i below ticks
          for start = (* i buffer-size)
          do (setf (subseq (also-alsa:buffer *alsa-out*) 0 buffer-size)
                   (subseq result start))
          do (also-alsa:alsa-write *alsa-out*)
          finally (return result))))

(defmethod pd:release :around ()
  (unwind-protect
       (call-next-method)
    (also-alsa:drain *alsa-out*)
    (also-alsa:alsa-close *alsa-out*)))
