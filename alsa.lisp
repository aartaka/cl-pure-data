;;;; alsa.lisp

(defpackage #:cl-pure-data-alsa
  (:nicknames #:pda #:pure-data-alsa)
  (:use #:cl)
  (:export #:*alsa-device*))

(in-package #:cl-pure-data-alsa)

;; Copied from cl-mixed.
(cffi:define-foreign-library libasound
  (:unix (:or "libasound.so.2.0.0" "libasound.so.2" "libasound.so"))
  (T (:or (:default "libasound") (:default "asound"))))

(cffi:defcenum pcm-stream
  :playback
  :capture)

(cffi:defcenum pcm-format
  (:unknown -1)
  :int8
  :uint8
  :int16
  :int16-be
  :uint16
  :uint16-be
  :int24
  :int24-be
  :uint24
  :uint24-be
  :int32
  :int32-be
  :uint32
  :uint32-be
  :float
  :float-be
  :double
  :double-be
  :iec958-subframe-le
  :iec958-subframe-be
  :mu-law
  :a-law
  :ima-adpcm
  :mpeg
  :gsm
  :special
  :s24-3le
  :s24-3be
  :u24-3le
  :u24-3be
  :s20-3le
  :s20-3be
  :u20-3le
  :u20-3be
  :s18-3le
  :s18-3be
  :u18-3le
  :u18-3be
  :s16
  :u16
  :s24
  :u24
  :s32
  :u32
  :float32
  :float64
  :iec958-subframe)

(cffi:defcenum pcm-access
  :mmap-interleaved
  :mmap-noninterleaved
  :mmap-complex
  :rw-interleaved
  :rw-noninterleaved)

(cffi:defcfun (pcm-open "snd_pcm_open") :int
  (pcm :pointer)
  (name :string)
  (stream pcm-stream)
  (mode :int))

(cffi:defcfun (pcm-set-params "snd_pcm_set_params") :int
  (pcm :pointer)
  (format pcm-format)
  (access pcm-access)
  (channels :uint)
  (rate :uint)
  (soft-resample :int)
  (latency :uint))

(cffi:defcfun (pcm-start "snd_pcm_start") :int
  (pcm :pointer))

(cffi:defcfun (pcm-writei "snd_pcm_writei") :long
  (pcm :pointer)
  (buffer :pointer)
  (frames :ulong))

(cffi:defcfun (pcm-recover "snd_pcm_recover") :int
  (pcm :pointer)
  (err :int)
  (silent :int))

(cffi:defcfun (pcm-drain "snd_pcm_drain") :int
  (pcm :pointer))

(cffi:defcfun (pcm-close "snd_pcm_close") :int
  (pcm :pointer))

;; cl-pure-data code.

(defvar *alsa-device* "sysdefault"
  "The device to use for output.")

(defvar *alsa-out* nil
  "The stream to output data for ALSA to.")

(defmethod pd:init-audio :around (ins outs sample-rate)
  (prog1
      (call-next-method)
    (unless *alsa-out*
      (cffi:load-foreign-library 'libasound)
      (let ((pcm (cffi:foreign-alloc :pointer)))
        (pcm-open pcm *alsa-device* :playback 0)
        (setf *alsa-out* (cffi:mem-ref pcm :pointer))
        (pcm-set-params
         *alsa-out* :float :rw-interleaved
         pd:*audio-out-channels* pd:*audio-sample-rate* 1 1000)
        (pcm-start *alsa-out*)))))

(defmethod pd:process :around (in-buffer &optional (ticks 1))
  (let* ((result (call-next-method))
         (foreign-result (cffi:foreign-array-alloc result `(:array :float ,(length result))))
         (played (pcm-writei *alsa-out* foreign-result (* pd:*audio-block-size* ticks))))
    (when (< played 0)
      (pcm-recover *alsa-out* played 0))
    (cffi:foreign-array-free foreign-result)
    result))

(defmethod pd:release :around ()
  (unwind-protect
       (call-next-method)
    (pcm-drain *alsa-out*)
    (pcm-close *alsa-out*)))
