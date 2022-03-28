(in-package #:libpd)

(cffi:define-foreign-library libpd
  (:darwin "libpd.dylib")
  (:unix "libpd.so")
  (:windows "libpd.dll"))

(cffi:use-foreign-library libpd)

;;; Initializing Pd

(defcfun "libpd_init" :int
  "Initialize libpd. It is safe to call this more than once.

Note that this function sets up signal handling so that floating point
exceptions (SIGFPE) will be ignored. The goal is to make sure that bad
Pd patches cannot cause a crash by dividing by zero. If you want to
handle SIGFPE in your code, you can install a handler after calling
libpd_init().")

(defcfun "libpd_clear_search_path" :void
  "Clear the Pd search path, i.e. the path where Pd looks for abstractions and externals.

Clearing the search path is usually unnecessary because libpd_init()
already takes care of this.")

(defcfun "libpd_add_to_search_path" :void
  "Add path to the Pd search path. Relative paths are relative to the current working directory of the program."
  (dirname :string))

;;; Opening patches

(defcfun "libpd_openfile" :pointer
  "Open a patch and return an opaque pointer that serves as a handle to the patch; returns NULL on failure.

The handle is useful for closing the patch and for getting its $0 id."
  (basename :string)
  (dirname :string))

(defcfun "libpd_closefile" :void
  "Close the patch corresponding to the patch handle pointer."
  (file :pointer))

(defcfun "libpd_getdollarzero" :int
  "Return the $0 tag of the patch corresponding to the patch handle pointer."
  (file :pointer))

;; Audio processing with Pd

(defcfun "libpd_blocksize" :int
  "Return the block size of Pd (default 64).

Pd computes audio in ticks of 64 frames at a time.")

(defcfun "libpd_init_audio" :int
  "Initialize audio rendering for the given number of input/output channels and sample rate.

The return value is 0 if and only if the initialization of Pd
succeeded."
  (in-channels :int)
  (out-channels :int)
  (sample-rate :int))

(defcfun "libpd_process_raw" :int
  "Process one Pd tick and copy the contents of the buffers directly to/from Pd, without striping.

In other words, the channels will be stacked rather than interleaved
in inBuffer and outBuffer."
  (in-buffer (:pointer :float))
  (out-buffer (:pointer :float)))

;; TODO: libpd_process_raw_double, libpd_process_raw_short.

(defcfun "libpd_process_float" :int
  "Read one buffer of input samples from inBuffer, process them with Pd, and write one buffer of output samples to outBuffer.

In order to reduce the overhead of functions calls, libpd offers the
option of processing more than one Pd tick per audio rendering
call. For minimal latency, choose one tick. If you run into
performance issues, try a larger number. The size of each buffer must
be the product of the number of channels, the number of ticks, and the
number of samples per tick. (The number of samples per tick is the
value returned by libpd_blocksize(), i.e. 64.) Samples will be
interleaved in the buffer, i.e. if there are two input channels, then
inBuf[0] is the first sample for the left channel, inBuffer[1] is the
first sample for the right channel, inBuffer[2] is the second sample
for the left channel, etc. The return value is 0 if and only if the
call succeeded."
  (ticks :int)
  (in-buffer (:pointer :float))
  (out-buffer (:pointer :float)))

(defcfun "libpd_process_double" :int
  "Like libpd_process_float, but with double precision."
  (ticks :int)
  (in-buffer (:pointer :double))
  (out-buffer (:pointer :double)))

(defcfun "libpd_process_short" :int
  "Like libpd_process_float, but with short samples.

Float samples from Pd are converted to shorts by multiplying by 32767
and casting to short. For efficiency reasons, this method does not
clip the input. If the float samples from Pd are outside the interval
[-1, 1], then the converted short samples will be garbage. If you
expect samples outside of [-1, 1], you need to clip them yourself in
your Pd patch."
  (ticks :int)
  (in-buffer (:pointer :short))
  (out-buffer (:pointer :short)))

;; Accessing arrays in Pd

(defcfun "libpd_arraysize" :int
  "Return the size of the given array, or a negative error code if the array doesn't exist."
  (name :string))

(defcfun "libpd_resize_array" :int
  "Resize the given array, or a negative error code if the array doesn't exist."
  (name :string)
  (size :long))

(defcfun "libpd_read_array" :int
  "Read n values of a named source array and write them to the dest, starting at offset, returns 0 on success.

Will return a negative error code if the array in Pd doesn't exist or
if the range given by n and offset exceeds the range of the
array. Performs no bound checking on the float array dest."
  (destination (:pointer :float))
  (source :string)
  (offset :int)
  ;; ?
  (n :int))

(defcfun "libpd_write_array" :int
  "Read n values from the float array src and write them to a named destination array.

Range checks and error codes as in libpd_read_array."
  (destination :string)
  (offset :int)
  (source (:pointer :float))
  ;; ?
  (n :int))

;;; Sending messages to Pd

(defcfun "libpd_bang" :int
  "Send a bang to the destination receiver,
i.e. libpd_bang(\"foo\") acts like a bang sent to [s foo] in Pd."
  (receiver :string))

(defcfun "libpd_float" :int
  "Send a float to the destination receiver.

The return value is 0 if and only if the call succeeded."
  (receiver :string)
  (float :float))

(defcfun "libpd_symbol" :int
  "Send a symbol to the destination receiver.

The return value is 0 if and only if the call succeeded."
  (receiver :string)
  (symbol :string))

(defcfun "libpd_set_float" :void
  "Write a float value to the given atom."
  (atom :pointer)
  (float :float))

(defcfun "libpd_set_symbol" :void
  "Write a symbol value to the given atom."
  (atom :pointer)
  (symbol :string))

(defcfun "libpd_list" :int
  "Send a list given by a length and an array of atoms to the given destination receiver.

The return value is zero on success, or a negative error code
otherwise."
  (receiver :string)
  (argc :int)
  (argv :pointer))

(defcfun "libpd_message" :int
  "Send a typed message to the given destination receiver.

The return value is zero on success, or a negative error code
otherwise."
  (receiver :string)
  (message :string)
  (argc :int)
  (argv :pointer))

(defcfun "libpd_start_message" :int
  "Initiate the composition of a new list or typed message.

The length argument indicates an upper bound for the number of
elements in the compound message, and the return value is a status
code that is zero on success, or nonzero if the length is too
large.

Note: You have to start each list or typed message with
libpd_start_message, but you don't have to finish it; it's okay to
abandon a message without further cleanup. Also, since the length
argument is an upper bound, it's okay to request more elements up
front than you will actually use in the end."
  (max-length :int))

(defcfun "libpd_add_float" :void
  "Add a float to the current message"
  (float :float))

(defcfun "libpd_add_symbol" :void
  "Add a symbol to the current message"
  (symbol :string))

(defcfun "libpd_finish_list" :int
  "Finish a list message and send it to the destination receiver.

The return value is 0 if and only if the call succeeded."
  (receiver :string))

(defcfun "libpd_finish_message" :int
  "Finish a typed message and send it to the destination receiver, with the message name given by the symbol argument.

The return value is 0 if and only if the call succeeded. Note that
libpd will let you send a typed message of arbitrary length, but Pd
itself will currently only handle typed messages with up to four
arguments."
  (receiver :string)
  (message :string))

;;; Receiving messages from Pd

;; TODO: docstring?
(defcfun "libpd_exists" :int
  (symbol :string))

(defcfun "libpd_bind" :pointer
  "Subscribe to messages sent to the given source receiver.

The call libpd_bind(\"foo\") adds an object to the patch that behaves
much like [r foo], with the output being passed on to the various
message hooks of libpd. The return value is an opaque pointer to the
new receiver object; save it in a variable if you want to be able to
delete this object later.

The call to libpd_bind() should take place after the call to
libpd_init()."
  (symbol :string))

(defcfun "libpd_unbind" :void
  "Delete the object referred to by the pointer p.

This is mostly intended for removing source receivers created by
libpd_bind."
  (object :pointer))

(defcfun "libpd_is_float" :int
  "Returns 1 if the atom is a float type, otherwise 0.

Does not perform a NULL check."
  (atom :pointer))

(defcfun "libpd_is_symbol" :int
  "Returns 1 if the atom is a symbol type, otherwise 0.

Does not perform a NULL check."
  (atom :pointer))

(defcfun "libpd_get_float" :float
  "Get the atom's float value.

Does not perform any NULL or type checks."
  (atom :pointer))

(defcfun "libpd_get_symbol" :string
  "Get the atom's symbol value.

Does not perform any NULL or type checks."
  (atom :pointer))

(defcfun "libpd_next_atom" :pointer
  (atom :pointer))

;; typedef void (*t_libpd_printhook)(const char *recv);
;; typedef void (*t_libpd_banghook)(const char *recv);
;; typedef void (*t_libpd_floathook)(const char *recv, float x);
;; typedef void (*t_libpd_symbolhook)(const char *recv, const char *sym);
;; typedef void (*t_libpd_listhook)(const char *recv, int argc, t_atom *argv);
;; typedef void (*t_libpd_messagehook)(const char *recv, const char *msg,
;;     int argc, t_atom *argv);

;;; Hook binding

(defcfun "libpd_set_printhook" :void
  (hook :pointer))

(defcfun "libpd_set_banghook" :void
  (hook :pointer))

(defcfun "libpd_set_floathook" :void
  (hook :pointer))

(defcfun "libpd_set_symbolhook" :void
  (hook :pointer))

(defcfun "libpd_set_listhook" :void
  (hook :pointer))

(defcfun "libpd_set_messagehook" :void
  (hook :pointer))

;; MIDI support

(defcfun "libpd_noteon" :int
  (channel :int)
  (pitch :int)
  (velocity :int))

(defcfun "libpd_controlchange" :int
  (channel :int)
  (controller :int)
  (value :int))

(defcfun "libpd_programchange" :int
  (channel :int)
  (value :int))

(defcfun "libpd_pitchbend" :int
  (channel :int)
  (value :int))

(defcfun "libpd_aftertouch" :int
  (channel :int)
  (value :int))

(defcfun "libpd_polyaftertouch" :int
  (channel :int)
  (pitch :int)
  (value :int))

(defcfun "libpd_midibyte" :int
  (port :int)
  (byte :int))

(defcfun "libpd_sysex" :int
  (port :int)
  (byte :int))

(defcfun "libpd_sysrealtime" :int
  (port :int)
  (byte :int))

;; typedef void (*t_libpd_noteonhook)(int channel, int pitch, int velocity);
;; typedef void (*t_libpd_controlchangehook)(int channel,
;;     int controller, int value);
;; typedef void (*t_libpd_programchangehook)(int channel, int value);
;; typedef void (*t_libpd_pitchbendhook)(int channel, int value);
;; typedef void (*t_libpd_aftertouchhook)(int channel, int value);
;; typedef void (*t_libpd_polyaftertouchhook)(int channel, int pitch, int value);
;; typedef void (*t_libpd_midibytehook)(int port, int byte);

(defcfun "libpd_set_noteonhook" :void
  (hook :pointer))

(defcfun "libpd_set_controlchangehook" :void
  (hook :pointer))

(defcfun "libpd_set_programchangehook" :void
  (hook :pointer))

(defcfun "libpd_set_pitchbendhook" :void
  (hook :pointer))

(defcfun "libpd_set_aftertouchhook" :void
  (hook :pointer))

(defcfun "libpd_set_polyaftertouchhook" :void
  (hook :pointer))

(defcfun "libpd_set_midibytehook" :void
  (hook :pointer))

;;; GUI

(defcfun "libpd_start_gui" :int
  "Open the current patches within a Pd vanilla GUI.

This requires the path to Pd's main folder that contains bin/, tcl/,
etc. For a macOS .app bundle, this is
/path/to/Pd-#.#-#.app/Contents/Resources. Returns 0 on success."
  (path :string))

(defcfun "libpd_stop_gui" :void
  "Stops the Pd vanilla GUI currently connected with the libpd instance.")

(defcfun "libpd_poll_gui" :void
  "Call this to manually update and handle any GUI messages.

This is called automatically when using a libpd_process
function. Note, this also facilitates network message processing, etc
so it can be useful to call repeatedly when idle for more
throughput.")

;;; Multiple Instances

(defcfun "libpd_new_instance" :pointer
  "Create a new pd instance.

Returns an instance pointer handle or NULL when libpd is not compiled
with PDINSTANCE.")

(defcfun "libpd_set_instance" :void
  "Set the current pd instance.

Subsequent libpd calls will then affect this instance only. This does
nothing when libpd is not compiled with PDINSTANCE."
  (instance :pointer))

(defcfun "libpd_free_instance" :void
  "Free a pd instance.

Does nothing when libpd is not compiled with PDINSTANCE"
  (instance :pointer))

(defcfun "libpd_this_instance" :pointer
  "Get the current pd instance.

Should always return a valid instance, regardless if libpd is compiled with PDINSTANCE or not.")

(defcfun "libpd_get_instance" :pointer
  "Get a pd instance by index.

Returns NULL if the index is out of bounds or \"this\" instance when
libpd is not compiled with PDINSTANCE."
  (index :int))

(defcfun "libpd_num_instances" :int
  "Get the number of pd instances.

Returns 1 when libpd is not compiled with PDINSTANCE.")

;;; Logging

(defcfun "libpd_set_verbose" :void
  "Set verbose print state, either 0 or 1."
  (verbose :int))

(defcfun "libpd_get_verbose" :int
  "Get the verbose print state: 0 or 1.")
