;;;; globals.lisp

(in-package #:cl-pure-data)

(defvar *instance* nil
  "Current PD instance.
If nil, PD is not yet initialized.")

(defvar *verbose* 1
  "Verbose print state.")

(defvar *queued*
  ;; Stolen from bordeaux-threads.
  #+(or armedbear
        (and allegro multiprocessing)
        (and clisp mt)
        (and openmcl openmcl-native-threads)
        (and cmu mp)
        corman
        (and ecl threads)
        mkcl
        lispworks
        (and digitool ccl-5.1)
        (and sbcl sb-thread)
        scl) t
  #-(or armedbear
        (and allegro multiprocessing)
        (and clisp mt)
        (and openmcl openmcl-native-threads)
        (and cmu mp)
        corman
        (and ecl threads)
        mkcl
        lispworks
        (and digitool ccl-5.1)
        (and sbcl sb-thread)
        scl) nil
  "Whether PD is initialized as queued.")

(defvar *patches* (make-hash-table :test #'equal)
  "A table of all the opened patches (as path -> raw pointers hash table).")

(defvar *subscriptions* '()
  "A list of the subscribed events (as raw pointers).")

(defvar *search-path* '()
  "List of paths to search for external patches.")
