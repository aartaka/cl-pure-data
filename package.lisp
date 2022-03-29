;;;; package.lisp

(defpackage #:cl-pure-data
  (:nicknames #:pd #:clpd)
  (:use #:cl)
  (:export
   ;; Hooks
   #:*print-hook*
   #:*bang-hook*
   #:*float-hook*
   #:*symbol-hook*
   #:*list-hook*
   #:*message-hook*
   #:init-default-hooks

   ;; Audio
   #:*audio-in-channels*
   #:*audio-out-channels*
   #:*audio-sample-rate*
   #:init-audio
   #:process

   ;; Arrays
   #:arr
   #:contents
   #:elem

   ;; Basic PD
   #:*instance*
   #:*verbose*
   #:*search-path*
   #:with-pd
   #:defpdfun
   #:open-patch
   #:close-patch
   #:with-patch
   #:message
   #:subscribe))

(defpackage #:libpd
  (:import-from #:cffi #:defcfun)
  (:use #:cl)
  (:export
   #:libpd-init #:libpd-clear-search-path #:libpd-add-to-search-path
   #:libpd-openfile #:libpd-closefile #:libpd-getdollarzero

   #:libpd-blocksize #:libpd-init-audio
   #:libpd-process-raw #:libpd-process-float #:libpd-process-double #:libpd-process-short

   #:libpd-arraysize #:libpd-resize-array #:libpd-read-array #:libpd-write-array

   #:libpd-bang #:libpd-float #:libpd-symbol
   #:libpd-set-float #:libpd-set-symbol
   #:libpd-list #:libpd-message
   #:libpd-start-message #:libpd-add-float #:libpd-add-symbol
   #:libpd-finish-list #:libpd-finish-message

   #:libpd-exists #:libpd-bind #:libpd-unbind

   #:libpd-is-float #:libpd-is-symbol #:libpd-get-float #:libpd-get-symbol #:libpd-next-atom

   #:libpd-set-printhook #:libpd-set-messagehook
   #:libpd-set-banghook #:libpd-set-floathook #:libpd-set-symbolhook #:libpd-set-listhook

   #:libpd-noteon #:libpd-controlchange #:libpd-programchange #:libpd-pitchbend #:libpd-aftertouch
   #:libpd-polyaftertouch #:libpd-midibyte #:libpd-sysex #:libpd-sysrealtime

   #:libpd-set-noteonhook #:libpd-set-controlchangehook #:libpd-set-programchangehook
   #:libpd-set-pitchbendhook #:libpd-set-aftertouchhook #:libpd-set-polyaftertouchhook
   #:libpd-set-midibytehook

   #:libpd-start-gui #:libpd-stop-gui #:libpd-poll-gui

   #:libpd-new-instance #:libpd-set-instance #:libpd-free-instance
   #:libpd-this-instance #:libpd-get-instance #:libpd-num-instances

   #:libpd-set-verbose #:libpd-get-verbose))
