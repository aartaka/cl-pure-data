;;;; cl-pure-data.asd

(asdf:defsystem #:cl-pure-data
  :description "Bindings for libpd, the data-processing interface for PureData."
  :author "Artyom Bologov"
  :license  "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:alexandria)
  :components ((:file "package")
               (:file "libpd")
               (:file "globals")
               (:file "hooks")
               (:file "pure-data")
               (:file "arr")
               (:file "audio")
               (:file "proxy")))

(asdf:defsystem #:cl-pure-data/alsa
  :description "The ALSA-enabled playback extension for cl-pure-data."
  :serial t
  :depends-on (#:cffi #:cl-pure-data)
  :components ((:file "alsa")))
