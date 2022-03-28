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
               (:file "hooks")
               (:file "pure-data")
               (:file "arr")
               (:file "audio")))

;; TODO: Make #:pd/alsa to generate sounds.
