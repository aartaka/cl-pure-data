;;;; arr.lisp

(in-package #:cl-pure-data)

(defclass arr ()
  ((name :initarg :name
         :reader name
         :initform (alexandria:required-argument "name"))))

(defun arr (name)
  "Construct a new array from the PD image, referencing a PD array by NAME."
  (make-instance 'arr :name name))

(defmethod contents ((array arr) &optional (start 0) end)
  "Get contents of ARRAY, or a subsequence of those, starting from START and until END.

If end is nil, read all of the array from START index.

Does bound-checking, raises errors and returns nil in case of problems.

Setf-able."
  (let* ((pd-array-length (libpd:libpd-arraysize (name array)))
         (end (or end (1- pd-array-length)))
         (size (- end start))
         (contents (make-array size :element-type 'float :initial-element 0.0))
         (array-type (list :array :float size))
         (foreign (cffi:foreign-array-alloc contents array-type)))
    (when (or (> end pd-array-length)
              (< end start)
              (minusp start))
      (error "Expected indices in between 0 and ~d, got (~d, ~d)"
             (1- pd-array-length) start end))
    (unwind-protect
         (if (zerop (libpd:libpd-read-array foreign (name array) start size))
             (cffi:foreign-array-to-lisp foreign array-type)
             nil)
      (cffi:foreign-array-free foreign))))

(defmethod (setf contents) ((new-value array) (array arr) &optional (start 0) end)
  (let* ((pd-array-length (libpd:libpd-arraysize (name array)))
         (end (or end (1- pd-array-length)))
         (size (- end start))
         (array-type (list :array :float size))
         (new-value (map 'vector (lambda (e) (coerce e 'single-float)) new-value)))
    (when (or (> end pd-array-length)
              (< end start)
              (minusp start))
      (error "Expected indices in between 0 and ~d, got (~d, ~d)."
             (1- pd-array-length) start end))
    (let ((foreign (cffi:foreign-array-alloc new-value array-type)))
      (unwind-protect
           (if (zerop (libpd:libpd-write-array (name array) start foreign size))
               new-value
               nil)
        (cffi:foreign-array-free foreign)))))

(defmethod elem ((array arr) (index integer))
  "Get ARRAY element by INDEX. Setf-able."
  (elt (contents array index (1+ index)) 0))

(defmethod (setf elem) ((new-value number) (array arr) (index integer))
  (setf (contents array index (1+ index)) (vector new-value)))
