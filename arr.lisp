;;;; arr.lisp

(in-package #:cl-pure-data)

(defclass arr ()
  ((name :initarg :name
         :initform (alexandria:required-argument "name"))))

(defun arr (name)
  "Construct a new array from the PD image, referencing a PD array by NAME."
  (make-instance 'arr :name name))

(defmethod contents ((array arr) &optional (start 0) end)
  "Get contents of ARRAY, or a subsequence of those, starting from START and until END.

If end is nil, read all of the array from START index.

Does bound-checking, raises errors and returns nil in case of problems.

Setf-able."
  (let* ((pd-array-length (libpd:libpd-arraysize (slot-value array 'name)))
         (end (or end (1- pd-array-length)))
         (size (- end start))
         (array-type (cffi::ensure-parsed-base-type (list :array :float size))))
    (when (or (> end pd-array-length)
              (< end start)
              (minusp start))
      (error "Expected indices in between 0 and ~d, got (~d, ~d)"
             (1- pd-array-length) start end))
    (cffi:with-foreign-array
        (arr (make-array size :element-type 'float :initial-element 0.0) array-type)
      (if (zerop (libpd:libpd-read-array arr (slot-value array 'name) start size))
          (cffi:foreign-array-to-lisp arr array-type)
          nil))))

(defmethod (setf contents) ((new-value array) (array arr) &optional (start 0) end)
  (let* ((pd-array-length (libpd:libpd-arraysize (slot-value array 'name)))
         (end (or end (1- pd-array-length)))
         (size (- end start))
         (array-type (cffi::ensure-parsed-base-type (list :array :float size)))
         (new-value (map 'vector (lambda (e) (coerce e 'single-float)) new-value)))
    (when (or (> end pd-array-length)
              (< end start)
              (minusp start))
      (error "Expected indices in between 0 and ~d, got (~d, ~d)."
             (1- pd-array-length) start end))
    (cffi:with-foreign-array (arr new-value array-type)
      (if (zerop (libpd:libpd-write-array (slot-value array 'name) start arr size))
          new-value
          nil))))

(defmethod elem ((array arr) (index integer))
  "Get ARRAY element by INDEX. Setf-able."
  (elt (contents array index (1+ index)) 0))

(defmethod (setf elem) ((new-value number) (array arr) (index integer))
  (setf (contents array index (1+ index)) (vector new-value)))
