;;;; Trivial in-memory byte output stream for CLISP. Not optimized.

(defclass buffer-output-stream (fundamental-binary-output-stream)
  ((buffer :initform (make-array 0 :fill-pointer 0 :adjustable t
                                   :element-type '(unsigned-byte 8)))))

(defmethod stream-write-byte ((stream buffer-output-stream) byte)
  (vector-push-extend byte (slot-value stream 'buffer))
  byte)

(defmethod stream-write-byte-sequence ((stream buffer-output-stream) byte-array
                                       &optional start end no-hang interactive)
  (declare (ignore start end no-hang interactive))
  (let ((buffer (slot-value stream 'buffer)))
    (loop for byte across byte-array do (vector-push-extend byte buffer))
    byte-array))

(defmacro with-output-to-byte-vector ((stream) &body body)
  `(let ((,stream (make-instance 'buffer-output-stream)))
     (progn ,@body)
     (slot-value ,stream 'buffer)))

;;

(defclass buffer-input-stream (fundamental-binary-input-stream)
  ((buffer :initarg :buffer)
   (position :initform 0)))

(defmethod stream-read-byte ((stream buffer-input-stream))
  (with-slots (buffer position) stream
    (unless (< position (length buffer))
      (error 'end-of-file))
    (prog1 (elt buffer position)
      (incf position))))

(defmethod stream-read-byte-sequence ((stream buffer-input-stream) byte-array
                                      &optional start end no-hang interactive)
  (declare (ignore start end no-hang interactive))
  (with-slots (buffer position) stream
    (unless (<= position (- (length buffer) (length byte-array)))
      (error 'end-of-file))
    (dotimes (i (length byte-array))
      (setf (elt byte-array i) (elt buffer (+ position i))))
    (incf position (length byte-array))
    (length byte-array)))

(defmacro with-input-from-byte-vector ((stream buffer) &body body)
  `(let ((,stream (make-instance 'buffer-input-stream :buffer ,buffer)))
     ,@body))
