(defun ht (&rest list)
  (let ((hash-table (make-hash-table)))
    (loop (unless (and list (cdr list)) (return hash-table))
          (let ((key (car list)) (value (cadr list)))
            (setf (gethash key hash-table) value)
            (setf list (cddr list))))))

;;

(defun encoding-test (value)
  (let ((filename "deleteme"))
    (with-open-file (stream filename :element-type '(unsigned-byte 8)
                                     :direction :output :if-exists :supersede)
      (write-toplevel value stream))
    (with-open-file (stream filename :element-type '(unsigned-byte 8))
      (read-toplevel stream))))

(encoding-test (ht "foo" (list "bar" (ht "baz" 12 "qux" t) nil)))
