(in-package :scanner)

(defclass scanner ()
  ((text
    :initarg :text
    :accessor text)
   (pos
    :initform 0
    :accessor pos)
   (delimiter
    :initform "\\s+"
    :accessor delimiter)))


(defmethod next ((scanner scanner) &key pattern (delimiter (delimiter scanner)))
  (if pattern
      (multiple-value-bind (start-pattern end-pattern)
          (cl-ppcre:scan pattern (text scanner) :start (pos scanner))
        (when start-pattern
         (let ((token (subseq (text scanner) start-pattern end-pattern)))
           (setf (pos scanner) end-pattern)
           (multiple-value-bind (start-delim end-delim)
               (cl-ppcre:scan delimiter (text scanner) :start (pos scanner))
             (when (and start-delim (= start-delim (pos scanner)))
               (setf (pos scanner) end-delim)))
           token)))
   (multiple-value-bind (start-delim end-delim)
       (cl-ppcre:scan delimiter (text scanner) :start (pos scanner))
     (when (or start-delim (< (pos scanner) (length (text scanner))))
       (let ((token (subseq (text scanner) (pos scanner) start-delim)))
         (setf (pos scanner) (or end-delim (length (text scanner))))
         token)))))

(defmethod next-int ((scanner scanner))
  (parse-integer (next scanner)))

(defmethod next-line ((scanner scanner))
  (next scanner :delimiter (format nil "~%")))

(defmethod reset ((scanner scanner))
  (setf (pos scanner) 0))

(defmethod has-next ((scanner scanner) &key (delimiter (delimiter scanner)))
  (with-previous-position (scanner)
    (let ((next (next scanner :delimiter delimiter)))
      (unless (equal next "")
        next))))

(defmethod has-next-int ((scanner scanner))
  (with-previous-position (scanner)
    (handler-case
        (next-int scanner)
      (sb-int:simple-parse-error () nil))))


(defmacro with-previous-position ((scanner) &body body)
  (with-gensyms (scanner-sym)
    `(let* ((,scanner-sym ,scanner)
            (previous-position (pos ,scanner-sym)))
       (let ((result ,@body))
         (setf (pos ,scanner-sym) previous-position)
         result))))

(defun scan (text)
  (make-instance 'scanner :text text))
