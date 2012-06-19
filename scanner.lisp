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


(defmethod next ((scanner scanner) &optional delimiter)
  (unless delimiter
    (setf delimiter (delimiter scanner)))
  (multiple-value-bind (start-delim end-delim)
      (cl-ppcre:scan delimiter (text scanner) :start (pos scanner))
    (let ((token (subseq (text scanner) (pos scanner) start-delim)))
      (setf (pos scanner) end-delim)
      token)))

(defmethod next-int ((scanner scanner))
  (parse-integer (next scanner)))

(defmethod next-line ((scanner scanner))
  (next scanner (format nil "~%")))

(defmethod reset ((scanner scanner))
  (setf (pos scanner) 0))
