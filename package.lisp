(defpackage :scanner
  (:use :cl :alexandria)
  (:export
   :scanner
   :next
   :next-int
   :next-line
   :reset
   :delimiter
   :scan
   :has-next
   :has-next-int))
