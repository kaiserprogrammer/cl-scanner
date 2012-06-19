(defpackage :scanner-test
  (:use :cl :scanner :fiveam))
(in-package :scanner-test)

(def-suite scanner)
(in-suite scanner)

(test no-text
  (let ((scanner (scan "")))
    (is (equal nil (next scanner)))
    (is (equal nil (next scanner)))))

(test no-next-left
  (let ((scanner (scan "abc ")))
    (is (equal "abc" (next scanner)))
    (is (equal nil (next scanner)))))

(test has-next
  (let ((scanner (scan "abc abd")))
    (is (equal "abc" (has-next scanner)))
    (is (equal "abc" (next scanner)))
    (is (equal "abd" (has-next scanner)))
    (is (equal "abd" (next scanner)))))

(test has-next-int
  (let ((scanner (scan "abc 10")))
    (is (not (has-next-int scanner)))
    (is (equal "abc" (next scanner)))
    (is (has-next-int scanner))
    (is (= 10 (next-int scanner)))))

(test scan-with-context
  (let ((data
         "begin 2005 04 02 1043 meeting Smith, John
end 2005 04 02 1204 Smith, John
begin 2005,04,02 1300 work Eubanks, Brian
end 2005 04 02 2120 Eubanks, Brian
alarm 2005 06 02 2301 At the beginning"))
    (let ((scanner (scan data)))
      (is (equal "begin" (next scanner)))
      (is (= 2005 (next-int scanner)))
      (is (= 4 (next-int scanner)))
      (is (= 2 (next-int scanner)))
      (is (= 1043 (next-int scanner)))
      (is (equal "meeting" (next scanner)))
      (is (equal "Smith, John" (next-line scanner)))
      (is (equal "end 2005 04 02 1204 Smith, John" (next-line scanner)))
      (reset scanner)
      (is (equal "begin 2005 04 02 1043 meeting Smith, John" (next-line scanner)))
      (next-line scanner)
      (is (equal "begin" (next scanner)))
      (setf (delimiter scanner) ",")
      (is (equal 2005 (next-int scanner)))
      (is (equal 4 (next-int scanner)))
      (setf (delimiter scanner) "\\s+")
      (is (equal 2 (next-int scanner)))
      (is (equal "1300 work Eubanks, Brian" (next-line scanner)))
      (is (equal "end" (next scanner)))
      (is (equal "2005" (next scanner)))
      (is (equal "04" (next scanner)))
      (is (equal "02" (next scanner)))
      (is (equal "2120" (next scanner)))
      (is (equal "Eubanks," (next scanner)))
      (is (equal "Brian" (next scanner)))
      (is (equal "alarm" (next scanner))))))

(run!)
