# cl-scanner

is an implementation for context dependent organizing

```lisp
(let ((scanner (scan (format nil "price 15 18~%name item~%"))))
  (loop for type = (next scanner)
      while type
      if (string= type "price")
      collect (list type (next-int scanner) (next-int scanner))
      else
      collect (list type (next scanner))))
;; => (("price" 15 18) ("name" "item"))
```