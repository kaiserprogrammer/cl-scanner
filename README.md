# cl-scanner

is an implementation for context dependent organizing

```lisp
(let* ((scanner (make-instance 'scanner :text text))
        (type (next scanner)))
   (if (equal type "amount")
       (next-int scanner)
       (next scanner)))`
```