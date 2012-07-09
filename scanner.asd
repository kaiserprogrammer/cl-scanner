(cl:defpackage :scanner-system
  (:use :cl :asdf))
(cl:in-package :scanner-system)

(defsystem :scanner
  :version "0.1"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :description "A scanner for context depending search"
  :depends-on (:cl-ppcre
               :alexandria)
  :components ((:file "package")
               (:file "scanner" :depends-on ("package"))))
