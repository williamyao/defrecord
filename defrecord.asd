;;;; Simpler class definitions.

(in-package #:common-lisp-user)
(defpackage #:defrecord/asdf
  (:use #:cl #:asdf))
(in-package #:defrecord/asdf)

(defsystem :defrecord
  :name "DEFRECORD"
  :description "Simpler class definitions"
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "defrecord")))
