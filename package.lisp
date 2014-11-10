;;;; package.lisp -*- Mode: Lisp;-*- 

(cl:in-package :cl-user)


(defpackage :rpg.match-macro
  (:use)
  (:export :match-macro :%match
           :code))


(defpackage :rpg.match-macro.internal
  (:use :rpg.match-macro :cl :fiveam
   :toadstool))

