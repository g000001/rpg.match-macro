;;;; test.lisp -*- Mode: Lisp;-*- 

(cl:in-package rpg.match-macro.internal)
;; (in-readtable :rpg.match-macro)

(def-suite rpg.match-macro)
(in-suite rpg.match-macro)


#|(?*-expander '(*x 
               ($r ?prep for-in-from-memq)
               *l 
               ($r ?verb for-do-collect-append-etc-memq)
               *form))|#

#|(*FORM ?VERB *L ?PREP *X)|#
;; NIL

;=>  (LIST (* (PUSH *X)) ?PREP (* (PUSH *L)) ?VERB (* (PUSH *FORM)))
;    (*FORM ?VERB *L ?PREP *X)
;    ((#'FOR-DO-COLLECT-APPEND-ETC-MEMQ ?VERB) (#'FOR-IN-FROM-MEMQ ?PREP))



;;; *EOF*

