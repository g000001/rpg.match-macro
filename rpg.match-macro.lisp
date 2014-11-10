;;;; rpg.match-macro.lisp -*- Mode: Lisp;-*- 

(cl:in-package :rpg.match-macro.internal) 


;; (in-readtable :rpg.match-macro)


;;; "rpg.match-macro" goes here. Hacks and glory await!

;(match-macro (if) (*form1 then *form2 )
; (cond ((%match '(*form2 else *form3) *form2)
;	(code (cond (*form1 *form2)
;		    (t *form3))))
;       (t (code (cond (*form1 *form2))))))


(defun do-code (x)
  (cond ((null x) nil)
        ((atom x)
         (let ((char1 (and (symbolp x)
                           (char (string x) 0))))
           (cond ((member char1 '(#\? #\*)) x)
                 (t (list 'quote x)))))
        ((and (symbolp (car x))
              (char= #\* (char (string (car x)) 0)))
         `(append ,(do-code (car x)) ,(do-code (cdr x))))
        (t `(cons ,(do-code (car x)) ,(do-code (cdr x)))))) 


(defmacro code (x)
  (do-code x)) 


(defmacro %match (pat arg)
  (cons-match-clause (eval pat) arg)) 


#|(defmacro match-macro ((name &rest name-args) args &body body)
  (declare (ignore name-args))
  `(macrolet ((%match (pat arg)
                (cons-match-clause pat arg)))
     (defmacro ,name (&rest args)
       (let (,@(collect-%match-vars `(%match ',args)))
         (%match '(,@args) args)
         (progn                        #|(let (,@(set-difference 
           (collect-%match-vars body)
           (collect-%match-vars `(%match ',args)))))|#
           ,@body)))))|# 


(defmacro match-macro ((name &rest name-args) args &body body)
  (declare (ignore name-args))
  (let ((match-vars (remove-duplicates (append (collect-%match-vars `(%match ',args))
                                               (collect-%match-vars body)))))
   `(defmacro ,name (&rest args)
      (let (,@match-vars)
        (declare (ignorable ,@match-vars))
        (%match '(,@args) args)
        (progn                        
          ,@body))))) 


(defun collect-%match-vars (xpr)
  (cond ((atom xpr) nil)
        ((eq '%match (car xpr))
         (append (nth-value 1 (?*-expander (eval (cadr xpr))))
                 (collect-%match-vars (cdr xpr))))
        ((consp (car xpr))
         (append (collect-%match-vars (car xpr))
                 (collect-%match-vars (cdr xpr))))
        (T (collect-%match-vars (cdr xpr))))) 


#|(defun cons-match-clause (pat arg)
  (multiple-value-bind (pat vars fns)
                       (?*-expander pat)
    (let ((gvars (mapcar (lambda (g) (declare (ignore g)) (gensym)) vars)))
      `(let (,@gvars)
         (and (*:toad-case ,arg
                ,pat
                *:-> ,@(and fns 
                            `((when (and ,@(mapcar (lambda (x)
                                                     `(funcall ,(car x) ,(cadr x)))
                                                   fns)))))
                (progn (setq ,@(mapcan #'list gvars vars)) T))
              (progn (setq ,@(mapcan #'list vars gvars))
                     T))))))|#


(defun cons-match-clause (pat arg)
  (multiple-value-bind (pat vars fns)
                       (?*-expander pat)
    (declare (ignore fns))
    (let ((gvars (mapcar (lambda (g) (declare (ignore g)) (gensym)) vars)))
      `(let (,@gvars)
         (and (toad-case1 (reverse ,arg)
                (,pat
                 (progn 
                   (setq ,@(mapcan #'list 
                                   (reverse gvars)
                                   (reverse
                                    (mapcar (lambda (x)
                                              (if (char= #\* (char (symbol-name x) 0))
                                                  `(reverse ,x)
                                                  x))
                                            vars))))
                   T)))
              (progn (setq ,@(mapcan #'list vars gvars))
                     T)))))) 


#|(cons-match-clause '(*x 
                     ($r ?prep for-in-from-memq)
                     *l 
                     ($r ?verb for-do-collect-append-etc-memq)
*form) 'arg)|#

#|(cons-match-clause '(*x 
                     ($r ?prep for-in-from-memq)
                     *l 
                     ($r ?verb for-do-collect-append-etc-memq)
                     *form) 'arg)|#

#||
(let ((*form '()))
  (toad-case1 '(+ 1 2 3)
    ((list '+ a b c)
     (list *form a b c))))

(toad-ecase 3
  4 -> :iv
  3 -> :iii)

(toad-ecase1 3
  (4 :iv)
  (3 :iii))


(toad-case '(1 2 -42 3)
  (list (* t) x (* t)) -> (when (< x 0)) x)
;=>  -42

(toad-case1 '(1 2 -42 3)
  ((list (* t) x (* t)) (when (< x 0)) x))
;=>  -42


(toad-case (list 1 2 'foo "bar" :baz)
  (list (* (or (and (satisfies #'numberp)
                    (push numbers))
               (and (satisfies #'symbolp)
                    (push symbols))
               (push others))))
    -> (values numbers symbols others))
;=>  (1 2)
;    (FOO :BAZ)
;    ("bar")


(toad-case1 (list 1 2 'foo "bar" :baz)
  ((list (* (or (and (satisfies #'numberp)
                     (push numbers))
                (and (satisfies #'symbolp)
                     (push symbols))
                (push others))))
   (values numbers symbols others)))
;=>  (1 2)
;    (FOO :BAZ)
;    ("bar")


||#


#|(match-macro (read-until) (?step ($r ? (lambda(q) (memq q '(isin ε)))) ?set do *form)
 ((lambda (*form1 *form2)
   (%match '(*form1 return *form2) *form)
   (code (do ((?step (read)(read)))
	     ((member ?step ?set) *form2)
	     *form1)))
  *form nil))|#


#|(defun ?*-expander (expr)
  (let ((vars '() )
        (fns '() ))
    (flet ((collect-matchv (x)
             (typecase x
               (cl:symbol (case (char (symbol-name x) 0)
                              (#\? (push x vars) x)
                              (#\* (push x vars) `(+ (push ,x)))
                              (otherwise `',x)))
               ((cl:cons (eql $r) *)
                (*:toad-case x
                  (list '$r x fn) *:-> (progn 
                                         (push x vars)
                                         (push `(#',fn ,x) fns)
                                         x))))))
      (values
       (typecase expr
         (cl:cons `(list ,@(mapcar #'collect-matchv expr)))
         (cl:symbol (collect-matchv expr))
         (cl:t expr))
       vars
       fns))))|#

(defun ?*-expander (expr)
  (let ((vars '() )
        (fns '() ))
    (flet ((collect-matchv (x)
             #|(typecase x
               (cl:symbol (case (char (symbol-name x) 0)
                              (#\? (push x vars) x)
                              (#\* (push x vars) `(+ (push ,x)))
                              (otherwise `',x)))
               ((cl:cons (eql $r) *)
                (toad-case1 x
                  ((list guardfn x fn)
                   (when (string-equal '$r guardfn))
                   (progn 
                     (push x vars)
             `(and (satisfies #',fn) ,x))))))|#
             (toad-case1 x
               (x (when (symbolp x)) (case (char (symbol-name x) 0)
                                       (#\? (push x vars) x)
                                       (#\* (push x vars) `(+ (push ,x)))
                                       (otherwise `',x)))
               ((list guardfn x fn) 
                (when (string-equal '$r guardfn))
                (progn 
                  (push x vars)
                  `(and (satisfies #',fn) ,x))))))
      (values
       (typecase expr
         (cl:cons `(list ,@(nreverse (mapcar #'collect-matchv expr))))
         (cl:symbol (collect-matchv expr))
         (cl:t expr))
       vars
       fns)))) 




#|(RPG.MATCH-MACRO.INTERNAL::?*-EXPANDER '(*X ($R ?PREP FOR-IN-FROM-MEMQ) *L
                                         ($R ?VERB FOR-DO-COLLECT-APPEND-ETC-MEMQ) *FORM))|#
;=>  (LIST (+ (PUSH *FORM)) (AND (SATISFIES #'FOR-DO-COLLECT-APPEND-ETC-MEMQ) ?VERB)
;          (+ (PUSH *L)) (AND (SATISFIES #'FOR-IN-FROM-MEMQ) ?PREP) (+ (PUSH *X)))
;    (*FORM ?VERB *L ?PREP *X)
;    NIL


;;; *EOF* 

