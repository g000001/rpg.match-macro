;;;; rpg.match-macro.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :rpg.match-macro
  :serial t
  :depends-on (:fiveam
               :named-readtables)
  :components ((:file "package")
               (:file "rpg.match-macro")
               (:file "test")))


(defmethod perform ((o test-op) (c (eql (find-system :rpg.match-macro))))
  (load-system :rpg.match-macro)
  (or (flet (($ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall ($ :fiveam :run) ($ :rpg.match-macro.internal :rpg.match-macro))))
          (funcall ($ :fiveam :explain!) result)
          (funcall ($ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; *EOF*
