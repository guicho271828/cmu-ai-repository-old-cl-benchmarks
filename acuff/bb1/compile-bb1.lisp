;;; Compile enough BB1 to test.  File made by Rich Acuff with help from
;;; Mike Hewett.

(in-package "BB1")
(use-package "USER")

(shadow 'if 'bb1)

(setq si:inhibit-fdefine-warnings t)

(defmacro bb1::if (test then &rest elses)
  `(lisp:if ,test ,then (progn ,@elses)))

(defmacro ignore (&rest ignore)
   (declare (ignore ignore))
   nil
   )

(compile-file "bb1var.lisp")
(load "bb1var")
(compile-file "bb1bb-macs.lisp")
(load "bb1bb-macs")
(compile-file "bb1utilities.lisp")
(load "bb1utilities")
(compile-file "bb1record.lisp")
(load "bb1record")
(compile-file "bb1record-fns.lisp")
(load "bb1record-fns")
(compile-file "bb1bb-fns.lisp")
(load "bb1bb-fns")
(compile-file "bb1-fns.lisp")
(load "bb1-fns")
(compile-file "bb1output.lisp")
(load "bb1output")
(compile-file "testbbedit.lisp")
(load "testbbedit")
