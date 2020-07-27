(in-package 'compiler)
;;in cmpeval.lsp

;(load "/usr/local/src/skcl/cmpnew/cmpif.lsp")
;(load "/usr2/skcl/lsp/make-declare.lsp")
(defun cc (file) (si::proclaim-file file) (compile-file file :c-file t))
;+        ((and (setq fd (get fname 'co1))
;+	      (funcall fd args)))
;        ((and (setq fd (get fname 'c1)) (inline-possible fname))
;         (funcall fd args))
;        ((and (setq fd (get fname 'c1conditional))

(si::putprop 'schar 'co1schar 'co1)

(defun co1schar (args)
   (and (listp (car args)) (not *safe-compile*)
	(cdr args)
	(eq (caar args) 'symbol-name)
	(c1expr `(aref (the string ,(second (car args)))
			,(second args)))))

(si::putprop 'schar 'co1schar 'co1)

(defconstant bil #\a)
(defun fo ()(disassemble '(defun joe (a) (eql bil (schar (symbol-name a) 3)))))

(defun replace-constant (lis &aux found tem)
  (do ((v lis (cdr v)))
      ((null v)  found)
      (cond ((and (constantp (car v))
		  (or (numberp (setq tem (eval (car v))))
		      (characterp tem)))
	     (setq found t) (setf (car v)tem)))))

;;optimize common case of eql with constant 
(defun co1eql (args)
  (or (cdr args) (not *safe-compile*) (return nil))
  (cond ((replace-constant args)
	 (cond ((characterp (second args))
		(setq args (reverse args))))
	 (cond ((characterp (car args))
		(c1expr `(= (char-code ,(car args))
			    (char-code ,(second args)))))))))

	 
(si::putprop 'eql 'co1eql 'co1)		    

(defun c1list-condition (args) (and (= *space* 0) (< (length args) 2)))

(push '((fixnum fixnum) fixnum nil nil
        "(#0)%(#1)")
      (get 'rem 'inline-always))


(defun co1typep (args)
  (let* ((x (car args))
	 (type (and (consp (second args))
		    (eq (car (second args)) 'quote)
		    (second (second args))))
	 (new
	   (and type
		(case type
		      (fixnum `(si::fixnump ,x))
		      (float `(floatp ,x))
		      (short-float `(short-float-p ,x))
		      (long-float `(long-float-p ,x))
		      (integer `(integerp ,x))
		      (cons `(consp ,x))
		      (t nil)))))
	 (and new (c1expr new))))


(si::putprop 'typep 'co1typep 'co1)		    

(push '((t) boolean nil nil
        "type_of(#0)==t_shortfloat")
      (get 'short-float-p 'inline-always))
(push '((t) boolean nil nil
        "type_of(#0)==t_longfloat")
      (get 'short-float-p 'inline-always))
		
(push '((t) boolean nil nil
        "type_of(#0)==t_fixnum")
      (get 'si::fixnump 'inline-always))

(push '((t) fixnum t nil "length(#0)") (get 'length 'inline-always))

(push '((fixnum fixnum) fixnum nil nil
        "@01;(#0>=0&&#1>0?(#0)/(#1):ifloor(#0,#1))")
      (get 'floor 'inline-always))


(push '((t t t) t t t
	"putf(#0,#1,#2)")
      (get 'si::put-f 'inline-always))
	

;(push '((fixnum fixnum) fixnum nil nil "(#0)/(#1)")
; 
;      (get 'floor 'inline-always))


