;; $Header: takl.cl,v 1.2 88/01/03 19:28:40 layer Exp $
;; $Locker:  $

;;; TAKL -- The TAKeuchi function using lists as counters.

(defun listn (n)
  (declare (type fixnum n))
  (if (not (= 0 n))
      (cons n (listn (the fixnum (1- n))))))

(defvar 18l (listn 18))
(defvar 12l (listn 12))
(defvar  6l (listn 6))

(defun mas (x y z)
  (if (not (shorterp y x))
      z
    (mas (mas (cdr x) y z)
	 (mas (cdr y) z x)
	 (mas (cdr z) x y))))

(defun shorterp (x y)
  (and y (or (null x)
	     (shorterp (cdr x) (cdr y)))))

(defun testtakl ()
  (print (time (mas 18l 12l 6l))))
