;; $Header: fprint.cl,v 1.2 88/01/03 19:28:31 layer Exp $
;; $Locker:  $

;;; FPRINT -- Benchmark to print to a file.

(defvar test-atoms '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56 klmnop67 
			      mnopqr78 opqrst89 qrstuv90 stuvwx01 uvwxyz12 
			      wxyzab23 xyzabc34 123456ab 234567bc 345678cd 
			      456789de 567890ef 678901fg 789012gh 890123hi))

(defun init-aux (m n atoms)
  (declare (fixnum m n))
  (cond ((= m 0) (pop atoms))
	(t (do ((i n (the fixnum (- i 2)))
		(a ()))
	       ((< i 1) a)
	     (declare (fixnum i))
	     (push (pop atoms) a)
	     (push (init-aux (the fixnum (1- m)) n atoms) a)))))

(defun fprint-init (m n atoms)
  (let ((atoms (subst () () atoms)))
    (do ((a atoms (cdr a)))
	((null (cdr a)) (rplacd a atoms)))
    (init-aux m n atoms)))

(defvar test-pattern (fprint-init 6. 6. test-atoms))

(defun fprint ()
  (if (probe-file "/tmp/fprint.tst")
      (delete-file "/tmp/fprint.tst"))
  (let ((stream (open "/tmp/fprint.tst" :direction :output)))
    (print test-pattern stream)
    (close stream)))

(defun testfprint ()
  (print (time (fprint))))
