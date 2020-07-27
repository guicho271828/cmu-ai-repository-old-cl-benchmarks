(allocate 'cons 2000 t)
(allocate 'fixnum 200 t)
;;so that the lisps do the same thing.
(setq *print-pretty* nil)
;;If running this on a machine without a floating point chip delete this
(when (and (boundp 'compiler::*cc*)
         (search "gcc" compiler::*cc*)
	 (search "msoft-float" compiler::*cc*))
      (setq compiler::*cc* "gcc -DVOL=volatile ")
;      (setq compiler::*register-min* 100000) (setq compiler::*cc* "cc -f68881 -DVOL= ")      (print "using cc")
      )
	 
    