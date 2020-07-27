;;; -*- MODE: LISP; READTABLE:COMMON-LISP; PACKAGE:BB1;  BASE:10  -*-

;;;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;                                                                                                                   +
;;;                                  BB1 Version 1.1                                                                  +
;;;                                                                                                                    +
;;;  Copyright (c) 1986  Stanford University, Department of Computer Science, Knowledge Systems Laboratory        +
;;;                                                                                                                    +
;;;  All rights reserved.  This software is distributed under license and may only be used under the                   +
;;;  conditions of that license.                                                                                        +
;;;                                                                                                                    +
;;;  BB1 was designed by Dr. Barbara Hayes-Roth.                                                                    +
;;;                                                                                                                    +
;;;  Micheal Hewett, Alan Garvey, M. Vaughan Johnson, Jr., Robert Schulman, and Reed Hastings                      +
;;;  have worked on this implementation.                                                                              +
;;;                                                                                                                    +
;;;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; 
;;;                                  FILE INFO
;;; 
;;; 
;;;
;;;  BB1OUTPUT.LISP      -     Output routines for the BB1 system.
;;;
;;;  Mike Hewett     02/05/86
;;;                    04/02/86 - MODIFIED BB1ERROR AND BB1WARNING to check *BB1-PRINT-ERRORS*
;;;                                                     and *BB1-PRINT-WARNINGS* before printing.


;;;  These are output routines for use in the BB1 system.
;;;  They are generally low-level routines used internally by
;;;  BB1.  Any user-level routines which do I/O probably use
;;;  these routines in some way.

;;;  Contents:  bb1error
;;;             bb1warning
;;;             bb1message
;;;             bb1-find-output-file
;;;             BB1-WITH-HELP-STREAM       - Displays a message in a temporary window.



;;;----- Package information ------

(in-package 'BB1)

;;;----------------------------


;;;  Source:


;;;  --------  BB1ERROR  -------

(defun bb1error (calling-routine &rest msg-elements)
  "Prints out an error message in a temporary window.
   Used internally by BB1 to signal a serious problem."

  (if *BB1-PRINT-ERRORS*

      (progn (bb1-with-help-stream (err-stream :label "User error detected"
					       :superior (or *bb1-bb-window*
							     *standard-output*))
	       
	       (format err-stream
		       "~%------------ ERROR ------------~%~A has noticed an error in your system.~%~%"
		       calling-routine)
	       
	       (let ((*bb1-message-stream* err-stream))
		 (apply #'bb1message
			msg-elements)
		 )
	       
	       (format err-stream
		       "~%-------------------------------~%")
	       )

	     nil  ; IMPORTANT!  BB1-WITH-HELP-STREAM doesn't return NIL
	     )

      ;;ELSE
      nil
      )
  )



;;;  --------  BB1WARNING  -------

;;;Edited by Byung Suk Lee         29 Jul 87  17:34
(defun bb1warning (calling-routine &rest msg-elements)
  "Prints out a warning message in a temporary window.
   Used internally by BB1 to signal a minor problem.."

  (if *BB1-PRINT-WARNINGS*

      (progn
	(bb1-with-help-stream (err-stream :label "User error detected"
					  :superior (or *bb1-bb-window*
							*standard-output*))
	  
	  (format err-stream
		  "~%------------ WARNING ------------~%~A has noticed a possible error in your system.~%~%"
		  calling-routine)
	  
	  (let ((*bb1-message-stream* err-stream))
	    (apply #'bb1message
		   msg-elements)
	    )
	  
	  (format err-stream
		  "~%-------------------------------~%")
	  )

	nil   ; IMPORTANT!  BB1-WITH-HELP-STREAM doesn't return NIL
	)

      ;;else
      NIL
      )
  )



;;;-------  BB1MESSAGE  -------

(defun bb1message (&rest print-elements)
  "Writes the given items to *BB1-MESSAGE-STREAM*.
   T is carriage return."

  (dolist (item print-elements)
    (if (eql item t)
	(terpri *BB1-MESSAGE-STREAM*)
      (princ item *BB1-MESSAGE-STREAM*)
      )
    )
  )





(defun bb1-find-output-file (&key (new-host      *bb1-host*)
			          (new-directory *bb1-current-directory*)
				  (new-name      *bb1-current-file-name*)
				  (new-type      *bb1-current-file-type*))
  "Uses BB1 defaults if arguments are not given.
   Returns a 'pathname' (which can be opened)."

  (make-pathname :host      new-host
		 :directory new-directory
		 :name      new-name
		 :type      new-type
		 :defaults  nil)
  )



;;; BB1-WITH-HELP-STREAM      move this all to bb1-macs so
                          ;;$output above does the right thing.  reed 17 Dec 86

;;;--- This is necessary because the LMI Lambda and the TI EXPLORER have
;;;--- the macro "WITH-HELP-STREAM" in different packages.  Probably good
;;;--- for generality anyway.

;(defmacro bb1-with-help-stream ((stream . options) &body bod)
;  "Uses SYS:WITH-HELP-STREAM."

;;      (system:with-help-stream (,stream . ,options) ,@bod)      ;  LMI LAMBDA

;;                                                            		; TI EXPLORER
;  `(si:with-help-stream (,stream . ,options) ,@bod)

;  )

