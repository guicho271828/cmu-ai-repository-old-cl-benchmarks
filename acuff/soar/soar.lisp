;;; -*- Mode:Common-Lisp; Base:10 -*-

;;; Concatenated from type module "utility" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/utility/release/specials.lisp".
;;; -*-mode: lisp; package: user -*-
;;; Utilities specials listing.

(proclaim '
(special *accept-file* *accepting-input* *action-closure* *action-count*
     *added-tokens* *always-learn* *atrace* *begin-build-time*
     *begin-time* *break-flag* *brknames* *brkpts* *brkrun* *c1* *c10*
     *c11* *c12* *c13* *c14* *c15* *c16* *c2* *c3* *c4* *c5* *c6* *c7*
     *c8* *c9* *carray* *cav* *cavi* *char-mode* *chunk-all-paths*
     *chunk-classes* *chunk-free-problem-spaces* *chunks* *condition-vars*
     *conflict-set* *constants* *context* *context-field-found*
     *context-stack* *critical* *curcond* *current-branch* *current-goal*
     *current-production-trace* *current-token* *current-wm* *cycle-count*
     *data-matched* *data-part* *date-created* *decide-count*
     *decide-trace* *decision-preferences* *default-multi*
     *default-user-select* *elaborations-count* *elapsed-build-time*
     *elapsed-time* *execute-path* *feature-count* *filters*
     *first-action* *first-node* *first-remove* *flag-part*
     *free-gensym-list* *free-pname-list* *full-print-id* *global-pair*
     *gtrace* *halt-flag* *impasse-subset-not-equal* *in-rhs* *indent*
     *init-wm* *initial-actions* *input-wme* *instance-attributes*
     *label-bindings* *last-arg* *last-node-on-bus* *last-obj-id*
     *last-pname* *last-tag* *last-value-var* *learn-ids* *learning*
     *limit-cs* *limit-token* *loading-default* *ltrace*
     *max-chunk-conditions* *max-elaborations* *max-recurse* *max-token*
     *max-wm* *matrix* *mem-array-size* *minor-version* *multi-attribute*
     *necessity-preference-values* *never-learn* *new-chunks*
     *order-trace* *otrace* *p-name* *p-type* *pand-id* *pand-save-id*
     *parallel-not-suspend* *path* *pcount* *pfired* *phase* *pnames*
     *potential-candidates* *potential-conditions* *ppline*
     *previously-traced* *print-attribute-list* *print-learn*
     *print-pname* *print-spo-list* *prod-count* *prop-names* *ptrace*
     *public-version* *real-cnt* *record* *recording* *refracts*
     *release-number* *remaining-cycles* *remaining-decide*
     *removed-tokens* *rhs-bound-vars* *save-class-list* *save-path*
     *secondary-variables* *select-equal* *soar-actions* *sp-forms*
     *spo-default-depth* *subgoal-tabs* *subgoaled* *subnum*
     *suspected-duplicates* *total-token* *total-wm* *trace-file*
     *trace-number* *tracep* *tracep-list* *ttrace* *unbound*
     *used-char-list* *used-gensym-list* *used-var-list* *user-ids*
     *user-pnames* *variable-memory* *vars* *version-number* *virtual-cnt*
     *warning* *watch-free-problem-spaces* *wm* *wm-filter*
     *wme-list-stack* *wmpart-list* *write-file* *wtrace* *xs*)
)

;;; Concatenated from type module "utility" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/utility/release/compat.lisp".
;;;-*-mode: lisp; syntax: common-lisp; package: user; base: 10. -*-
;;;  Macros and functions to help in the conversion of the Soar system
;;;     from Interlisp to Common Lisp, modified from Zetalisp 
;;;	compatibility file.

; Added for Common Lisp (weren't in Zetalisp compatibility file)

#-:ti
(defmacro selectq (tag &rest clauses)
   `(case ,tag ,@(butlast clauses) (otherwise ,(car (last clauses)))))

#-:ti
(defmacro neq (x y)
   `(not (eql ,x ,y)))

#-:ti
(defmacro difference (&body z)
   (cons '- z))

#-:ti
(defmacro plus (&body z)
   (cons '+ z))

#-:ti
(defmacro times (&body z)
   (cons '* z))

#-:ti
(defmacro *throw (&body z)
   (cons 'throw z))

#-:ti
(defmacro time-difference (&body z)
   (cons '- z))

#-:ti
(defmacro greaterp (&body z)
   (cons '> z))

#-:ti
(defmacro quotient (&body z)
   (cons '/ z))

#-:ti
(defmacro fix (x)
   `(floor ,x))

#-:ti
(defmacro flatc (x)
   `(length (princ-to-string ,x)))

#-:ti
(defmacro lessp (&body z)
   (cons '< z))

#-:ti
(defmacro remainder (n d)
   `(rem ,n ,d))

;;; End Common Lisp additions to Zetalisp file.

;;; Cursor-position-tracking output functions for Common Lisp.  Currently
;;; they will only work if only one output stream is used at a time.  These
;;; routines will only work if soarterpri is called at the beginning of
;;; a batch of prints to set the cursor position to zero. -GRY, 9/19/86

(defvar *current-cursor-pos* 0
    "Current output cursor position")

(defun nwritn (x)
   (declare (ignore x))
   *current-cursor-pos*)

(defun soarterpri (&optional (stream *standard-output*))
    (setq *current-cursor-pos* 0)
    (terpri stream))

(defun soarprinc (thing &optional (stream *standard-output*))
    (setq *current-cursor-pos* (+ *current-cursor-pos* (flatc thing)))
    (princ thing stream))

;;; Do not print a decimal point after integers
(setq *print-radix* nil)

; See the precautions listed at the definition of soarcarsort, below.
(defmacro soarsort (list)
	`(sort ,list #'string-lessp))

(defmacro dremove (item list)
	`(delete ,item ,list))
;; "REMOVE" should be identical to remove.

(defmacro drain (&body z)
	(cons 'finish-output z))

(defmacro soarlistp (z)
    `(consp ,z))

	;; PRINC and SETF should be OK as they are

(defmacro soarputprop (n v a)
	`(setf (get ,n ,a) ,v))

(defun soaraddprop (n v a)
	(soarputprop n (cons v (get n a)) a))

(defmacro soarmapc (func list)
	`(mapc ,func ,list))

(defmacro soarmapcar (func list)
	`(mapcar ,func ,list))

(defmacro soarmapconc (func list)
	`(mapcan ,func ,list))

(defmacro soarpush (item list)
	`(push ,item ,list))

	;;  The arithmetic functions. Common lisp should automatically coerce

(defmacro idifference (x y)
	`(- ,x ,y))

(defmacro iplus (x y)
	`(+ ,x ,y))
(defmacro add (x y)
	`(+ ,x ,y))

(defmacro iquotient (x y)
	`(truncate (/ ,x ,y)))

; Number comparison function, using eql instead of = for now since some
; places call it with non-numbers nonetheless.
(defmacro eqp (&body z)
	(cons 'eql z))

(defmacro soar-copy (thing)
	`(copy-tree ,thing))

(defmacro flength (x)
	`(length ,x))


	;; The next two macros must override their interlisp definitions
	;;	which contain undefined functions FNTH and ADD1.
(defmacro soarnth (ind list)
	`(nth ,ind ,list))

(defmacro soarnthcdr (ind list)
	`(nthcdr ,ind ,list))

(defmacro time-conversion (x)
  `(/ ,x ,(coerce internal-time-units-per-second 'float)))

(defmacro alwaystime ()
	`(get-internal-run-time))

(defvar *soar-gensym-counter* 0 
  "The counter by which soar generates gensyms.")

(defmacro init-gensym (&body z)
	(declare (ignore z))
  ;`(gensym 0)  BGM
   `(setq *soar-gensym-counter* 0))

(defun soargensymbol (x)
  ; Gensym replaced. BGM
;(intern (symbol-name (gensym (write-to-string x :escape nil :case :upcase))))
 (intern (concatenate 'string (write-to-string x :escape nil :case :upcase)
		      (format () "~A" (incf *soar-gensym-counter*)))))

(defmacro soar-i-to-s (n)
    `(format nil "~d" ,n))

(defun soarpack (&rest z)
   (intern (apply #'concatenate
		  (cons 'string
		      (mapcar #'(lambda (x)
				   (write-to-string x :escape nil
					    	      :case :upcase))
			      z)))))

(defmacro soar-do (&body z)
	(cons 'do z))

; Code that uses this must take care that the keys are numbers.
; This was the case when soarcarsort was defined.  This same precaution
; must be taken with soarsort, but it must be given strings, characters,
; or symbols.

(defmacro soarcarsort (x)
	`(sort ,x #'< :key #'car))

(defmacro soarcatch (&body z)
	(cons 'catch z))

(defmacro dreverse (x)
	`(nreverse ,x))

(defun soarsyntax ()
    (set-macro-character #\{
                         (function (lambda (s c)
                                       (declare (ignore s c))
                                       '\{)))
    (set-macro-character #\}
                         (function (lambda (s c)
                                       (declare (ignore s c))
                                       '\})))
    (set-macro-character #\^
                         (function (lambda (s c)
                                       (declare (ignore s c))
                                       '\^))))

(defun soarresetsyntax ())

(defun soarload (z)
    (soarsyntax)
    (load z)
    (soarresetsyntax))

(defun soarclearprops (sym)
	(setf (symbol-plist sym)  () )
)

(defun machine-dependent-init ()
	(setq *trace-file* *standard-output*)
)

(defmacro *commentary (&body z)
	(declare (ignore z))
	)

(defmacro makevector (x)
	`(the simple-vector (make-array ,x)))

(defun soarlast (list)
	 (car (last list)))

#-:ti(defmacro comment (&body z)
	(declare (ignore z))
	t)

(defmacro pop-match-elt (x)
	`(prog (temp)
	  (setq temp (classify-match-elt ,x))
	  (setq ,x (car temp))
	  (return (cdr temp))))

(defmacro pop-term (x)
	`(prog (temp)
	  (setq temp (classify-term ,x))
	  (setq ,x (car temp))
	  (return (cdr temp))))

(defun start-soar nil
	(i-g-v))

; New in version 3.2

(defmacro soarmember (a b)
    `(member ,a ,b :test #'equal))

(defmacro soarmemq (a b)
    `(member ,a ,b :test #'eql))

(defmacro soarwhile (test &body body)
  `(prog nil
      loop
	 (when (not ,test) (return))
	 ,@body
	 (go loop)))

(defun soarnthchar (a b)
	(intern (make-string 1 :initial-element
			           (char (write-to-string a :escape nil
							     :case :upcase)
					 (the fixnum
					      (- (the fixnum b) 1))))))

(defun rplnode (x c d)
    (rplaca x c)
    (rplacd x d))

(defmacro soarassq (item alist)
	`(assoc ,item ,alist :test #'eql))

(defmacro soarassoc (item alist)
	`(assoc ,item ,alist :test #'equal))

(defmacro traceterpri ()
    `(soarterpri (trace-file)))

; Defined in soar.funs.
;(defmacro traceprinc (thing)
;    `(soarprinc ,thing (trace-file)))

(defun showload (file)
  (prog (in nextexpr)
        (setq in (open file))
	(traceterpri)
     l1 (setq nextexpr (read in nil '**end-of-file**))
        (cond ((eq nextexpr '**end-of-file**) (return)))
        (traceprinc "----> ")
        (traceprinc nextexpr)
	(traceterpri)
        (traceprinc (eval nextexpr))
	(traceterpri)
        (go l1)))

(defmacro soarapply (&body z) (cons 'funcall z))

; This should return true iff something that should cause a break
; after the current decision cycle is in the input buffer.
; For now, always return nil.

(DEFUN SOARLISTEN NIL ; From Jel may 18th 87.
       (BLOCK SOARLISTEN
         (COND ((LISTEN)
                (COND ((NOT (EQL (PEEK-CHAR) #\Newline)) T)
                      (T (READ-CHAR) NIL))))))

(defmacro check-execute (outs)
	   `(and *execute-path* (setq ,outs (and *path* (list (pop *path*))))))

(defun soardifference (x y)
    (mapcan (function (lambda (z)
			      (cond ((not (soarmember z y)) (cons z nil))
				    (t nil))))
	    x))

; For Soar 4.2
(defmacro subnumber (x)
    `(parse-integer (symbol-name ,x) :start 1 :junk-allowed t))

; For Soar 4.3
(defun span-chars (x prt)
    (do ((ch (peek-char nil prt) (peek-char nil prt)))
	((not (member ch x)))
      (read-char prt nil 'end-of-file)))

;; rg 11/19/86: accept moved from main file so eof can work properly
(defmacro accept (&rest z)
	  (list 'nlam-accept (list 'quote z)))

(defun nlam-accept (z) ; Randy.Gobbel 18-Sep-86 12:34 
       (prog (port arg result)
	     (cond ((> (length z)
		       1)
		    (soarwarn "ACCEPT: wrong number of arguments" z)
		    (return nil)))
	     (setq port t)
	     (cond (*accept-file* (setq port ($ifile *accept-file*))
				  (cond ((null port)
					 (soarwarn 
					     "ACCEPT: file has been closed"
						   *accept-file*)
					 (return nil)))))
	     (cond ((eqp (length z)
			 1)
		    (setq arg ($varbind (car z)))
		    (cond ((not (symbolp arg))
			   (soarwarn "ACCEPT: illegal file name" arg)
			   (return nil)))
		    (setq port ($ifile arg))
		    (cond ((null port)
			   (soarwarn "ACCEPT: file not open for input" arg)
			   (return nil)))))
	     (stop-elapsed-time)
	     (setq result (list (flat-value (read port nil 'end-of-file))))
	     (start-elapsed-time)
	     (return result)))

(defun acceptline (z)
    (prog (def arg stream result)
	  (setq stream t)
	  (setq def z)
	  (cond (*accept-file*
		    (setq stream ($ifile *accept-file*))
		    (cond ((null stream)
			   (soarwarn '|acceptline: file has been closed|
			       *accept-file*)
			   (return nil)))))
	  (cond ((> (length def) 0)
		 (setq arg ($varbind (car def)))
		 (cond ((and (symbolp arg) ($ifile arg))
			(setq stream ($ifile arg))
			(setq def (cdr def))))))
	  (stop-elapsed-time)
	  (span-chars '(#\space #\tab) stream)
; Is the following read-char skipping the previous newline character?
	  (read-char stream)
	  (cond ((soarmemq (peek-char nil stream nil '*EOF*)
			   '(#\newline *EOF*))
		 (mapc (function $change) def)
		 (start-elapsed-time)
		 (return nil)))
	l (setq result (list (flat-value (read stream nil 'end-of-file))))
	  (span-chars '(#\space #\tab) stream)
	  (cond ((not (soarmemq (peek-char nil stream nil '*EOF*)
				'(#\newline *EOF*)))
		 (go l)))
	  (start-elapsed-time)
	  (return result)))

(defun soarconcat (a b c)
     (intern (concatenate 'string
			   (write-to-string a :escape nil :case :upcase)
			   (write-to-string b :escape nil :case :upcase)
			   (write-to-string c :escape nil :case :upcase))))

(defmacro imod (a b)
    `(rem (floor ,a) (floor ,b)))

(defmacro forward-slash nil ''/)
(defmacro backward-slash nil ''\\)
(defmacro arithmetic-operators nil ''(+ - * / \\))
(defmacro quote-character nil ''/)

; Example call: (putvector array index value)

(defmacro putvector (array index value)
     `(setf (svref ,array ,index) ,value))

; Example call: (getvector name index)

(defmacro getvector (name index)
    `(svref ,name ,index))


; Definitions and macros for Soar 4.4
    
(defmacro wme-class (wme) `(nth 0 ,wme))
(defmacro wme-id (wme) `(nth 1 ,wme))
(defmacro wme-object (wme) `(nth 1 ,wme))
(defmacro wme-attribute (wme) `(nth 2 ,wme))
(defmacro wme-role (wme) `(nth 2 ,wme))
(defmacro wme-value (wme) `(nth 3 ,wme))
(defmacro wme-reference (wme) `(nth 4 ,wme))
(defmacro wme-goal (wme) `(nth 5 ,wme))
(defmacro wme-problem-space (wme) `(nth 6 ,wme))
(defmacro wme-state (wme) `(nth 7 ,wme))
(defmacro wme-operator (wme) `(nth 8 ,wme))

(defun setf-wme-class (wme val) (rplaca wme val))
(defun setf-wme-id (wme val) (rplaca (cdr wme) val))
(defun setf-wme-object (wme val) (rplaca (cdr wme) val))
(defun setf-wme-attribute (wme val) (rplaca (nthcdr 2 wme) val))
(defun setf-wme-role (wme val) (rplaca (nthcdr 2 wme) val))
(defun setf-wme-value (wme val) (rplaca (nthcdr 3 wme) val))
(defun setf-wme-reference (wme val) (rplaca (nthcdr 4 wme) val))
(defun setf-wme-goal (wme val) (rplaca (nthcdr 5 wme) val))
(defun setf-wme-problem-space (wme val) (rplaca (nthcdr 6 wme) val))
(defun setf-wme-state (wme val) (rplaca (nthcdr 7 wme) val))
(defun setf-wme-operator (wme val) (rplaca (nthcdr 8 wme) val))

(defun soar-date ()
    (multiple-value-bind (second minute hour date month year)
	(get-decoded-time)
	(declare (ignore second minute hour))
	(format nil "~A ~D, ~D"
	    (case month
		(1 "January ")
		(2 "February ")
		(3 "March ")
		(4 "April ")
		(5 "May ")
		(6 "June ")
		(7 "July ")
		(8 "August ")
		(9 "September ")
		(10 "October ")
		(11 "November ")
		(12 "December "))
	    date
	    year)))

(defun soaropen (name iomode)
    (case iomode
	(input (open name :direction :input))
	(output (open name :direction :output))
	(otherwise (soarerror "Open: illegal I/O mode" iomode))))

(defun soarclose (stream)
    (close stream))

;;; [RMW -- 7/8/86] Added dummy functions to keep compiler
;;; from complaining about Interlisp functions to handle
;;; I/O in the *accepting-input* mode
;;; Andy Goldings changes for asynchronous.

(defun readp (&rest z) (apply #'listen z))

(defun ratom (&rest z) (apply #'read z))

(defun readc (&rest z) (princ (apply #'read-char z)))

(defmacro control (&rest z) (declare (ignore z)) t)

#|
(defmacro readc (&rest z) (declare (ignore z)) t)
(defmacro readp (&rest z) (declare (ignore z)) t)
(defmacro ratom (&rest z) (declare (ignore z)) t)

|#

;;; [RMW -- 7/8/86] Added leq and apply* to get around
;;; leftover Interlisp functions in soar.funs
(defmacro leq (&rest z) `(<= ,@z))

(defmacro apply* (fun &rest args) `(apply ,fun (quote ,args)))

;;; [RMW -- 7/8/86] Added dummy init-task function to avoid bombing
(defun init-task ()
	(format t "No init-task defined.~%")
)

(defun single-slash () #\/)

(defun soar-menu (header choice-list)
       (prog (choice count result)
       l1    (traceterpri)
	     (traceprint header)
	     (traceprint "Choose from this list by position in list (1-n)")
	     (setq count 1)
	     (soarmapc #'(lambda (x)
				 (traceprinc2 count ": ")
				 (print-choice (cond ((soarlistp x)
						      (car x))
						     (t x)))
				 (setq count (1+ count)))
		       choice-list)
	     (traceprinc "? ")
	     (setq choice (read))
	     (cond ((and (numberp choice)
			 (> choice 0)
			 (< choice (1+ (length choice-list))))
		    (setq result (soarnth (1- choice)
					  choice-list))
		    (cond ((and (soarlistp result)
				(cdr result))
			   (return (cadr result)))
			  ((soarlistp result)
			   (return (cadr result)))
			  (t (return result)))))
	     (traceprintc "Your answer was not a number between 1 and n.")
	     (go l1)))

;;; Concatenated from type module "utility" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/utility/release/utility.lisp".
;;; -*-mode: lisp; package: user -*-
;;; Utility.lisp

(defun broken2 (value) ; John.Laird 19-Sep-85 18:06 

;    tests if should stop run

       (cond ((or (soarlistp value)
		  (null value))
	      nil)
	     ((or (soarmemq (get value 'name)
			    *brknames*)
		  (soarmemq value *brknames*)
		  (and *brkrun* (eq (get value 'name)
				    *brkrun*))
		  (eq value *brkrun*)))))





(defun cdrmember (element list) ; John.Laird  3-Oct-85 09:41 
       (prog nil l0 (cond ((null list)
			   (return nil))
			  ((equal (cdr element)
				  (cdr (pop list)))
			   (return t)))
	     (go l0)))






(defun condition-trace (x) ; John.Laird 19-Sep-85 17:41 
       (trace-condition-action x 'condition))







(defun eliminate-trailing-nil (wme-list) ; Randy.Gobbel 22-Apr-86 10:38 
       (prog nil (cond ((atom wme-list)
			(return wme-list))
		       ((soarlast wme-list)
			(return wme-list)))
	     (setq wme-list (dreverse wme-list))
	     l1
	     (cond ((car wme-list)
		    (return (dreverse wme-list))))
	     (setq wme-list (cdr wme-list))
	     (go l1)))






(defun extract-ids (wme-list) ; Randy.Gobbel 22-Apr-86 10:34 
       (cond ((null wme-list)
	      nil)
	     ;; BGM added the symbolp here so that waterjug will run on non lisp machines.
	     ((and (symbolp (car wme-list)) (get (car wme-list) 'gensymed))
	      (cons (car wme-list)
		    (extract-ids (cdr wme-list))))
	     (t (extract-ids (cdr wme-list)))))





(defun find-max-goal-depth (goal-list) ; Randy.Gobbel 21-Aug-86 18:13  ; * 
 ; comment 
       (prog (max-depth max-depth-goal goal-depth)
	     (setq max-depth -1)
	     (setq max-depth-goal nil)
	     l0
	     (cond ((null goal-list)
		    (return max-depth-goal))
		   ((is-negation (car goal-list))
		    (setq goal-list (cddr goal-list))
		    (go l0)))
	     (setq goal-depth (get (wme-id (car goal-list))
				   'goal-depth))
	     (cond ((and goal-depth (< max-depth goal-depth))
		    (setq max-depth goal-depth)
		    (setq max-depth-goal (wme-id (car goal-list)))))
	     (pop goal-list)
	     (go l0)))






(defun find-variables (list) ; John.Laird 11-Sep-85 17:29 
       (cond ((null list)
	      nil)
	     ((variablep (car list))
	      (cons (car list)
		    (find-variables (cdr list))))
	     (t (find-variables (cdr list)))))

(defun flatten (l) ; John.Laird 31-Jul-84 13:43 
       (soarmapconc #'(lambda (item)
			      (cond ((atom item)
				     (list item))
				    (t (flatten item))))
		    l))

(defun gensyminit nil  ; John.Laird 22-Jul-85 14:35  ; John.Laird 
 ; 22-Jul-85 14:21 
       (soarmapc #'soarclearprops *used-gensym-list*)
       (soarmapc #'move-to-free-gensym-list *used-gensym-list*)
       (setq *used-gensym-list* nil))







(defun intrq (x y) ; John.Laird 18-Apr-85 08:11 
       (cond ((atom x)
	      nil)
	     ((soarmemq (car x)
			y)
	      (cons (car x)
		    (intrq (cdr x)
			   y)))
	     (t (intrq (cdr x)
		       y))))





(defun is-negation (item) ; Randy.Gobbel 13-Jun-86 18:40 
       (soarmemq item '(- *)))






(defun move-to-free-gensym-list (symbol) ; John.Laird 22-Jul-85 16:46 
       (prog (char sym-list)
	     (setq char (soarnthchar symbol 1))
	     (setq sym-list (soarassq char *free-gensym-list*))
	     (cond ((null sym-list)
		    (setq *free-gensym-list* (append *free-gensym-list*
						     (list (list char symbol))))
		    )
		   (t (rplacd sym-list (append (list symbol)
					       (cdr sym-list)))))))






(defun remove-condition-trash (condition) ; Randy.Gobbel 17-Oct-86 12:36 
       (prog (return-condition)
	     (setq return-condition nil)
	     (soarwhile condition
			(cond ((and (soarlistp (car condition))
				    (eq (caar condition)
					'<<))
			       (pop condition)
			       (soarpush '*unbound* return-condition))
			      ((soarlistp (car condition))
			       (soarpush (car (remove-condition-trash
						(cdr (pop condition))))
					 return-condition))
			      ((eq (car condition)
				   '})
			       (pop condition))
			      ((predicatep (car condition))
			       (setq condition (cddr condition)))
			      (t (soarpush (pop condition)
					   return-condition))))
	     (and (null return-condition)
		  (return '(*unbound*)))
	     (return (nreverse return-condition))))







(defun remove-nil (x) ; Randy.Gobbel 13-Jun-86 14:34 
       (cond ((null x)
	      x)
	     ((and (soarlistp x)
		   (null (car x)))
	      (remove-nil (cdr x)))
	     ((and (soarlistp x)
		   (eq (car x)
		       '!))
	      (remove-nil (cdr x)))
	     ((and (soarlistp x)
		   (null (cdr x)))
	      (remove-nil (car x)))
	     ((listp x)
	      (append (remove-nil (car x))
		      (remove-nil (cdr x))))
	     (t (list x))))






(defun soarerror (what where) ; Randy.Gobbel  3-Apr-86 14:01 
       (soarwarn what where)
       (*throw '!error! '!error!))






(defun soargensym (x) ; Randy.Gobbel  3-Apr-86 13:41 
       (prog (sym-list charx sym)
	     (setq charx (soarnthchar x 1))
	     (setq sym-list (soarassq charx *free-gensym-list*))
	     (cond ((null (cdr sym-list))
		    (setq sym (soargensymbol charx)))
		   (t (setq sym (cadr sym-list))
		      (rplacd sym-list (cddr sym-list)))) ; FORCE GENSYMED 
 ; AND WMPART* TO BE TOP PROPERTIES OF ID. 
	     (soarputprop sym (hash-id sym)
			  'gensymed)
	     (soarputprop sym nil 'wmpart*)
	     (soarputprop sym *cycle-count* 'creation-time)
	     (soarpush sym *used-gensym-list*)
	     (return sym)))






(defun soargenvar (id) ; hts: 31-Jan-86 14:39 
       (prog (first-char gen-number new-var)
	     (setq first-char (soarnthchar id 1))
	     (cond ((soarmemq first-char *used-char-list*)
		    (setq gen-number (get first-char 'var-count)))
		   (t (soarpush first-char *used-char-list*)
		      (setq gen-number 1)))
	     (setq new-var (soarpack '< first-char (soar-i-to-s gen-number)
				     '>))
	     (soarpush new-var *used-var-list*)
	     (soarclearprops new-var)
	     (soarputprop first-char (1+ gen-number)
			  'var-count)
	     (return new-var)))






(defun start-default nil  ; Randy.Gobbel  9-May-86 10:13 
       (setq *loading-default* t))





(defun start-elapsed-time nil  ; Randy.Gobbel 13-Jun-86 14:34 
       (setq *begin-time* (alwaystime)))





(defun stop-default nil  ; Randy.Gobbel  9-May-86 10:14 
       (setq *loading-default* nil))





(defun stop-elapsed-time nil  ; Randy.Gobbel 13-Jun-86 14:35 
       (setq *elapsed-time* (iplus *elapsed-time* (time-difference (alwaystime)
								   *begin-time*)
				   )))






(defun t-in-list? (x) ; John.Laird  7-Oct-85 11:01 
       (soarmemq t (flatten x)))





(defun conflict (a b) ; edited: 23-Mar-86 09:23 
       (prog (old)
	     (setq old (get a 'conflicts))
	     (and (not (eq a b))
		  (not (soarmemq b old))
		  (soarputprop a (cons b old)
			       'conflicts))))






(defun creation-time (wme) ; edited: 23-Mar-86 09:16 
       (cdr (soarassq wme (get (wm-hash wme)
			       'wmpart*))))






(defun current-field nil (field-name *subnum*))






(defun expand-preference (pref) ; Randy.Gobbel 16-Jun-86 17:32 
       (prog (attribs)
	     (setq attribs '(role value reference goal problem-space state 
				  operator))
	     (return (append (list (car pref)
				   (cadr pref))
			     (soarmapconc #'(lambda (x)
						    (list (pop attribs)
							  x))
					  (soar-copy (cddr pref)))))))






(defun flat-value (x) ; Randy.Gobbel 11-Jun-86 16:35 
       (cond ((atom x)
	      x)
	     (t (soarmapc #'flat-value x))))






(defun get-duplicates (vlist) ; Randy.Gobbel 13-May-86 14:18  ; RETURNS A 
 ; LIST OF ALL ELEMENTS THAT APPEAR TWICE IN VLIST 
       (soar-do ((vl vlist (cdr vl))
		 (dup))
		((null vl)
		 (return dup))
		(cond ((and (soarmemq (car vl)
				      (cdr vl))
			    (not (soarmemq (car vl)
					   dup)))
		       (soarpush (car vl)
				 dup)))))





(defun get-hash-id (id) ; Randy.Gobbel 23-Jun-86 19:05 
       (or (get id 'gensymed)
	   0))





(defun get-non-duplicates (list) ; Randy.Gobbel 13-May-86 14:18 
       (soar-do ((l1 list (cdr l1))
		 (nondup)
		 (dup))
		((null l1)
		 (return nondup))
		(cond ((soarmemq (car l1)
				 dup))
		      ((soarmemq (car l1)
				 (cdr l1))
		       (soarpush (car l1)
				 dup))
		      (t (soarpush (car l1)
				   nondup)))))






(defun getupto (end) ; Randy.Gobbel 11-Sep-86 15:25 
       (prog (v)
	     (and (atom *ppline*)
		  (return nil))
	     (setq v (car *ppline*))
	     (setq *ppline* (cdr *ppline*))
	     (cond ((eqp v end)
		    (return (list v)))
		   (t (return (cons v (getupto end)))))))





(defun getval nil  ; edited: 23-Mar-86 09:24 
       (prog (res v1)
	     (setq v1 (car *ppline*))
	     (setq *ppline* (cdr *ppline*))
	     (cond ((soarmemq v1 '(<> = < <+ => > <=>))
		    (setq res (cons v1 (getval))))
		   ((eq v1 '{)
		    (setq res (cons v1 (getupto '}))))
		   ((eq v1 '<<)
		    (setq res (cons v1 (getupto '>>))))
		   ((eq v1 '//)
		    (setq res (list v1 (car *ppline*)))
		    (setq *ppline* (cdr *ppline*)))
		   (t (setq res (list v1))))
	     (return res)))





(defun nulla (x) ; pkh: 13-Feb-84 14:09 
       (cond ((null x)
	      nil)
	     ((atom x)
	      nil)
	     ((and (null (car x))
		   (nulla (cdr x)))
	      nil)
	     (t t)))






(defun order-part (conflict-elem)
       (cdr conflict-elem))





(defun variablep (x) ; John.Laird 10-May-85 16:29  ; TRUE IF X IS A 
 ; VARIABLE NAME 
       (and (symbolp x)
	    (eq (soarnthchar x 1)
		'<)
	    (not (predicatep x))
	    (not (eq x '<<))))






(defun wm-hash (x) ; Randy.Gobbel 21-Apr-86 17:04 
       (wme-id x))







;;; Concatenated from type module "rete" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/rete/release/network.lisp".
;;; -*-Mode: lisp; package: user -*-
;;; Network.lisp

(defun
  compute-negation-index
  (matrix) ; John.Laird 17-Jun-86 09:42  ; CREATES A LIST OF TRIPLES - 
 ; EACH TRIPLES CONSISTS OF A VARIABLE NAME - CONDITION INDEX - ELEMENT 
 ; INDEX 
  (prog
    (save-matrix negation-vars negation-list count1 neg-var save-negation-vars 
		 negated-conditions forall-conditions condition temp)
    (setq negated-conditions nil)
    (setq negation-vars nil)
    (setq negation-list nil)
    (setq forall-conditions nil)
    (setq save-matrix matrix)
    (soarwhile
      matrix
      (cond
	((and (is-negation (car matrix))
	      (not (soarlistp (caadr matrix))))
	 (setq temp (remove-condition-trash (cadr matrix)))
	 (cond ((eq (car matrix)
		    '-)
		(soarpush temp negated-conditions))
	       ((eq (car matrix)
		    '*)
		(soarpush temp forall-conditions)))
	 (pop matrix)
	 (setq negation-vars (union (find-all-vars-in-condition temp)
				    negation-vars)))
	((is-negation (car matrix))
	 (setq temp
	       (soarmapcar
		 #'(lambda (x)
			   (setq temp (remove-condition-trash x))
			   (setq negation-vars
				 (union (find-all-vars-in-condition temp)
					negation-vars))
			   temp)
		 (cadr matrix)))
	 (cond ((eq (car matrix)
		    '-)
		(soarpush temp negated-conditions))
	       ((eq (car matrix)
		    '*)
		(soarpush temp forall-conditions)))
	 (pop matrix)))
      (pop matrix))
    (and (null negated-conditions)
	 (null forall-conditions)
	 (return nil))
    (setq save-negation-vars negation-vars)
    (soarwhile
      save-negation-vars
      (setq neg-var (pop save-negation-vars))
      (setq count1 0)
      (setq temp nil)
      (setq matrix save-matrix)
      (soarwhile
	(and (neq (car matrix)
		  '-->)
	     (null temp))
	(setq condition (pop matrix))
	(cond ((is-negation condition)
	       (pop matrix))
	      (t (cond ((setq temp (soarmemq
				neg-var
				(setq condition (remove-condition-trash 
								  condition))))
			(soarpush (list neg-var count1
					(difference (length condition)
						    (length temp)))
				  negation-list)))
		 (setq count1 (1+ count1))))))
    (return (list negated-conditions forall-conditions negation-list))))

(defun create-new-class (class) ; Randy.Gobbel 13-Jun-86 14:31 
       (cond ((not (soarmemq class *save-class-list*))
	      (soarpush class *save-class-list*))))

(defun mem-length (mem) ; John.Laird 16-Apr-85 10:49 
       (prog (n)
	     (setq n 0)
	     (soarmapc #'(lambda (ci)
				 (cond ((soarlistp ci)
					(setq n (+ n 1)))))
		       mem)
	     (return n)))

(defun $change (x) ; Randy.Gobbel 17-Jun-86 15:31 
       (cond ((soarlistp x)
	      (eval-function x))
	     (t (list ($varbind x)))))






(defun $varbind (x) ; Randy.Gobbel 17-Jun-86 15:30 
       (prog (r)
	     (and (not *in-rhs*)
		  (return x))
	     (setq r (soarassq x *variable-memory*))
	     (cond (r (return (cdr r)))
		   ((variablep x)
		    (setq r (soargensym (soarnthchar x 2)))
		    (make-var-bind x r)
		    (return r))
		   (t (return x)))))

(defun &all (outs lmem rpred tests) ; Randy.Gobbel 13-May-86 13:54 
       (not-left outs rpred tests lmem nil))

(defun &any (outs register const-list) ; Randy.Gobbel 11-Sep-86 15:26 
       (prog (z c)
	     (setq z (eval register))
	     (cond ((numberp z)
		    (go number)))
	     symbol
	     (cond ((null const-list)
		    (return nil))
		   ((eq (car const-list)
			z)
		    (go ok))
		   (t (setq const-list (cdr const-list))
		      (go symbol)))
	     number
	     (cond ((null const-list)
		    (return nil))
		   ((and (numberp (setq c (car const-list)))
			 (eqp c z))
		    (go ok))
		   (t (setq const-list (cdr const-list))
		      (go number)))
	     ok
	     (eval-nodelist outs)))

(defun &bus (outs) ; Randy.Gobbel 22-Aug-86 11:45  ; Set the registers to 
 ; the values of the fields of the active wme. execute each output, 
 ; indexing off class name and attribute name in the wme if possible to 
 ; speed things up. 
 ; Hey what's got 16 slots ? Not a goal context info ? Not a preference ?
       (prog (dp class-list pair)
	     (setq dp (car *data-part*))
	     (setq *c1* (car dp))
	     (setq dp (cdr dp))
	     (setq *c2* (car dp))
	     (setq dp (cdr dp))
	     (setq *c3* (car dp))
	     (setq dp (cdr dp))
	     (setq *c4* (car dp))
	     (setq dp (cdr dp))
	     (setq *c5* (car dp))
	     (setq dp (cdr dp))
	     (setq *c6* (car dp))
	     (setq dp (cdr dp))
	     (setq *c7* (car dp))
	     (setq dp (cdr dp))
	     (setq *c8* (car dp))
	     (setq dp (cdr dp))
	     (setq *c9* (car dp))
	     (setq dp (cdr dp))
	     (setq *c10* (car dp))
	     (setq dp (cdr dp))
	     (setq *c11* (car dp))
	     (setq dp (cdr dp))
	     (setq *c12* (car dp))
	     (setq dp (cdr dp))
	     (setq *c13* (car dp))
	     (setq dp (cdr dp))
	     (setq *c14* (car dp))
	     (setq dp (cdr dp))
	     (setq *c15* (car dp))
	     (setq dp (cdr dp))
	     (setq *c16* (car dp))
	     (setq class-list (get *c1* '&bus-branch))
	     (cond ((and (not *execute-path*)
			 class-list)
		    (eval-nodelist (left-outs (car class-list)))
		    (cond ((setq pair (soarassq *c3* (cdr class-list)))
			   (eval-nodelist (left-outs (cdr pair)))))))
	     (eval-nodelist outs)))

(defun &not (outs lmem rpred tests) ; John.Laird 23-Jun-86 11:04 
       (not-left outs rpred tests lmem t))

(defun &old (a b c d e)
       nil)

(defun &p (rating name var-dope ce-var-dope rhs) ; Randy.Gobbel 
 ; 16-Jul-86 16:44  ; ADD OR REMOVE A TOKEN FROM THE CONFLICT SET. 
       (prog nil (cond ((eq *flag-part* 'new)
			(insertcs name *data-part*))
		       (t (removecs name *data-part*)))))

(defun add-test (list new old) ; John.Laird 11-Jun-86 15:25 
       (prog (ttype lloc rloc)
	     (setq *feature-count* (1+ *feature-count*))
	     (setq ttype (get (cadr new)
			      'b))
	     (setq rloc (encode-pair (caddr new)
				     (cadddr new)))
	     (setq lloc (encode-pair (caddr old)
				     (cadddr old)))
	     (return (cons ttype (cons lloc (cons rloc list))))))

(defun add-token (memlis data-part hashflag num) ; Dan.Scales 
 ; 13-Jan-86 17:54 
       (prog nil (cond ((eq *flag-part* 'new)
			(real-add-token memlis data-part hashflag num))
		       (t (remove-old memlis data-part hashflag num)))))

(defun and-left (outs mem tests) ; Randy.Gobbel 13-May-86 14:04 
       (prog (memdp tlist tst lind rind res path i memlist memlist1 flag id dp)
	     (check-execute outs)
	     (setq path *path*)
	     (setq dp *data-part*)
	     (cond ((and tests (eq (car tests)
				   'teqb)
			 (equal (caddr tests)
				'(0 . 1)))
		    (setq flag t)
		    (setq id (gelm *data-part* (cadr tests)))
		    (setq memlist (soarassq id (getvector mem
							  (get-hash-id id))))
		    (or memlist (return nil))
		    (setq memlist1 (cdr memlist))
		    (setq memlist nil)
		    (setq tests (cdddr tests))
		    (go loop1))
		   (t (setq i 0)))
	     loopi
	     (and (>= i *mem-array-size*)
		  (return nil))
	     (setq memlist (getvector mem i))
	     fail
	     (cond ((null memlist)
		    (and flag (return nil))
		    (setq i (1+ i))
		    (go loopi)))
	     (setq memlist1 (cdr (pop memlist)))
	     loop1
	     (and (null memlist1)
		  (go fail))
	     (setq memdp (pop memlist1))
	     (setq tlist tests)
	     tloop
	     (and (null tlist)
		  (go succ))
	     (setq tst (pop tlist))
	     (setq lind (pop tlist))
	     (setq rind (pop tlist))
	     (setq res (cond ((eq tst 'teqb)
			      (eqp (gelm memdp rind)
				   (gelm *data-part* lind)))
			     (t (soarapply tst (gelm memdp rind)
					   (gelm *data-part* lind)))))
	     (cond (res (go tloop))
		   (t (go loop1)))
	     succ
	     (setq tlist outs)
	     (setq *data-part* (append memdp *data-part*))
	     loop
	     (cond ((null tlist)
		    (setq *data-part* dp)
		    (go loop1)))
	     (setq *path* path)
	     (setq tst (pop tlist))
	     (setq res (pop tst))
	     (cond ((eq res '&lmem)
		    (&lmem (car tst)
			   (cadr tst)))
		   ((or (eq res '&not)
			(eq res '&all))
		    (not-left (car tst)
			      (caddr tst)
			      (cadddr tst)
			      (cadr tst)
			      (eq res '&not)))
		   (t (apply res tst)))
	     (go loop)))





(defun and-right (outs mem tests) ; Randy.Gobbel 13-May-86 14:04 
       (prog (dp memdp tlist tst lind rind res)
	     (setq dp *data-part*)
	     fail
	     (and (null mem)
		  (return nil))
	     (setq memdp (pop mem))
	     (setq tlist tests)
	     tloop
	     (and (null tlist)
		  (go succ))
	     (setq tst (pop tlist))
	     (setq lind (pop tlist))
	     (setq rind (pop tlist))
	     (setq res (cond ((eq tst 'teqb)
			      (eqp (gelm dp rind)
				   (gelm memdp lind)))
			     (t (soarapply tst (gelm dp rind)
					   (gelm memdp lind)))))
	     (cond (res (go tloop))
		   (t (go fail)))
	     succ
	     (setq tlist outs)
	     (setq *data-part* (append dp memdp))
	     loop
	     (cond ((null tlist)
		    (setq *data-part* dp)
		    (go fail)))
	     (setq tst (pop tlist))
	     (setq res (pop tst))
	     (cond ((eq res '&lmem)
		    (&lmem (car tst)
			   (cadr tst)))
		   ((or (eq res '&not)
			(eq res '&all))
		    (not-left (car tst)
			      (caddr tst)
			      (cadddr tst)
			      (cadr tst)
			      (eq res '&not)))
		   (t (apply res tst)))
	     (go loop)))

(defun any-bound? (vlist) ; Randy.Gobbel 13-May-86 14:04 
       (soar-do ((vl1 vlist (cdr vl1)))
		((null vl1)
		 nil)
		(cond ((bound-var? (car vl1))
		       (return t)))))

(defun ari (x) ; Randy.Gobbel  4-Apr-86 16:12 
       (cond ((atom x)
	      (soarwarn "Bad syntax in arithmetic expression" x)
	      0)
	     ((atom (cdr x))
	      (ari-unit (car x)))
	     ((eq (cadr x)
		  '+)
	      (plus (ari-unit (car x))
		    (ari (cddr x))))
	     ((eq (cadr x)
		  '-)
	      (difference (ari-unit (car x))
			  (ari (cddr x))))
	     ((eq (cadr x)
		  '*)
	      (times (ari-unit (car x))
		     (ari (cddr x))))
	     ((eq (cadr x)
		  (forward-slash))
	      (quotient (ari-unit (car x))
			(ari (cddr x))))
	     ((eq (cadr x)
		  (backward-slash))
	      (imod (ari-unit (car x))
		    (ari (cddr x))))
	     (t (soarwarn "Bad syntax in arithmetic expression" x)
		0)))

(defun ari-unit (a) ; Randy.Gobbel  7-Apr-86 14:35 
       (prog (r)
	     (cond ((soarlistp a)
		    (setq r (ari a)))
		   (t (setq r ($varbind a))))
	     (cond ((not (numberp r))
		    (soarwarn "Bad value in arithmetic expression" a)
		    (return 0))
		   (t (return r)))))

(defun attach-left (old new)
       (rplaca (cdr old)
	       (cons new (cadr old))))

(defun conflict-set-compare (x y) ; JonL 23-Apr-84 19:06 
       (prog (xorder yorder xl yl xv yv)
	     (setq xorder (order-part x))
	     (setq yorder (order-part y))
	     (setq xl (car xorder))
	     (setq yl (car yorder))
	     data
	     (cond ((and (null xl)
			 (null yl))
		    (go ps))
		   ((nulla yl)
		    (return t))
		   ((nulla xl)
		    (return nil)))
	     (setq xv (car xl))
	     (setq yv (car yl))
	     (cond ((> xv yv)
		    (return t))
		   ((> yv xv)
		    (return nil)))
	     (setq xl (cdr xl))
	     (setq yl (cdr yl))
	     (go data)
	     ps
	     (setq xl (cdr xorder))
	     (setq yl (cdr yorder))
	     psl
	     (cond ((null xl)
		    (return t)))
	     (setq xv (car xl))
	     (setq yv (car yl))
	     (cond ((> xv yv)
		    (return t))
		   ((> yv xv)
		    (return nil)))
	     (setq xl (cdr xl))
	     (setq yl (cdr yl))
	     (go psl)))

(defun best-of* (best rem)
       (cond ((not rem)
	      best)
	     ((conflict-set-compare best (car rem))
	      (best-of* best (cdr rem)))
	     (t (best-of* (car rem)
			  (cdr rem)))))

(defun beta-equiv (a b) ; edited: 16-Feb-84 11:52 
       (and (eq (car a)
		(car b))
	    (equal (cddddr a)
		   (cddddr b))
	    (or (eq (car a)
		    '&and)
		(equal (caddr a)
		       (caddr b)))))

(defun bound? (var) ; edited: 23-Mar-86 09:21 
       (or (soarmemq var *rhs-bound-vars*)
	   (var-dope var)))

(defun branch-bakptrs (x) ; Randy.Gobbel 23-Jun-86 19:04 
       (cadddr x))

(defun branch-node (x) ; Randy.Gobbel 23-Jun-86 19:04 
       (caadr x))

(defun branch-path (x) ; Randy.Gobbel 23-Jun-86 19:04 
       (cadr x))

(defun branch-tsize (x) ; Randy.Gobbel 23-Jun-86 19:04 
       (caddr x))

(defun branch-vars (x) ; Randy.Gobbel 23-Jun-86 19:04 
       (car x))

(defun build-beta (type tests old-branch new-branch) ; Randy.Gobbel 
 ; 13-May-86 14:04  ; JOIN THE ALPHA BRANCH JUST BUILT AND THE BETA BRANCH 
 ; REPRESENTING THE PRODUCTION SO FAR BY A BETA NODE (&AND OR &NOT OR &ALL) 
 ; PUT A MEMORY NODE ON THE ALPHA BRANCH AND, IF NECESSARY, THE BETA 
 ; BRANCH, BEFORE JOINING. 
       (prog (rpred lpred)
	     (cond ((null type)
		    (set-branch-path old-branch (branch-path new-branch))
		    (return)))
	     (link-new-node (list '&rmem nil (protohmem))
			    new-branch)
	     (setq rpred (caddr (branch-node new-branch)))
	     (set-branch-bakptrs new-branch (cons (branch-node new-branch)
						  (branch-bakptrs new-branch)))
	     (cond ((eq type '&and)
		    (link-new-node (list '&lmem nil (protomem))
				   old-branch)
		    (setq lpred (caddr (branch-node old-branch)))
		    (set-branch-bakptrs old-branch
					(cons (branch-node old-branch)
					      (branch-bakptrs old-branch))))
		   (t (setq lpred (protomem))))
	     (link-new-beta-node (list type nil lpred rpred tests)
				 old-branch new-branch)
	     (cond ((neq type '&and)
		    (set-branch-bakptrs old-branch
					(cons (branch-node old-branch)
					      (branch-bakptrs old-branch)))))))






(defun check-0-args (x) ; Randy.Gobbel 11-Sep-86 15:19 
       (or (eqp (length x)
		1)
	   (soarwarn "Should not have arguments" x)))





(defun check-accept (x) ; Randy.Gobbel 11-Sep-86 15:20 
       (cond ((eqp (length x)
		   1)
	      nil)
	     ((eqp (length x)
		   2)
	      (check-rhs-atomic (cadr x)))
	     (t (soarwarn "Too many arguments" x))))

(defun check-acceptline (x) ; RG: 20-Feb-86 18:13 
       (soarmapc #'check-rhs-atomic (cdr x)))

(defun check-action (x) ; Randy.Gobbel  2-Jul-86 16:23 
       (prog (a)
	     (cond ((atom x)
		    (soarwarn "Atomic Action" x)
		    (return nil)))
	     (setq a (car x))
	     (cond ((eq a 'soar-bind)
		    (check-bind x))
		   ((eq a 'label-bind)
		    (check-bind x))
		   ((eq a 'call2)
		    (return nil))
		   ((eq a 'tabstop)
		    (check-bind x))
		   ((eq a 'make)
		    (check-make x))
		   ((eq a 'write)
		    (soarwarn "Illegal action:" a))
		   ((eq a 'write1)
		    (check-write x))
		   ((eq a 'write2)
		    (check-write x))
		   ((eq a 'call1)
		    (check-call x))
		   ((eq a 'call)
		    (soarwarn "Illegal action:" a))
		   ((eq a 'halt)
		    (check-halt x))
		   ((eq a 'openfile)
		    (soarwarn "Illegal action:" a))
		   ((eq a 'openfile1)
		    (check-openfile x))
		   ((eq a 'closefile)
		    (soarwarn "Illegal action:" a))
		   ((eq a 'closefile1)
		    (check-closefile x))
		   ((eq a 'default)
		    (check-default x))
		   (t (soarwarn "Illegal Action" x)))))

(defun check-arithmetic (l) ; Randy.Gobbel  7-Apr-86 15:12 
       (cond ((atom l)
	      (soarwarn "Syntax error in arithmetic expression" l))
	     ((atom (cdr l))
	      (check-term (car l)))
	     ((not (soarmemq (cadr l)
			     (arithmetic-operators)))
	      (soarwarn "Unknown operator" l))
	     (t (check-term (car l))
		(check-arithmetic (cddr l)))))

(defun check-bind (z) ; Randy.Gobbel  3-Apr-86 13:45 
       (prog (v)
	     (or (> (length z)
		    1)
		 (soarwarn "Needs arguments" z))
	     (setq v (cadr z))
	     (or (variablep v)
		 (soarwarn "Takes variable as argument" z))
	     (note-variable v)
	     (check-change& (cddr z))))

(defun check-build-collect (args) ; Randy.Gobbel  7-Apr-86 15:15 
       (prog (r)
	     top
	     (and (null args)
		  (return nil))
	     (setq r (car args))
	     (setq args (cdr args))
	     (cond ((soarlistp r)
		    (check-build-collect r))
		   ((eq r (quote-character))
		    (and (null args)
			 (soarwarn "Nothing to evaluate" r))
		    (check-rhs-value (car args))
		    (setq args (cdr args))))
	     (go top)))





(defun check-call (z) ; Randy.Gobbel  3-Apr-86 13:47 
       (prog (f)
	     (and (null (cdr z))
		  (soarwarn "Needs arguments" z))
	     (setq f (cadr z))
	     (and (variablep f)
		  (soarwarn "Function name must be a constant" z))
	     (or (symbolp f)
		 (soarwarn "Function name must be a symbolic atom" f))
	     (or (externalp f)
		 (soarwarn "Function named not declared external" f))
	     (check-change& (cddr z))))





(defun check-change& (z) ; Randy.Gobbel 13-Jun-86 14:40 
       (soarwhile (not (atom z))
		  (check-rhs-value (pop z))))





(defun check-closefile (z) ; Randy.Gobbel  3-Apr-86 13:47 
       (and (null (cdr z))
	    (soarwarn "Needs arguments" z))
       (check-change& (cdr z)))





(defun check-compute (x)
       (check-arithmetic (cdr x)))





(defun check-crlf (x)
       (check-0-args x))





(defun check-default (z) ; Randy.Gobbel  3-Apr-86 13:48 
       (and (null (cdr z))
	    (soarwarn "Needs arguments" z))
       (check-change& (cdr z)))





(defun check-halt (z) ; Randy.Gobbel  3-Apr-86 14:04 
       (or (null (cdr z))
	   (soarwarn "Does not take arguments" z)))





(defun check-limits nil  ; Randy.Gobbel 12-Sep-86 12:17 
       (cond ((> (length *conflict-set*)
		 *limit-cs*)
	      (soarterpri)
	      (soarterpri)
	      (printlinec (list "CONFLICT SET SIZE EXCEEDED THE LIMIT OF" 
				*limit-cs* 'after *p-name*))
	      (setq *halt-flag* t)))
       (cond ((> *current-token* *limit-token*)
	      (soarterpri)
	      (soarterpri)
	      (printlinec (list "TOKEN MEMORY SIZE EXCEEDED THE LIMIT OF" 
				*limit-token* 'after *p-name*))
	      (setq *halt-flag* t))))





(defun check-make (z) ; Randy.Gobbel  8-May-86 16:41 
       (or (soarmemq *p-type* '(elaborate-once elaborate serial))
	   (soarwarn "Illegal make in production type" *p-type*))
       (and (null (cdr z))
	    (soarwarn "Missing arguments in Make" " "))
       (check-change& (cdr z)))





(defun check-openfile (z) ; Randy.Gobbel  3-Apr-86 13:48 
       (and (null (cdr z))
	    (soarwarn "Needs arguments" z))
       (check-change& (cdr z)))





(defun check-print-control (x) ; Randy.Gobbel  3-Apr-86 13:48 
       (prog nil (cond ((bound? x)
			(return x)))
	     (cond ((or (not (numberp x))
			(< x 1)
			(> x 127))
		    (soarwarn "Illegal value for printer control" x)))))





(defun check-rhs (rhs) ; RG: 20-Feb-86 18:13 
       (soarmapc #'check-action rhs))





(defun check-rhs-atomic (x) ; Randy.Gobbel  3-Apr-86 14:05 
       (and (variablep x)
	    (not (bound? x))
	    (soarwarn "Unbound variable" x)))





(defun check-rhs-function (x) ; Randy.Gobbel 18-Jun-86 13:56 
       (prog (a)
	     (setq a (car x))
	     (cond ((eq a 'compute)
		    (check-compute x))
		   ((eq a 'arith)
		    (check-compute x))
		   ((eq a 'accept)
		    (check-accept x))
		   ((eq a 'acceptline)
		    (check-acceptline x))
		   ((eq a 'crlf)
		    (check-crlf x))
		   ((eq a 'tabto)
		    (check-tabto x))
		   ((eq a 'rjust)
		    (check-rjust x))
		   ((not (externalp a))
		    (soarwarn "RHS function not declared external" a)))))





(defun check-rhs-value (x) ; John.Laird  4-Apr-85 10:04  ; ARGLIST = (X) 
       (cond ((soarlistp x)
	      (check-rhs-function x))))





(defun check-rjust (x) ; Randy.Gobbel 11-Sep-86 15:20 
       (or (eqp (length x)
		2)
	   (soarwarn "Wrong number of arguments" x))
       (check-print-control (cadr x)))





(defun check-tabto (x) ; Randy.Gobbel 11-Sep-86 15:20 
       (or (eqp (length x)
		2)
	   (soarwarn "Wrong number of arguments" x))
       (check-print-control (cadr x)))





(defun check-term (x) ; Randy.Gobbel  7-Apr-86 14:36 
       (cond ((soarlistp x)
	      (check-arithmetic x))
	     (t (check-rhs-atomic x))))





(defun check-write (z) ; Randy.Gobbel  3-Apr-86 13:51 
       (and (null (cdr z))
	    (soarwarn "Needs arguments" z))
       (check-change& (cdr z)))






(defun cmp-any nil  ; Randy.Gobbel 13-May-86 14:04 
       (prog (a z)
	     (sublex)
	     la
	     (cond ((end-of-ce)
		    (soarerror "MISSING >>" a)))
	     (setq a (sublex))
	     (cond ((neq '>> a)
		    (setq z (cons a z))
		    (go la)))
	     (link-new-node (list '&any nil (current-field)
				  z)
			    *current-branch*)))

(defun cmp-atomic nil  ; edited: 21-Feb-84 20:02 
       (prog (test x)
	     (setq x (peek-sublex))
	     (cond ((eq x '=)
		    (setq test 'eq)
		    (sublex))
		   ((eq x '<>)
		    (setq test 'ne)
		    (sublex))
		   ((eq x '<)
		    (setq test 'lt)
		    (sublex))
		   ((eq x '<=)
		    (setq test 'le)
		    (sublex))
		   ((eq x '>)
		    (setq test 'gt)
		    (sublex))
		   ((eq x '>=)
		    (setq test 'ge)
		    (sublex))
		   ((eq x '<=>)
		    (setq test 'xx)
		    (sublex))
		   (t (setq test 'eq)))
	     (cmp-symbol test)))

(defun cmp-atomic-or-any nil (cond ((eq (peek-sublex)
					'<<)
				    (cmp-any))
				   (t (cmp-atomic))))





(defun cmp-beta (kind old-branch new-branch last-sectflag sectflag) ; 
 ; Randy.Gobbel 22-Aug-86 12:42 
       (prog (tlist newv vname vpred vpos oldv new-vars old-vars joinflag)
	     (setq new-vars (branch-vars new-branch))
	     (setq old-vars (branch-vars old-branch))
	     (setq joinflag (or (null kind)
				(eq kind '&and)))
	     la
	     (and (null new-vars)
		  (go lb))
	     (setq newv (pop new-vars))
	     (setq vname (car newv))
	     (setq vpred (cadr newv))
	     (setq vpos (cadddr newv))
	     (setq oldv (soarassq vname (branch-vars old-branch)))
	     (cond (oldv (setq tlist (add-test tlist newv oldv))
			 (cond ((and joinflag (eq vpred 'eq)
				     (< vpos (+ (caddr oldv)
						(cadddr oldv))))
				(set-branch-vars old-branch
						 (dremove oldv
							  (branch-vars 
								 old-branch)))
				(promote-var old-branch newv))))
		   (joinflag (promote-var old-branch newv)))
	     (go la)
	     lb
	     (build-beta kind tlist old-branch new-branch)
	     (cond (joinflag  ; DON'T FUDGE THE VARS THAT WERE JUST 
 ; PROMOTED 
			     (fudge old-vars (branch-tsize new-branch))
			     (set-branch-tsize old-branch
					       (+ (branch-tsize new-branch)
						  (branch-tsize old-branch)))))
	     (cond (last-sectflag (set-branch-bakptrs
				    old-branch
				    (list (nreverse (branch-bakptrs old-branch))
					  ))))
	     (cond ((not sectflag)
		    (set-branch-bakptrs old-branch
					(append (branch-bakptrs new-branch)
						(branch-bakptrs old-branch))))
		   ((not kind)
		    (set-branch-bakptrs old-branch (branch-bakptrs new-branch)))
		   (t (set-branch-bakptrs old-branch
					  (cons (nreverse
						  (branch-bakptrs new-branch))
						(branch-bakptrs old-branch))))))
       )

(defun prepare-sublex (ce) (setq *curcond* ce))

(defun cmp-ce (c) ; Randy.Gobbel 23-Jun-86 19:41 
       (prog (pref-found)
	     (new-subnum 0)
	     (and (atom c)
		  (soarerror "Atomic conditions are not allowed" c))
	     (setq pref-found (eq (wme-class c)
				  'preference))
	     (prepare-sublex c)
	     (return (soarwhile (not (end-of-ce))
				(incr-subnum)
				(setq *context-field-found*
				      (and pref-found (>= *subnum* 5)))
				(cmp-element)))))

(defun cmp-conds (cl) ; Randy.Gobbel 22-Aug-86 13:29 
       (prog (type last-branch last-sectflag)
	     (setq last-branch (make-branch))
	     (set-branch-vars last-branch nil)
	     (set-branch-tsize last-branch 0)
	     (set-branch-path last-branch nil)
	     (set-branch-bakptrs last-branch nil)
	     nextcond
	     (cond ((or (null cl)
			(eq (car cl)
			    '-->))
		    (return last-branch)))
	     (cond ((eq (car cl)
			'-)
		    (setq type '&not)
		    (pop cl))
		   ((eq (car cl)
			'*)
		    (setq type '&all)
		    (pop cl))
		   ((null (branch-path last-branch))
		    (setq type nil))
		   (t (setq type '&and)))
	     (cond ((soarlistp (caar cl))
		    (setq *current-branch* (cmp-conds (car cl))))
		   (t (set-branch-tsize *current-branch* 1)
		      (set-branch-path *current-branch* (list *first-node*))
		      (set-branch-vars *current-branch* nil)
		      (set-branch-bakptrs *current-branch* nil)
		      (cmp-ce (flatten (car cl)))))
	     (cmp-beta type last-branch *current-branch* last-sectflag
		       (soarlistp (caar cl)))
	     (setq last-sectflag (and (null type)
				      (soarlistp (caar cl))))
	     (pop cl)
	     (go nextcond)))


(defun cmp-constant (test) ; Randy.Gobbel 13-Jun-86 11:29 
       (prog (sym)
	     (or (soarmemq test '(eq ne xx))
		 (soarerror "Non-numeric constant after numeric predicate"
			    (sublex)))
	     (cond ((neq (setq sym (sublex))
			 '*unbound*)
		    (link-new-node (list (get test 'a)
					 nil
					 (current-field)
					 sym)
				   *current-branch*)))))

(defun cmp-element nil  ; Randy.Gobbel 13-Jun-86 14:41 
       (cond ((eq (peek-sublex)
		  '{)
	      (cmp-product))
	     (t (cmp-atomic-or-any))))

(defun cmp-new-eq-var (name old) ; John.Laird 11-Jun-86 15:23 
       (prog (pred next)
	     (set-branch-vars *current-branch*
			      (delq old (branch-vars *current-branch*)))
	     (setq next (soarassq name (branch-vars *current-branch*)))
	     (cond (next (cmp-new-eq-var name next))
		   (t (cmp-new-var name 'eq)))
	     (setq pred (cadr old))
	     ;; This get here fails and returns nil at certain points. 
	     ;; I think that it should be generating an teqs.
	     (link-new-node (list (get pred 's)
				  nil
				  (field-name (caddr old))
				  (current-field))
			    *current-branch*)))

(defun cmp-new-var (name test) ; Randy.Gobbel 13-May-86 14:04 
       (set-branch-vars *current-branch* (cons (list name test 1 *subnum*)
					       (branch-vars *current-branch*))))

(defun cmp-number (test) ; John.Laird 11-Jun-86 15:23 
       (link-new-node (list (get test 'n)
			    nil
			    (current-field)
			    (sublex))
		      *current-branch*))

(defun cmp-old-eq-var (test old) ; John.Laird 11-Jun-86 15:23 
       (link-new-node (list (get test 's)
			    nil
			    (current-field)
			    (field-name (cadddr old)))
		      *current-branch*))

(defun cmp-p (name type matrix) ; Randy.Gobbel 12-Sep-86 12:05  ; COMPILE 
 ; A PRODUCTION GIVEN IN P-FORMAT BY MATRIX. 
       (prog (branch actions)
	     (cond ((soarlistp name)
		    (soarerror "Illegal production name" name))
		   ((equal (get name 'production)
			   matrix)
		    (return nil)))
	     (cond ((not (soarmemq type '(elaborate serial)))
		    (soarerror "Illegal production type" type)))
	     (and *print-pname* (traceprinc name))
	     (cond ((excise-p name)
		    (traceprinc (list 'excised name))))
	     (setq *pcount* (1+ *pcount*))
	     (setq *feature-count* 0)
	     (setq *rhs-bound-vars* nil)
	     (setq branch (cmp-conds matrix))
	     (setq *vars* (branch-vars branch))
	     (cond ((not (soarmemq name *pnames*))
		    (soarpush name *pnames*)))
	     (cond ((and (not *loading-default*)
			 (not (soarmemq name *user-pnames*)))
		    (soarpush name *user-pnames*)))
	     (setq actions (cdr (soarmemq '--> matrix)))
	     (check-rhs actions)
	     (link-new-node (list '&p *feature-count* name (encode-dope)
				  nil
				  (cons 'progn actions))
			    branch)
	     (soarputprop name type 'type)
	     (soarputprop name (check-goal-ctx-test matrix)
			  'autonomic) ; SAVE BACKPOINTERS TO THE MEMORY 
 ; NODES OF THE PRODUCTION JUST BUILT. 
	     (soarputprop name (nreverse (cons (branch-node branch)
					       (branch-bakptrs branch)))
			  'backpointers)
	     (soarputprop name matrix 'production)
	     (soarputprop name (compute-negation-index matrix)
			  'negation-index)
	     (soarputprop name (branch-node branch)
			  'topnode)
	     (return nil)))





(defun cmp-product nil  ; Randy.Gobbel  3-Apr-86 14:28 
       (prog (save)
	     (setq save (rest-of-ce))
	     (sublex)
	     la
	     (cond ((end-of-ce)
		    (cond ((soarmember '} save)
			   (soarerror "Wrong context for }" save))
			  (t (soarerror "Missing }" save))))
		   ((eq (peek-sublex)
			'})
		    (sublex)
		    (return nil)))
	     (cmp-atomic-or-any)
	     (go la)))


(defun cmp-symbol (test) ; Randy.Gobbel 13-Jun-86 14:42 
       (cond ((variablep (peek-sublex))
	      (cmp-var test))
	     ((numberp (peek-sublex))
	      (cmp-number test))
	     ((symbolp (peek-sublex))
	      (cmp-constant test))
	     (t (soarerror "Unrecognized symbol" (sublex)))))

(defun cmp-var (test) ; Randy.Gobbel 13-May-86 14:09 
       (prog (old name)
	     (setq name (sublex))
	     (setq old (soarassq name (branch-vars *current-branch*)))
	     (cond ((and old (eq (cadr old) 'eq))
		    (cmp-old-eq-var test old))
		   ((and old (eq test 'eq))
		    (cmp-new-eq-var name old))
		   ((and (eq test 'eq)
			 *context-field-found*)
		    (cmp-new-var name 'eqnil))
		   (t (cmp-new-var name test)))))

(defun compile-production (name type matrix) ; Randy.Gobbel 
 ; 18-Jun-86 16:32 
       (setq *p-type* type)
       (setq *p-name* name)
       (setq *matrix* matrix)
       (soarcatch '!error! (cmp-p *p-name* *p-type* *matrix*))
       (setq *p-name* nil))

(defun encode-dope nil  ; Randy.Gobbel 13-May-86 14:18 
       (prog (r all z k)
	     (setq all *vars*)
	     la
	     (and (atom all)
		  (return r))
	     (setq z (pop all))
	     (setq k (encode-pair (caddr z)
				  (cadddr z)))
	     (setq r (cons (car z)
			   (cons k r)))
	     (go la)))





(defun encode-pair (a b) ; John.Laird  8-Nov-85 16:23 
       (cons (1- a)
	     (1- b)))





(defun end-of-ce nil (atom *curcond*))





(defun equiv (a b) ; Dan.Scales 24-Jan-86 15:11  ; TWO MEMORY NODES ARE 
 ; EQUIVALENT IF THEY ARE THE SAME TYPE. TWO ALPHA-TEST NODES ARE 
 ; EQUIVALENT IF THEY ARE THE SAME TYPE AND CONTAIN THE SAME TEST. 
       (and (eq (car a)
		(car b))
	    (or (soarmemq (car a)
			  '(&lmem &rmem))
		(equal (cddr a)
		       (cddr b)))))





(defun eval-args (z) ; Randy.Gobbel 17-Jun-86 15:31 
       (prog (val)
	     (setq val nil)
	     (soarwhile z (setq val (append ($change (pop z))
					    val)))
	     (return (dreverse val))))





(defun eval-function (form) ; Randy.Gobbel  3-Apr-86 14:05 
       (cond ((not *in-rhs*)
	      (soarwarn "Functions cannot be used at top level" (car form)))
	     (t (eval form))))





(defun eval-nodelist (outs) ; Randy.Gobbel 23-Jun-86 19:05 
       (cond (*execute-path* (cond (*path* ((lambda (out)
						    (apply (car out)
							   (cdr out)))
					    (pop *path*)))))
	     (t (soarmapc #'(lambda (out)
				    (apply (car out)
					   (cdr out)))
			  outs))))





(defun eval-rhs (pname data) ; Randy.Gobbel 12-Sep-86 12:06 
       (prog (node port)
	     (setq *p-name* pname)
	     (cond ((and (or *ptrace* (soarmemq pname *tracep-list*))
			 (trace-problem-space?)
			 (trace-production?))
		    (setq port (trace-file))
		    (traceprintc (1+ *decide-count*))
		    (traceprinc3 ":" *cycle-count* " ")
		    (traceprinc pname)
		    (and *ptrace* *ttrace* (time-tag-print data port))))
	     (setq *data-matched* data)
	     (cond ((and *pfired* (get pname 'pfired))
		    (soarputprop pname (1+ (get pname 'pfired))
				 'pfired))
		   (*pfired* (soarputprop pname 1 'pfired)))
	     (setq node (get pname 'topnode))
	     (init-var-mem (var-part node))
	     (setq *in-rhs* t)
	     (setq *current-goal* (find-max-goal-depth *data-matched*))
	     (soarmapc #'condition-trace *data-matched*)
	     (cond ((null *never-learn*)
		    (setq *data-matched* (append (add-negations *current-goal*
								(reverse 
							     *data-matched*))
						 *data-matched*))))
	     (eval (rhs-part node))
	     (post-process-results *current-goal*)
	     (setq *in-rhs* nil)))





(defun field-name (num) ; Randy.Gobbel  3-Apr-86 14:29 
       (cond ((and (> num 0)
		   (< num 65))
	      (cond ((<= num 9)
		     (getvector *carray* (1- num)))
		    ((soarpack '*c (soar-i-to-s num)
			       '*))))
	     ((soarerror "Condition is too long" (rest-of-ce)))))





(defun filter (elm) ; Randy.Gobbel 19-Jun-86 17:58 
       (prog (fl indx val)
	     (setq fl *filters*)
	     top
	     (and (atom fl)
		  (return t))
	     (setq indx (pop fl))
	     (setq val (pop fl))
	     (cond ((ident (soarnth (1- indx)
				    elm)
			   val)
		    (go top)))
	     (return nil)))





(defun find-equiv-beta-node (node list)
       (prog (a)
	     (setq a list)
	     l1
	     (cond ((atom a)
		    (return nil))
		   ((beta-equiv node (car a))
		    (return (car a))))
	     (setq a (cdr a))
	     (go l1)))





(defun find-equiv-node (node list)
       (prog (a)
	     (setq a list)
	     l1
	     (cond ((atom a)
		    (return nil))
		   ((equiv node (car a))
		    (return (car a))))
	     (setq a (cdr a))
	     (go l1)))





(defun find-left-mem (node) ; Dan.Scales  5-Mar-86 09:37  ; GET THE LEFT 
 ; MEMORY OF AN &AND NODE OR NOT-MEMORY OF A &NOT NODE. 
       (car (caddr node)))





(defun find-right-mem (node) ; Randy.Gobbel 13-May-86 14:18  ; RETURN LIST 
 ; OF TOKENS IN RIGHT (HASH) MEMORY OF &AND NODE. 
       (prog (i result tmp tmp1 mem)
	     (setq mem (caddr node))
	     (setq i 0)
	     loop
	     (and (>= i *mem-array-size*)
		  (return result))
	     (setq tmp (getvector mem i))
	     loop1
	     (cond ((null tmp)
		    (setq i (1+ i))
		    (go loop)))
	     (setq tmp1 (cdr (pop tmp)))
	     (cond (result (setq result (append tmp1 result)))
		   (t (setq result tmp1)))
	     (go loop1)))







(defun fudge (vars fudgefactor) ; Randy.Gobbel 13-May-86 14:18 
       (soarmapc #'(lambda (z)
			   (rplaca (cddr z)
				   (+ (caddr z)
				      fudgefactor)))
		 vars))



(defun gelm (x k) ; Randy.Gobbel 11-Sep-86 15:25 
       (prog (ce)
	     (setq ce (car k))
	     celoop
	     (and (eqp ce 0)
		  (go ph2))
	     (setq x (cdr x))
	     (and (eqp ce 1)
		  (go ph2))
	     (setq x (cdr x))
	     (and (eqp ce 2)
		  (go ph2))
	     (setq x (cdr x))
	     (and (eqp ce 3)
		  (go ph2))
	     (setq x (cdr x))
	     (and (eqp ce 4)
		  (go ph2))
	     (setq ce (- ce 4))
	     (go celoop)
	     ph2
	     (setq x (car x)) ; WARNING! NIL at the end of following 
 ; SELECTQ is a hack for FranzLisp 
	     (return (selectq (cdr k)
			      (0 (wme-class x))
			      (1 (wme-id x))
			      (2 (wme-attribute x))
			      (3 (wme-value x))
			      (4 (wme-reference x))
			      (5 (wme-goal x))
			      (6 (wme-problem-space x))
			      (7 (wme-state x))
			      (8 (wme-operator x))
			      ()
			      ))))


(defun get-wm (z)
       (setq *wm-filter* z)
       (setq *wm* nil)
       (mapwm #'get-wm2)
       (prog2 nil *wm* (setq *wm* nil)))





(defun get-wm2 (elem) ; RG: 20-Feb-86 20:57 
       (cond ((or (null *wm-filter*)
		  (soarmember (cdr elem)
			      *wm-filter*))
	      (setq *wm* (cons (car elem)
			       *wm*)))))






(defun incr-subnum nil  ; RG: 20-Feb-86 12:03 
       (setq *subnum* (1+ *subnum*)))





(defun init-var-mem (vlist)
       (prog (v ind r)
	     (setq *variable-memory* nil)
	     top
	     (and (atom vlist)
		  (return nil))
	     (setq v (car vlist))
	     (setq ind (cadr vlist))
	     (setq vlist (cddr vlist))
	     (setq r (gelm *data-matched* ind))
	     (setq *variable-memory* (cons (cons v r)
					   *variable-memory*))
	     (go top)))






  
(defun kill-node (node) ; edited: 16-Feb-84 11:34 
       (prog nil top (and (atom node)
			  (return nil))
	     (rplaca node '&old)
	     (setq node (cdr node))
	     (go top)))





(defun left-outs (node)
       (cadr node))





(defun link-both (left right succ path) ; Dan.Scales 24-Jan-86 15:16  ; 
 ; LINK SUCC AS AN OUTPUT TO BOTH LEFT AND RIGHT, UNLESS AN EQUIVALENT 
 ; NODE ALREADY EXISTS. 
       (prog (a r)
	     (setq a (intrq (mem-outs left)
			    (mem-outs right)))
	     (setq r (find-equiv-beta-node succ a))
	     (and r (return (cons r path)))
	     (setq *real-cnt* (iplus 1 *real-cnt*))
	     (attach-left left succ)
	     (attach-left right succ)
	     (return (cons succ path))))





(defun link-left (succ path) ; Randy.Gobbel 22-Aug-86 13:56  ; ADD SUCC TO 
 ; THE NESS AN EQUIVALENT NODE ALREADY EXISTS. IF SUCC IS A MEMORY OR &P 
 ; NODE, PRIME IT IF NECESSARY. RETURN UPDATE TO PATH. 
       (prog (pred a r class class-list attr pair flag)
	     (setq pred (car path))
	     (setq flag *last-node-on-bus*)
	     (setq *last-node-on-bus* nil) ; CHECK IF WE ARE COMPILING A 
 ; NODE THAT DOES A CLASS NAME CHECK OR ATTRIBUTE NAME CHECK. IF SO, STORE 
 ; THE NODE SPECIALLY SO WE CAN INDEX OFF CLASS AND ATTRIBUTE NAMES. 
	     (cond ((and (eq (car pred)
			     '&bus)
			 (eq (car succ)
			     'teqa)
			 (eq (caddr succ)
			     '*c1*))
		    (setq class (cadddr succ))
		    (setq class-list (get class '&bus-branch))
		    (setq *last-node-on-bus* t)
		    (cond ((null class-list)
			   (soarputprop class (cons succ nil)
					'&bus-branch))
			  (t (return (cons (car class-list)
					   path)))))
		   ((and flag (eq (car succ)
				  'teqa)
			 (eq (caddr succ)
			     '*c3*))
		    (setq class (cadddr pred))
		    (setq class-list (get class '&bus-branch))
		    (setq attr (cadddr succ))
		    (cond ((setq pair (soarassq attr (cdr class-list)))
			   (return (cons (cdr pair)
					 path)))
			  (t (soarputprop class (cons (car class-list)
						      (cons (cons attr succ)
							    (cdr class-list)))
					  '&bus-branch))))
		   (t (setq a (left-outs pred))
		      (setq r (find-equiv-node succ a))
		      (and r (cond ((soarmemq (car succ)
					      '(&p &lmem &rmem))
				    (return (list r)))
				   (t (return (cons r path)))))
		      (attach-left pred succ)))
	     (setq *real-cnt* (iplus 1 *real-cnt*))
	     (cond ((soarmemq (car succ)
			      '(&p &lmem &rmem))
		    (execute-node-path (cons succ path))
		    (return (list succ)))
		   (t (return (cons succ path))))
	     (return succ)))





(defun link-new-beta-node (r lbranch rbranch) ; Randy.Gobbel 
 ; 11-Jun-86 14:24 
       (setq *virtual-cnt* (1+ *virtual-cnt*))
       (set-branch-path lbranch (link-both (branch-node lbranch)
					   (branch-node rbranch)
					   r
					   (branch-path lbranch))))





(defun link-new-node (r branch) ; Randy.Gobbel 22-Aug-86 13:41  ; LINK R 
 ; ONTO BRANCH * 
       (cond ((not (soarmemq (car r)
			     '(&p &lmem &rmem &and &not &all)))
	      (setq *feature-count* (1+ *feature-count*))))
       (setq *virtual-cnt* (1+ *virtual-cnt*))
       (set-branch-path branch (link-left r (branch-path branch))))






(defun make-bottom-node nil (setq *first-node* (list '&bus nil)))





(defun make-branch nil  ; Randy.Gobbel 20-Jun-86 14:35 
       (list nil nil nil nil))





(defun make-var-bind (var elem)
       (setq *variable-memory* (cons (cons var elem)
				     *variable-memory*)))





(defun mapwm (fn) ; RG: 20-Feb-86 18:02 
       (prog (wmpl part)
	     (setq wmpl *wmpart-list*)
	     lab1
	     (cond ((atom wmpl)
		    (return nil)))
	     (setq part (get (car wmpl)
			     'wmpart*))
	     (setq wmpl (cdr wmpl))
	     (soarmapc fn part)
	     (go lab1)))





(defun mark-conflicts (rem all)
       (cond ((not (null rem))
	      (mark-conflicts2 (car rem)
			       all)
	      (mark-conflicts (cdr rem)
			      all))))





(defun mark-conflicts2 (atm lst)
       (prog (l)
	     (setq l lst)
	     top
	     (and (atom l)
		  (return nil))
	     (conflict atm (car l))
	     (setq l (cdr l))
	     (go top)))





(defun match (flag wme) ; Randy.Gobbel 22-Apr-86 10:40  ; Send wme down 
 ; the network, with *flag-part* set to flag. 
       (setq *flag-part* flag)
       (setq *data-part* (list wme))
       (apply (car *first-node*)
	      (cdr *first-node*)))





(defun mem-outs (x) ; Randy.Gobbel 23-Jun-86 19:06 
       (cadr x))






(defun new-subnum (k) ; Randy.Gobbel  3-Apr-86 14:28 
       (or (numberp k)
	   (soarerror "Tab must be a number" k))
       (setq *subnum* (fix k)))





(defun not-left (outs mem tests own-mem flag) ; Randy.Gobbel 
 ; 11-Sep-86 15:29 
       (prog (memdp tlist tst lind rind res c i memlist memlist1)
	     (check-execute outs)
	     (setq c 0)
	     (setq i 0)
	     loopi
	     (and (>= i *mem-array-size*)
		  (go fin))
	     (setq memlist (getvector mem i))
	     fail
	     (cond ((null memlist)
		    (setq i (1+ i))
		    (go loopi)))
	     (setq memlist1 (cdr (pop memlist)))
	     loop1
	     (and (null memlist1)
		  (go fail))
	     (setq memdp (pop memlist1))
	     (setq tlist tests)
	     tloop
	     (cond ((null tlist)
		    (and flag (setq c (1+ c)))
		    (go loop1)))
	     (setq tst (pop tlist))
	     (setq lind (pop tlist))
	     (setq rind (pop tlist))
	     (setq res (soarapply tst (gelm memdp rind)
				  (gelm *data-part* lind)))
	     (cond (res (go tloop))
		   (t (or flag (setq c (1+ c)))
		      (go loop1)))
	     fin
	     (add-token own-mem *data-part* nil c)
	     (or (eqp c 0)
		 (return))
	     (setq tlist outs)
	     loop
	     (or tlist (return))
	     (setq tst (pop tlist))
	     (setq res (pop tst))
	     (cond ((eq res '&lmem)
		    (&lmem (car tst)
			   (cadr tst)))
		   ((or (eq res '&not)
			(eq res '&all))
		    (not-left (car tst)
			      (caddr tst)
			      (cadddr tst)
			      (cadr tst)
			      (eq res '&not)))
		   (t (apply res tst)))
	     (go loop)))





(defun not-right (outs mem tests flag) ; Randy.Gobbel 11-Sep-86 15:29 
       (prog (fp dp memdp tlist tst lind rind res inc newc)
	     (setq fp *flag-part*)
	     (setq dp *data-part*)
	     (cond ((eq fp 'new)
		    (setq inc 1)
		    (setq *flag-part* nil))
		   (t (setq inc -1)
		      (setq *flag-part* 'new)))
	     fail
	     (cond ((null mem)
		    (setq *flag-part* fp)
		    (return)))
	     (setq memdp (car mem))
	     (setq newc (cadr mem))
	     (setq tlist tests)
	     tloop
	     (and (null tlist)
		  (cond (flag (go succ))
			(t (go nosend))))
	     (setq tst (pop tlist))
	     (setq lind (pop tlist))
	     (setq rind (pop tlist))
	     (setq res (soarapply tst (gelm dp rind)
				  (gelm memdp lind)))
	     (cond (res (go tloop))
		   (t (cond (flag (go nosend)))))
	     succ
	     (setq newc (+ inc newc))
	     (rplaca (cdr mem)
		     newc)
	     (cond ((not (or (and (eqp inc -1)
				  (eqp newc 0))
			     (and (eqp inc 1)
				  (eqp newc 1))))
		    (go nosend)))
	     (setq tlist outs)
	     (setq *data-part* memdp)
	     loop
	     (or tlist (go donesend))
	     (setq tst (pop tlist))
	     (setq res (pop tst))
	     (cond ((eq res '&lmem)
		    (&lmem (car tst)
			   (cadr tst)))
		   ((or (eq res '&not)
			(eq res '&all))
		    (not-left (car tst)
			      (caddr tst)
			      (cadddr tst)
			      (cadr tst)
			      (eq res '&not)))
		   (t (apply res tst)))
	     (go loop)
	     donesend
	     (setq *data-part* dp)
	     nosend
	     (setq mem (cddr mem))
	     (go fail)))





(defun note-variable (var)
       (setq *rhs-bound-vars* (cons var *rhs-bound-vars*)))






(defun nlam-p (z) ; Randy.Gobbel 12-Sep-86 12:05 
       (traceprinc '*)
       (drain)
       (cond ((soarlistp (cadr z))
	      (compile-production (car z)
				  'elaborate
				  (cdr z)))
	     (t (compile-production (car z)
				    (cadr z)
				    (cddr z)))))






(defun peek-sublex nil (car *curcond*))





(defun ppattval nil  ; Randy.Gobbel 12-Sep-86 12:06 
       (prog (att val)
	     (setq att (cadr *ppline*))
	     (setq *ppline* (cddr *ppline*))
	     (cond ((or (null *ppline*)
			(eq (car *ppline*)
			    '^))
		    (setq val nil))
		   (t (setq val (getval))))
	     (cond ((> (+ (nwritn t)
			  (flatc att)
			  (flatc val))
		       76)
		    (traceprintc " ")
		    (indent)))
	     (traceprinc2 '^ att)
	     (soarmapc #'(lambda (z)
				 (traceprinc2 " " z))
		       val)))

(defun promote-var (branch dope) ; Randy.Gobbel 13-May-86 14:23 
       (prog (vname vpred)
	     (setq vname (car dope))
	     (setq vpred (cadr dope))
	     (and (neq vpred 'eqnil)
		  (neq vpred 'eq)
		  (soarerror "Illegal predicate for first occurrence"
			     (list vname vpred))) ; EQNIL IS NOT A VALID 
 ; PREDICATE FOR FIRST OCCURRENCE 
	     (and (eq vpred 'eqnil)
		  (rplaca (cdr dope)
			  'eq))
	     (set-branch-vars branch (cons dope (branch-vars branch)))))





(defun protolmem nil  ; Randy.Gobbel 23-Jun-86 19:06 
       (protomem))





(defun protomem nil (list nil))





(defun protonotmem nil  ; Randy.Gobbel 23-Jun-86 19:06 
       (protomem))





(defun protormem nil  ; Randy.Gobbel 23-Jun-86 19:06 
       (protomem))





(defun real-add-token (lis data-part hashflag num) ; Randy.Gobbel 
 ; 23-Jun-86 18:26 
       (prog (hashval id l idl)
	     (setq *current-token* (1+ *current-token*))
	     (setq *added-tokens* (1+ *added-tokens*))
	     (cond (num (rplaca lis (cons data-part (cons num (car lis)))))
		   (hashflag (setq id (wme-id (car data-part)))
			     (setq hashval (get-hash-id id))
			     (setq l (getvector lis hashval))
			     (setq idl (soarassq id l))
			     (cond (idl (rplacd idl (cons data-part (cdr idl))))
				   (t (putvector lis hashval
						 (cons (list id data-part)
						       l)))))
		   (t (rplaca lis (cons data-part (car lis)))))))






(defun rematm (atm list) ; Randy.Gobbel 11-Sep-86 15:31 
       (cond ((atom list)
	      list)
	     ((eqp atm (car list))
	      (rematm atm (cdr list)))
	     (t (cons (car list)
		      (rematm atm (cdr list))))))






(defun remove-old (lis data hashloc num) ; Randy.Gobbel 21-Apr-86 12:18 
       (cond (num (remove-old-num lis data))
	     (hashloc (remove-old-hash lis data (wme-id (car data))))
	     (t (remove-old-no-num lis data))))





(defun remove-old-no-num (lis data) ; Randy.Gobbel 16-May-86 10:55 
       (prog (m next last)
	     (setq m (car lis))
	     (cond ((atom m)
		    (return nil))
		   ((top-levels-eq data (car m))
		    (setq *current-token* (1- *current-token*))
		    (setq *removed-tokens* (1+ *removed-tokens*))
		    (rplaca lis (cdr m))
		    (return (car m))))
	     (setq next m)
	     loop
	     (setq last next)
	     (setq next (cdr next))
	     (cond ((atom next)
		    (return nil))
		   ((top-levels-eq data (car next))
		    (rplacd last (cdr next))
		    (setq *current-token* (1- *current-token*))
		    (setq *removed-tokens* (1+ *removed-tokens*))
		    (return (car next)))
		   (t (go loop)))))





(defun remove-old-num (lis data) ; Randy.Gobbel 16-May-86 10:56 
       (prog (m next last)
	     (setq m (car lis))
	     (cond ((atom m)
		    (return nil))
		   ((top-levels-eq data (car m))
		    (setq *current-token* (1- *current-token*))
		    (setq *removed-tokens* (1+ *removed-tokens*))
		    (rplaca lis (cddr m))
		    (return (car m))))
	     (setq next m)
	     loop
	     (setq last next)
	     (setq next (cddr next))
	     (cond ((atom next)
		    (return nil))
		   ((top-levels-eq data (car next))
		    (rplacd (cdr last)
			    (cddr next))
		    (setq *current-token* (1- *current-token*))
		    (setq *removed-tokens* (1+ *removed-tokens*))
		    (return (car next)))
		   (t (go loop)))))






(defun rest-of-ce nil *curcond*)





(defun rhs-part (pnode)
       (caddr (cdddr pnode)))






(defun set-branch-bakptrs (x y) ; Randy.Gobbel 23-Jun-86 19:07 
       (rplaca (cdddr x)
	       y))





(defun set-branch-path (x y) ; Randy.Gobbel 23-Jun-86 19:07 
       (rplaca (cdr x)
	       y))





(defun set-branch-tsize (x y) ; Randy.Gobbel 23-Jun-86 19:07 
       (rplaca (cddr x)
	       y))





(defun set-branch-vars (x y) ; Randy.Gobbel 23-Jun-86 19:07 
       (rplaca x y))





(defun sublex nil  ; Randy.Gobbel  9-Jun-86 16:29 
       (prog1 (car *curcond*)
	      (setq *curcond* (cdr *curcond*))))





(defun teqa (outs register constant) ; Randy.Gobbel 11-Sep-86 17:53 
       (prog (a)
	     (setq a (eval register))
	     (return (cond ((eq a constant)
			    (eval-nodelist outs))
			   ((and (numberp a)
				 (numberp constant)
				 (eqp a constant)
				 (eval-nodelist outs)))
			   (t nil)))))





(defun teqb (new eqvar) ; Randy.Gobbel 11-Sep-86 15:26 
       (cond ((eq new eqvar)
	      t)
	     ((not (numberp new))
	      nil)
	     ((not (numberp eqvar))
	      nil)
	     ((eqp new eqvar)
	      t)
	     (t nil)))





(defun teqn (outs register constant) ; Randy.Gobbel 11-Sep-86 15:26 
       (prog (z)
	     (setq z (eval register))
	     (and (numberp z)
		  (eqp z constant)
		  (eval-nodelist outs))))





(defun teqs (outs vara varb) ; Randy.Gobbel 11-Sep-86 15:26 
       (prog (a b)
	     (setq a (eval vara))
	     (setq b (eval varb))
	     (cond ((eq a b)
		    (eval-nodelist outs))
		   ((and (numberp a)
			 (numberp b)
			 (eqp a b))
		    (eval-nodelist outs)))))





(defun tgeb (new eqvar)
       (cond ((not (numberp new))
	      nil)
	     ((not (numberp eqvar))
	      nil)
	     ((not (greaterp eqvar new))
	      t)
	     (t nil)))





(defun tgen (outs register constant) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (z)
	     (setq z (eval register))
	     (and (numberp z)
		  (not (greaterp constant z))
		  (eval-nodelist outs))))





(defun tges (outs vara varb) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (a b)
	     (setq a (eval vara))
	     (setq b (eval varb))
	     (and (numberp a)
		  (numberp b)
		  (not (greaterp b a))
		  (eval-nodelist outs))))





(defun tgtb (new eqvar)
       (cond ((not (numberp new))
	      nil)
	     ((not (numberp eqvar))
	      nil)
	     ((greaterp new eqvar)
	      t)
	     (t nil)))





(defun tgtn (outs register constant) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (z)
	     (setq z (eval register))
	     (and (numberp z)
		  (greaterp z constant)
		  (eval-nodelist outs))))





(defun tgts (outs vara varb) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (a b)
	     (setq a (eval vara))
	     (setq b (eval varb))
	     (and (numberp a)
		  (numberp b)
		  (greaterp a b)
		  (eval-nodelist outs))))






(defun tleb (new eqvar)
       (cond ((not (numberp new))
	      nil)
	     ((not (numberp eqvar))
	      nil)
	     ((not (greaterp new eqvar))
	      t)
	     (t nil)))





(defun tlen (outs register constant) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (z)
	     (setq z (eval register))
	     (and (numberp z)
		  (not (greaterp z constant))
		  (eval-nodelist outs))))





(defun tles (outs vara varb) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (a b)
	     (setq a (eval vara))
	     (setq b (eval varb))
	     (and (numberp a)
		  (numberp b)
		  (not (greaterp a b))
		  (eval-nodelist outs))))





(defun tltb (new eqvar)
       (cond ((not (numberp new))
	      nil)
	     ((not (numberp eqvar))
	      nil)
	     ((greaterp eqvar new)
	      t)
	     (t nil)))





(defun tltn (outs register constant) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (z)
	     (setq z (eval register))
	     (and (numberp z)
		  (greaterp constant z)
		  (eval-nodelist outs))))





(defun tlts (outs vara varb) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (a b)
	     (setq a (eval vara))
	     (setq b (eval varb))
	     (and (numberp a)
		  (numberp b)
		  (greaterp b a)
		  (eval-nodelist outs))))





(defun tnea (outs register constant) ; Randy.Gobbel 11-Sep-86 17:52 
       (prog (a)
	     (setq a (eval register))
	     (return (cond ((eq a constant)
			    nil)
			   ((and (numberp a)
				 (numberp constant))
			    (and (not (eqp a constant))
				 (eval-nodelist outs)))
			   (t (eval-nodelist outs))))))





(defun tneb (new eqvar) ; Randy.Gobbel 11-Sep-86 15:26 
       (cond ((eq new eqvar)
	      nil)
	     ((not (numberp new))
	      t)
	     ((not (numberp eqvar))
	      t)
	     ((eqp new eqvar)
	      nil)
	     (t t)))





(defun tnen (outs register constant) ; Randy.Gobbel 11-Sep-86 15:27 
       (prog (z)
	     (setq z (eval register))
	     (and (or (not (numberp z))
		      (not (eqp z constant)))
		  (eval-nodelist outs))))





(defun tnes (outs vara varb) ; Randy.Gobbel 11-Sep-86 15:27 
       (prog (a b)
	     (setq a (eval vara))
	     (setq b (eval varb))
	     (cond ((eq a b)
		    (return nil))
		   ((and (numberp a)
			 (numberp b)
			 (eqp a b))
		    (return nil))
		   (t (eval-nodelist outs)))))





(defun top-levels-eq (la lb) ; Randy.Gobbel 11-Sep-86 17:56 
       (prog nil lx (cond ((eqp la lb)
			   (return t))
			  ((null la)
			   (return nil))
			  ((null lb)
			   (return nil))
			  ((not (eqp (car la)
				     (car lb)))
			   (return nil)))
	     (setq la (cdr la))
	     (setq lb (cdr lb))
	     (go lx)))





(defun txxa (outs register constant) ; Randy.Gobbel  7-Apr-86 14:42 
       (and (symbolp (eval register))
	    (eval-nodelist outs)))





(defun txxb (new eqvar)
       (cond ((numberp new)
	      (cond ((numberp eqvar)
		     t)
		    (t nil)))
	     (t (cond ((numberp eqvar)
		       nil)
		      (t t)))))





(defun txxn (outs register constant) ; Randy.Gobbel  7-Apr-86 14:42 
       (prog (z)
	     (setq z (eval register))
	     (and (numberp z)
		  (eval-nodelist outs))))





(defun txxs (outs vara varb) ; Randy.Gobbel  7-Apr-86 14:42 

       (prog (a b)
	     (setq a (eval vara))
	     (setq b (eval varb))
	     (cond ((and (numberp a)
			 (numberp b))
		    (eval-nodelist outs))
		   ((and (not (numberp a))
			 (not (numberp b)))
		    (eval-nodelist outs)))))





(defun var-dope (var) ; edited: 23-Mar-86 09:17 
       (soarassq var *vars*))





(defun var-part (pnode)
       (car (cdddr pnode)))






(defun &lmem (outs memory) ; Dan.Scales 27-Jan-86 03:50 
       (add-token memory *data-part* nil nil)
       (check-execute outs)
       (soarmapc #'(lambda (node)
			   (setq node (cdr node))
			   (and-left (car node)
				     (caddr node)
				     (cadddr node)))
		 outs))





(defun &rmem (outs memory) ; Randy.Gobbel 13-May-86 13:55 
       (add-token memory *data-part* t nil)
       (check-execute outs)
       (soarmapc #'(lambda (node)
			   (cond ((eq (car node)
				      '&and)
				  (and-right (cadr node)
					     (caaddr node)
					     (cadddr (cdr node))))
				 ((eq (pop node)
				      '&not)
				  (not-right (car node)
					     (caadr node)
					     (cadddr node)
					     t))
				 (t (not-right (car node)
					       (caadr node)
					       (cadddr node)
					       nil))))
		 outs))





(defun execute-node-path (path) ; Randy.Gobbel 18-Mar-86 15:29  ; SEND 
 ; TOKENS FROM TOP NODE OF PATH THROUGH EACH NODE OF THE PATH * 
       (prog (node)
	     (setq path (nreverse path))
	     (setq node (car path))
	     (setq *execute-path* t)
	     (cond ((eq (car node)
			'&bus)
		    (setq *save-path* path)
		    (mapwm #'(lambda (x)
				     (push-wme-down-path (list (car x))))))
		   ((eq (car node)
			'&lmem)
		    (setq *save-path* (cdr path))
		    (soarmapc #'(lambda (x)
					(push-wme-down-path x))
			      (car (caddr node)))))
	     (setq *execute-path* nil)))





(defun hash-id (id) ; Randy.Gobbel 20-Mar-86 22:30 
       (remainder (subnumber id)
		  *mem-array-size*))





(defun protohmem nil  ; edited:  1-Feb-86 09:38 
       (makevector *mem-array-size*))





(defun push-wme-down-path (wme) ; Randy.Gobbel 18-Mar-86 15:30  ; PUSH WME 
 ; THROUGH THE NODES INDICATED BY PATH, STOPPING AT THE FIRST FAILING NODE 
       (prog (node)
	     (setq *flag-part* 'new)
	     (setq *data-part* wme)
	     (setq *path* *save-path*)
	     (setq node (pop *path*)) ; BEGINNING OF A PATH IS EITHER THE 
 ; &BUS NODE OR AN &AND NODE BELOW AN &LMEM NODE. 
	     (cond ((eq (pop node)
			'&bus)
		    (&bus (car node)))
		   (t (and-left (car node)
				(caddr node)
				(cadddr node))))))





(defun remove-old-hash (lis data hashid) ; Randy.Gobbel 16-May-86 10:57 
       (prog (m idlist next last)
	     (setq m (getvector lis (get-hash-id hashid)))
	     (setq idlist (soarassq hashid m))
	     (cond ((null idlist)
		    (return nil)))
	     (setq next idlist)
	     loop
	     (setq last next)
	     (setq next (cdr next))
	     (cond ((atom next)
		    (return nil))
		   ((top-levels-eq data (car next))
		    (rplacd last (cdr next))
		    (or (cdr idlist)
			(putvector lis (get-hash-id hashid)
				   (dremove idlist m)))
		    (setq *current-token* (1- *current-token*))
		    (setq *removed-tokens* (1+ *removed-tokens*))
		    (return (car next)))
		   (t (go loop)))))





(defun teqnilb (x y) ; Randy.Gobbel 11-Sep-86 17:58 
       (or (eq x y)
	   (null x)
	   (and (numberp x)
		(numberp y)
		(eqp x y))))




;;; Concatenated from type module "rete" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/rete/release/reorder.lisp".
;;; -*-mode: lisp; package: user -*-


(defun all-bound? (vlist) ; Randy.Gobbel 26-Mar-86 20:48  ; John.Laird 
 ; 14-Oct-85 15:44 
       (soar-do ((vl1 vlist (cdr vl1)))
		((null vl1)
		 t)
		(cond ((not (bound-var? (car vl1)))
		       (return nil)))))





(defun bound-preference? (pr) ; Randy.Gobbel 26-Mar-86 20:49  ; John.Laird 
 ; 7-Oct-85 10:53 
       (all-bound? (get-context-vars-in-pref pr)))





(defun bound-var? (v) ; Randy.Gobbel 26-Mar-86 20:49  ; John.Laird 
 ; 27-Mar-85 09:40 
       (get v 'bound))





(defun check-negations (negations) ; Randy.Gobbel 13-May-86 14:04 
       (soar-do ((neg negations (cddr neg))
		 (active)
		 (unactive))
		((null neg)
		 (cons active unactive))
		(cond ((all-bound? (get-all-vars-in-neg (car neg)))
		       (soarpush (cadr neg)
				 active)
		       (soarpush (strip-condition (car neg))
				 active))
		      (t (soarpush (cadr neg)
				   unactive)
			 (soarpush (car neg)
				   unactive)))))





(defun
  classify
  (dup-vars conditions flag) ; Randy.Gobbel 11-Sep-86 18:08  ; CLASSIFY 
 ; THE CONDITIONS OF OLD-CONDITIONS INTO GROUPS - RETURN A PAIR OF THE 
 ; RANK OF THE HIGHEST RANKING GROUP AND THE LIST OF ELEMENTS IN THAT 
 ; GROUP BASED ON THE TYPES OF ELEMENTS IN THEIR ATTRIBUTE AND VALUE 
 ; FIELDS - DUP-VARS A LIST OF ALL VARIABLES THAT OCCUR MORE THAN ONCE IN 
 ; THE ORIGINAL CONDITION LIST BEING ORDERED 
  (prog (attr-type attr-val val-type val-val temp c i mini result)
	(setq mini 102)
	l1
	(cond ((null conditions)
	       (return (rplnode *global-pair* mini (nreverse result)))))
	(setq c (pop conditions))
	(cond ((eq (get-id-var c)
		   'conj)
	       (setq i 0)
	       (go l2))
	      ((setq i (get-prev-rank c)) ; USE PREVIOUSLY CALCULATED RANK 
 ; IF IT EXISTS 
	       (cond (i (go l2))))
	      ((eq (get-condition-type c)
		   'preference)
	       (cond ((eq 'operator (wme-role (cdr c)))
		      (setq i (+ 6 *default-multi*)))
		     (t (setq i 3)))
	       (and flag (put-rank c i)))
	      (t (setq temp (get-attr-val-types c))
		 (setq attr-type (caar temp))
		 (setq val-type (cadr temp))
		 (setq attr-val (cdar temp))
		 (setq val-val (cddr temp)) ; CHANGE TYPE OF FIELD TO 
 ; SET-VAR IF THE TYPE IS ALREADY VAR AND THE VARIABLE IN THE FIELD IS IN 
 ; SET-VAR 
		 (cond ((and (eq attr-type 'var)
			     (bound-var? attr-val))
			(setq attr-type 'set-var)))
		 (cond ((and (eq val-type 'var)
			     (bound-var? val-val))
			(setq val-type 'set-var)))
		 (cond ((and (eq attr-type 'const)
			     (eq val-type 'const))
			(setq i 1)
			(put-rank c 1))
		       ((and (soarmemq attr-type '(const set-var))
			     (soarmemq val-type '(const set-var)))
			(setq i 2)
			(and flag (put-rank c 2)))
		       ((eq (get-condition-type c)
			    'goal)
			(cond ((eqp (get-mult-num (get-condition-type c)
						  attr-val)
				    1)
			       (setq i 4))
			      (t (setq i 5)))
			(put-rank c i))
		       ((and (eq attr-type 'const)
			     (eq val-type 'var)
			     (soarmemq val-val dup-vars))
			(setq i (+ 6 (get-mult-num (get-condition-type c)
						   attr-val))))
		       ((and (soarmemq attr-type '(var set-var))
			     (soarmemq attr-val dup-vars)
			     (soarmemq val-type '(const var set-var))
			     (soarmemq val-val dup-vars))
			(setq i (+ 6 *default-multi*)))
		       (t (setq i 101)
			  (put-rank c 101)))))
	l2
	(cond ((< i mini)
	       (setq mini i)
	       (setq result (list c)))
	      ((= i mini)
	       (soarpush c result)))
	(go l1)))





(defun classify-match-elt (x) ; Randy.Gobbel 10-Jun-86 14:03  ; Dan.Scales 
 ; 23-Mar-85 10:33  ; RETURN A (TYPE VALUE) LIST CLASSIFYING THE NEXT 
 ; MATCH ELEMENT (VARIABLE - CONSTANT - PREDICATE EXPRESSION OR 
 ; DISJUNCTION) TYPE CAN BE VARIABLE - CONSTANT OR OTHER * 
       (cond ((and (soarlistp (car x))
		   (eq (caar x)
		       '<<))
	      (rplnode *global-pair* (cdr x)
		       '(other)))
	     ((predicatep (car x))
	      (rplnode *global-pair* (cddr x)
		       '(other)))
	     ((variablep (car x))
	      (rplnode *global-pair* (cdr x)
		       (cons 'var (car x))))
	     (t (rplnode *global-pair* (cdr x)
			 (cons 'const (car x))))))





(defun
  classify-term
  (x) ; Randy.Gobbel 12-Jun-86 16:45  ; Dan.Scales 23-Mar-85 10:33  ; 
 ; RETURN (TYPE VALUE) LIST CLASSIFYING THE NEXT MATCH TERM (VARIABLE 
 ; CONSTANT PREDICATE DISJUNCTION OR CONJUNCTION OF MATCH ELEMENTS) IF THE 
 ; TERM IS A CONJUNCTION - IT IS CLASSIFIED ACCORDING TO ITS NON-PREDICATE 
 ; COMPONENT 
  (prog
    (result)
    (setq result '(other))
    (cond ((and (soarlistp (car x))
		(eq (caar x)
		    '{))
	   (return (prog (next-elt y)
			 (setq y (cdr x))
			 (setq x (cdar x))
			 loop
			 (cond ((eq (car x)
				    '})
				(return (rplnode *global-pair* y result)))
			       (t (setq next-elt (pop-match-elt x))
				  (cond ((soarmemq (car next-elt)
						   '(var const))
					 (setq result next-elt)))
				  (go loop))))))
	  (t (return (classify-match-elt x))))))





(defun dup-action (condition action-list) ; John.Laird 19-Jun-86 17:23 
       (prog (shared-variable action)
	     (setq shared-variable (car (intrq condition *suspected-duplicates*)
					))
	     l1
	     (setq action (pop action-list))
	     (cond ((soarmemq shared-variable action)
		    (return action))
		   ((null action)
		    (return)))
	     (go l1)))





(defun find-all-vars-in-condition (c) ; Randy.Gobbel 15-Jul-86 17:50  ; 
 ; Dan.Scales 23-Mar-85 10:33 
       (soar-do ((vars))
		((null c)
		 vars)
		(cond ((variablep (car c))
		       (soarpush (car c)
				 vars)
		       (pop c))
		      ((and (soarlistp (car c))
			    (eq (caar c)
				'<<))
		       (pop c))
		      ((soarlistp (car c))
		       (setq vars (append (find-all-vars-in-condition
					    (cdar c))
					  vars))
		       (pop c))
		      (t (pop c)))))





(defun find-best-condition (dup-vars active-conditions un-active-conditions ccl 
				     recurse-level minsofar) ; 
 ; Randy.Gobbel 26-Mar-86 21:54  ; RETURN THE BEST CONDITION TO APPEAR 
 ; NEXT IN THE ORDERING - DUP-VARS A LIST OF THE VARIABLES THAT OCCUR MORE 
 ; THAN ONCE IN THE ORIGINAL CONDITION LIST TO BE ORDERED - 
 ; ACTIVE-CONDITIONS ARE THE CONDITIONS WHOSE ID IS A SET-VAR AND HENCE 
 ; ARE ELIGIBLE FOR ORDERING - UN-ACTIVE-CONDITIONS IS A LIST OF THE 
 ; CONDITIONS THAT ARE TO BE ORDERED BUT ARE NOT YET ACTIVE, CCL IS A 
 ; CLASSIFICATION OF THE ACTIVE-CONDITONS, AND RECURSE-LEVEL IS HOW MANY 
 ; TIMES FIND-BEST-CONDITION HAS BEEN CALLED RECURSIVELY. 
       (prog (ccl1 i c tie-conds minc minrank temp new-vars act-conds 
		   un-act-conds s-v best)
	     (setq i (car ccl))
	     (setq tie-conds (cdr ccl)) ; USING GROUP RETURNED BY CLASSIFY 
 ; - EITHER TAKE THE TOP CONDITION OR TRY TO BREAK THE TIE VIA A RECURSIVE 
 ; CALL TO FIND-BEST-CONDITIONS. TAKE THE TOP CONDITION IF: THE CURRENT 
 ; GROUP IS A THROUGH E OR GROUP I; THERE IS ONLY ONE CONDITION AND THE 
 ; CURRENT GROUP IS NOT GROUP 6 IN A RECURSIVE CALL; OR WE HAVE ALREADY 
 ; REACHED THE MAXIMUM RECURSION. 
	     (cond ((> i 101)
		    (return nil))
		   ((or (<= i 6)
			(= i 101)
			(and (null (cdr tie-conds))
			     (or (not (= i 7))
				 (= recurse-level 0)))
			(= recurse-level *max-recurse*)) ; TAKE TOP 
 ; ELEMENT OF GROUP 
		    (setq c (car tie-conds))
		    (return (cons (list i)
				  c)))) ; ATTEMPT TO BREAK A TIE BY 
 ; RECURSIVE CALLS 
	     (setq minrank '(102)) ; STOP THIS SEARCH IF WE'VE ALREADY 
 ; FOUND A CONDITION THAT'S BETTER 
	     (cond ((and minsofar (or (> i (car minsofar))
				      (and (= i (car minsofar))
					   (cdr minsofar)
					   (= 1 (cadr minsofar)))))
		    (return '((100)))))
	     l2 ; RECURSIVELY RANK NEXT TYING CONDITION 
	     (setq c (pop tie-conds))
	     (and (null c)
		  (go l3))
	     (setq temp (get-attr-val-types c))
	     (cond ((eq (cadr temp)
			'const) ; NO VAR IN VALUE FIELD SO USE VAR IN ATTR 
 ; FIELD 
		    (setq new-vars (list (cdar temp))))
		   (t (setq new-vars (list (cddr temp)))))
	     (setq act-conds (remove c active-conditions))
	     (setq s-v (mark-as-set new-vars))
	     (setq temp (get-conditions-with-vars s-v un-active-conditions))
	     (setq act-conds (append (car temp)
				     act-conds))
	     (setq un-act-conds (cdr temp))
	     (setq ccl1 (classify dup-vars act-conds nil))
	     (setq best (find-best-condition dup-vars act-conds un-act-conds 
					     ccl1 (+ 1 recurse-level)
					     (cdr minrank)))
	     (unmark-as-set s-v) ; THIS LATEST CONDITION BETTER THAN THE 
 ; ONES SO FAR? 
	     (cond ((or (and (equal (car best)
				    minrank)
			     (eq (get-id-var c)
				 *last-value-var*))
			(lexless (car best)
				 minrank)) ; SAVE IT 
		    (setq minrank (car best))
		    (setq minc c)))
	     (cond ((eq (car minrank)
			1)
		    (go l3)))
	     (go l2)
	     l3 ; RETURN BEST CONDITION 
	     (return (cons (cons i minrank)
			   minc))))





(defun find-info-on-cond (c flag) ; Randy.Gobbel 16-Jul-86 16:47 
       (prog (attr val id vars next-term prefvars)
	     (setq attr '(const))
	     (setq val '(const))
	     (setq next-term (pop-term c))
	     (cond ((eq (car next-term)
			'var)
		    (soarpush (cdr next-term)
			      vars)))
	     (setq next-term (pop-term c))
	     (cond ((eq (car next-term)
			'var)
		    (soarpush (cdr next-term)
			      vars)
		    (setq id (cdr next-term)))
		   ((eq (car next-term)
			'const)
		    (soarwarn "Constant identifier/object field in:  " c)
		    (setq id 'const))
		   ((eq (car next-term)
			'other)
		    (soarwarn "Identifier/object field not variable in:  " c)
		    (setq id nil))
		   (t (setq id (cdr next-term))))
	     (setq next-term (pop-term c))
	     (cond ((eq (car next-term)
			'var)
		    (soarpush (cdr next-term)
			      vars)))
	     (setq attr next-term)
	     (setq next-term (pop-term c))
	     (cond ((eq (car next-term)
			'var)
		    (soarpush (cdr next-term)
			      vars)))
	     (setq val next-term)
	     (cond (flag (soarwhile c (setq next-term (pop-term c))
				    (cond ((eq (car next-term)
					       'var)
					   (soarpush (cdr next-term)
						     prefvars))))
			 (return (list id vars prefvars nil))))
	     (return (list id vars (cons attr val)
			   nil))))





(defun find-pred-vars-in-condition (c) ; Randy.Gobbel 10-Jun-86 14:51  ; 
 ; Dan.Scales 23-Mar-85 10:33  ; RETURNS ALL VARIABLES APPEARING IN 
 ; PREDICATES IN CONDITION C - USED TO ENSURE THAT A CONDITION IS REJECTED 
 ; IF IT CONTAINS A PREDICATE WITH A VARIABLE THAT HASN'T BEEN BOUND YET 
       (soar-do ((type)
		 (varlist))
		((null c)
		 varlist)
		(cond ((predicatep (car c))
		       (pop c)
		       (setq type (pop-match-elt c))
		       (cond ((eq (car type)
				  'var)
			      (soarpush (cdr type)
					varlist))))
		      ((and (soarlistp (car c))
			    (eq (caar c)
				'<<))
		       (setq c (cdr c)))
		      ((soarlistp (car c))
		       (setq type (find-pred-vars-in-condition (pop c)))
		       (cond (type (setq varlist (append type varlist)))))
		      (t (pop c)))))





(defun get-all-vars-in-neg (neg) ; Dan.Scales 23-Mar-85 10:33 
       (car neg))





(defun get-attr-val-types (x) ; Dan.Scales 23-Mar-85 11:21 
       (cadr (cddar x)))





(defun get-condition-type (c) ; Dan.Scales 23-Mar-85 11:21 
       (cadr c))





(defun
  get-conditions-with-vars
  (vars cl) ; Randy.Gobbel 13-May-86 14:18  ; RETURNS A DOTTED PAIR OF A 
 ; LIST OF ALL CONDITIONS IN CL WHOSE ID VARIABLES ARE IN THE LIST VARS 
 ; AND WHICH DON'T HAVE ANY PREDICATES WITH VARIABLES NOT IN VARS AND A 
 ; LIST OF THE REST OF THE CONDITIONS 
  (cond
    ((null vars)
     nil)
    (t
      (soar-do
	((cl1 cl (cdr cl1))
	 (result-with)
	 (result-wo)
	 (id))
	((null cl1)
	 (cons (nreverse result-with)
	       (nreverse result-wo)))
	(setq id (get-id-var (car cl1)))
	(cond ((and (or (and (not (eq (get-condition-type (car cl1))
				      'preference))
			     (bound-var? id))
			(eq id 'const)
			(and (eq id 'conj)
			     (any-bound? (get-vars-in-condition (car cl1))))
			(and (eq (get-condition-type (car cl1))
				 'preference)
			     (bound-preference? (car cl1))))
		    (no-unbound-predicates? (car cl1)))
	       (soarpush (car cl1)
			 result-with))
	      (t (soarpush (car cl1)
			   result-wo)))))))





(defun get-context-vars-in-pref (pr) ; Dan.Scales 23-Mar-85 10:33 
       (cadr (cddar pr)))





(defun get-id-var (c) ; Dan.Scales 23-Mar-85 10:33  ; RETURN THE VARIABLE 
 ; ASSOCIATED WITH THE ID ATTRIBUTE IN CONDITION C * 
       (cadr (car c)))





(defun get-mult-num (type attr) ; Randy.Gobbel 11-Sep-86 18:09  ; 
 ; Dan.Scales 23-Mar-85 10:33 
       (soar-do ((l *multi-attribute* (cdr l)))
		((null l)
		 1)
		(cond ((and (eq (caar l)
				type)
			    (eqp (cadar l)
				 attr))
		       (cond ((car (cddar l))
			      (return (car (cddar l))))
			     (t (return *default-multi*)))))))





(defun get-pred-vars-in-condition (c) ; Dan.Scales 23-Mar-85 10:33 
       (car (car c)))





(defun get-prev-rank (c) ; Dan.Scales 23-Mar-85 10:33 
       (car (cddddr (car c))))





(defun get-vars-in-condition (c) ; Dan.Scales 23-Mar-85 10:33 
       (caddr (car c)))





(defun insert-ne-undec (c2) ; Randy.Gobbel 11-Sep-86 15:05  ; Insert <> 
 ; undecided along with any variables in c that do not appear in dup-vars 
 ; - which is meant to be a list of variables that appear more than once 
 ; in a production 
       (prog (val)
	     (cond ((not (soarmemq (wme-attribute c2)
				   '(problem-space state operator)))
		    (return c2)))
	     (setq val (wme-value c2))
	     (cond ((and (soarlistp val)
			 (eq (car val)
			     '<<)) ; Disjunction - no vars - just return 
		    (return c2))
		   ((and (soarlistp val)
			 (soarmemq 'undecided val)) ; Undec already 
 ; present 
		    (return c2))
		   ((variablep val) ; Simple variable - make list with 
 ; undec and return 
		    (setq val (append '({ <> undecided)
				      (list val '}))))
		   ((eq val 'undecided)
		    (return c2))
		   (t  ; Conjunction without undec - add it in 
		      (rplacd val (append '(<> undecided)
					  (cdr val)))))
	     (setf-wme-value c2 val)
	     (return c2)))





(defun
  instantiate-negated-condition
  (condition data-matched var-list current-goal) ; Randy.Gobbel 
 ; 19-Aug-86 11:36 
  (prog (element new-condition item new-id)
	(setq new-condition nil)
	(soarwhile condition (setq item (pop condition))
		   (cond ((variablep item)
			  (setq element (cdr (soarassq item var-list)))
			  (cond ((and element (soarlistp element))
				 (soarpush (soarnth (cadr element)
						    (soarnth (car element)
							     data-matched))
					   new-condition))
				((setq element
				       (cdr (soarassq item 
						      *secondary-variables*)))
				 (soarpush element new-condition))
				(t (setq new-id (soargensym (soarnthchar item 2)
							    ))
				   (soarpush new-id new-condition)
				   (soarpush (cons item new-id)
					     *secondary-variables*)
				   (soarputprop new-id
						(get (wme-id (reverse 
							      new-condition))
						     'in-id-field)
						'in-id-field)
				   (soarputprop new-id t 'id-test))))
			 (t (soarpush item new-condition))))
	(setq new-condition (nreverse new-condition))
	(cond ((negated-chunk-condition-test new-condition current-goal)
	       (return (list new-condition))))
	(return nil)))





(defun
  instantiate-remaining-conditions
  (conditions current-goal var-list data-matched symbol) ; Randy.Gobbel 
 ; 30-Jun-86 19:03 
  (prog
    (condition new-condition return-list)
    (soarwhile
      conditions
      (setq condition (pop conditions))
      (setq *secondary-variables* nil)
      (cond ((soarlistp (car condition))
	     (setq new-condition
		   (soarmapconc #'(lambda (x)
					  (instantiate-negated-condition x 
							       data-matched 
								   var-list 
							       current-goal))
				condition))
	     (cond (new-condition (soarpush symbol return-list)
				  (soarpush new-condition return-list))))
	    ((setq new-condition (instantiate-negated-condition condition 
							       data-matched 
								var-list 
							       current-goal))
	     (soarpush symbol return-list)
	     (soarpush (car new-condition)
		       return-list))))
    (return return-list)))





(defun lexless (a b) ; Dan.Scales 23-Mar-85 10:33 
       (soar-do ((a1 a (cdr a1))
		 (b1 b (cdr b1)))
		((null a1)
		 t)
		(cond ((null a1)
		       (return t))
		      ((null b1)
		       (return nil))
		      ((< (car a1)
			 (car b1))
		       (return t))
		      ((< (car b1)
			 (car a1))
		       (return nil)))))





(defun mark-as-set (vlist) ; Randy.Gobbel 11-Sep-86 15:08  ; John.Laird 
 ; 12-Sep-85 11:14 
       (cond ((or (null vlist)
		  (equal vlist '(nil)))
	      nil)
	     (t (soar-do ((vl1 (cond ((atom vlist)
				      (list vlist))
				     (t vlist))
			       (cdr vl1))
			  (result))
			 ((null vl1)
			  result)
			 (cond ((and (symbolp (car vl1))
				     (not (get (car vl1)
					       'bound)))
				(soarputprop (car vl1)
					     t
					     'bound)
				(soarpush (car vl1)
					  result)))))))





(defun no-unbound-predicates? (c) ; edited: 4-Feb-86 09:28 
       (all-bound? (get-pred-vars-in-condition c)))





(defun predicatep (x) ; Randy.Gobbel 26-Mar-86 21:55  ; John.Laird 
 ; 10-May-85 16:20  ; TRUE IS X IS A PREDICATE SYMBOL 
       (and (symbolp x)
	    (get x 'predicate)))





(defun put-rank (c n) ; Dan.Scales 23-Mar-85 10:33 
       (rplaca (cddddr (car c))
	       n))





(defun
  re-order-conditions
  (cl) ; Randy.Gobbel 12-Sep-86 12:06 
  (prog (un-active-conditions new-conditions set-vars dup ccl best 
			      active-conditions negations temp 
			      already-no-active-conditions)
	(cond ((null cl)
	       (return nil)))
	(setq cl (sort-conditions cl))
	(setq *last-value-var* nil)
	(setq negations (cadr cl))
	(setq dup (caddr cl))
	(setq cl (car cl)) ; UN-ACTIVE-CONDITIONS IS A LIST OF ALL 
 ; CONDITIONS THAT HAVEN'T BEEN ORDERED YET 
	(setq un-active-conditions cl)
	(setq active-conditions nil)
	(go l4)
	l0
	(cond ((null un-active-conditions)
	       (cond (negations (soarwhile negations (soarpush (cadr negations)
							       new-conditions)
					   (soarpush
					     (strip-condition (car negations))
					     new-conditions)
					   (and *warning*
						(soarwarn 
		 "Unlinked condition or unnecessary variable in condition "
							  (p-to-sp (car 
							     new-conditions))))
					   (setq negations (cddr negations)))))
	       (unmark-as-set set-vars)
	       (setq *condition-vars* (append set-vars *condition-vars*))
	       (return (nreverse new-conditions)))
	      ((or already-no-active-conditions
		   (neq 'goal (get-condition-type (car un-active-conditions))))
	       (soarpush (strip-condition (car un-active-conditions))
			 new-conditions)
	       (setq un-active-conditions (cdr un-active-conditions))
	       (setq already-no-active-conditions nil)
	       (cond ((neq (wme-class (car new-conditions))
			   'preference)
		      (and *warning* (soarwarn 
			     "Condition not linked to previous conditions "
					       (p-to-sp (car new-conditions)))))
		     )
	       (go l0)))
	l4
	(setq already-no-active-conditions t)
	(setq temp (get-id-var (car un-active-conditions)))
	(cond ((eq temp 'conj)
	       (setq temp (car (get-vars-in-condition (car un-active-conditions)
						      )))))
	(setq set-vars (append (mark-as-set temp)
			       set-vars))
	l3
	(setq temp (get-conditions-with-vars set-vars un-active-conditions))
	(setq active-conditions (append (car temp)
					active-conditions))
	(setq un-active-conditions (cdr temp))
	(cond ((null active-conditions)
	       (go l0)))
	(setq ccl (classify dup active-conditions t))
	(setq best (find-best-condition dup active-conditions 
					un-active-conditions ccl 0 nil))
	(setq already-no-active-conditions nil)
	(cond ((null best)
	       (go l0)))
	(cond (*order-trace* (traceprint (cons (car best)
					       (strip-condition (cdr best))))))
	(setq best (cdr best))
	(setq active-conditions (dremove best active-conditions))
	(setq set-vars (append (mark-as-set (get-vars-in-condition best))
			       set-vars))
	(setq temp (get-attr-val-types best))
	(cond ((eq (cadr temp)
		   'var)
	       (setq *last-value-var* (cddr temp)))
	      (t (setq *last-value-var* nil)))
	(setq temp (strip-condition best)) ; INSERT <> UNDEC IN 
 ; APPROPRIATE PLACES IF THIS IS A GOAL-CONTEXT-INFO 
	(cond ((eq (get-condition-type best)
		   'goal)
	       (insert-ne-undec temp)))
	(soarpush temp new-conditions)
	(setq temp (check-negations negations))
	(setq new-conditions (append (car temp)
				     new-conditions))
	(setq negations (cdr temp))
	(go l3)))





(defun reorder-p-conds (prod) ; Randy.Gobbel 16-Jul-86 16:48 
       (prog (type)
	     (setq *p-name* (pop prod))
	     (setq *condition-vars* nil)
	     (setq type (cond ((soarmemq (car prod)
					 '(elaborate serial))
			       (pop prod))
			      (t 'elaborate)))
	     (setq prod (reorder-p-conds1 prod '-->))
	     (return (append (list 'p *p-name* type)
			     (cdr prod)
			     (list '-->)
			     (test-connected-actions (car prod))))))





(defun reorder-p-conds1 (condlist delim) ; Randy.Gobbel 20-Jun-86 13:47 
       (prog (tmp conds)
	     condloop
	     (cond ((or (null condlist)
			(eq (car condlist)
			    '-->)
			(eq (car condlist)
			    '}))
		    (go doneconds)))
	     (cond ((eq (car condlist)
			'{)
		    (setq tmp (reorder-p-conds1 (cdr condlist)
						'}))
		    (setq condlist (car tmp))
		    (soarpush (cdr tmp)
			      conds))
		   (t (soarpush (pop condlist)
				conds)))
	     (go condloop)
	     doneconds
	     (cond ((and (eq delim '})
			 (neq (car condlist)
			      '}))
		    (soarerror "Missing }" nil))
		   ((eq delim '-->)
		    (cond ((eq (car condlist)
			       '})
			   (soarerror "Extra }" nil))
			  ((neq (car condlist)
				'-->)
			   (soarerror "Missing -->" nil)))))
	     (pop condlist)
	     (cond ((null conds)
		    (cond ((eq delim '-->)
			   (soarerror "Missing conditions in rule LHS" nil))
			  (t (soarerror "Missing conditions between { and }" 
					nil)))))
	     (return (cons condlist (re-order-conditions (nreverse conds))))))





(defun
  sort-conditions
  (cl) ; Randy.Gobbel 11-Sep-86 15:06  ; SORT CONDITIONS IN CONDITION LIST 
 ; CL INTO PREFERENCES NEGATIONS AND REGULAR CONDITIONS 
  (prog
    (regulars c c1 foralls goal-contexts tmp varlist dup nl)
    (setq foralls nil)
    (soarwhile
      cl
      (setq c (pop cl))
      (cond ((is-negation c) ; C1 CONTAINS WHAT FOLLOWS - OR * 
	     (setq c1 (pop cl))
	     (cond ((listp (car c1))
		    (soarpush (cons (soarmapconc #'find-all-vars-in-condition 
						 c1)
				    c1)
			      foralls))
		   (t (soarpush (cons (find-all-vars-in-condition c1)
				      c1)
				foralls)))
	     (unmark-as-set (get-all-vars-in-neg (car foralls)))
	     (soarpush c foralls))
	    ((listp (car c))
	     (setq tmp (cons (list nil 'conj
				   (soarmapconc #'find-all-vars-in-condition c)
				   nil nil)
			     c))
	     (unmark-as-set (get-vars-in-condition tmp))
	     (cond ((eq (caar c)
			'goal)
		    (soarpush tmp goal-contexts))
		   (t (soarpush tmp regulars)))
	     (setq varlist (append (get-vars-in-condition tmp)
				   varlist)))
	    (t (setq tmp (cons (cons (find-pred-vars-in-condition c)
				     (find-info-on-cond
				       c
				       (cond ((eq (car c)
						  'preference)
					      'preference)
					     (t nil))))
			       c))
	       (unmark-as-set (get-pred-vars-in-condition tmp))
	       (unmark-as-set (get-vars-in-condition tmp))
	       (cond ((eq (car c)
			  'goal)
		      (soarpush tmp goal-contexts))
		     (t (soarpush tmp regulars)))
	       (setq varlist (append (get-pred-vars-in-condition tmp)
				     (append (get-vars-in-condition tmp)
					     varlist))))))
    (setq dup (get-duplicates varlist))
    (setq varlist (take-out-duplicates varlist))
    (setq nl foralls)
    (soarwhile nl (setq tmp (get-all-vars-in-neg (cadr nl)))
	       (rplaca (cadr nl)
		       (append (intrq (get-duplicates tmp)
				      varlist)
			       (get-non-duplicates tmp)))
	       (cond ((null (get-all-vars-in-neg (cadr nl)))
		      (soarwarn 
		     "Negation or forall unconnected to rest of production"
				(cdadr nl))))
	       (setq nl (cddr nl)))
    (return (list (append (nreverse goal-contexts)
			  (nreverse regulars))
		  (nreverse foralls)
		  dup))))





(defun strip-condition (x) ; Dan.Scales 23-Mar-85 11:22 
       (cond ((atom x)
	      x)
	     (t (cdr x))))





(defun take-out-duplicates (lst) ; Randy.Gobbel 11-Sep-86 15:06 
       (soar-do ((l lst (cdr l))
		 (result))
		((null l)
		 (return (nreverse result)))
		(cond ((not (soarmemq (car l)
				      (cdr l)))
		       (soarpush (car l)
				 result)))))





(defun unmark-as-set (vlist) ; Dan.Scales 23-Mar-85 10:33 
       (soar-do ((vl1 (cond ((atom vlist)
			     (list vlist))
			    (t vlist))
		      (cdr vl1)))
		((null vl1)
		 vlist)
		(remprop (car vl1)
			 'bound)))



;;; Concatenated from type module "interface" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/interface/release/macros.lisp".
;;; -*-mode: lisp; package: user -*-
;;; Macros for soar.

(defmacro call1 (&rest z)
	  (list 'nlam-call1 (list 'quote z)))

(defmacro call2 (&rest z)
	  (list 'nlam-call2 (list 'quote z)))

(defmacro d (&rest dc)
	  (list 'nlam-d (list 'quote dc)))

(defmacro init-wm (&rest z)
	  (list 'nlam-init-wm (list 'quote z)))

(defmacro label-bind (&rest z)
	  (list 'nlam-label-bind (list 'quote z)))

(defmacro pfired (&optional number)
	  (list 'nlam-pfired (list 'quote number)))

(defmacro pi (&rest z)
	  (list 'nlam-pi (list 'quote z)))

(defmacro po (&rest object)
	  (list 'nlam-po (list 'quote object)))

(defmacro pop-goal (&rest goals)
	  (list 'nlam-pop-goal (list 'quote goals)))

(defmacro ptrace (&rest arg)
	  (list 'nlam-ptrace (list 'quote arg)))

(defmacro run-task (&optional number)
	  (list 'nlam-run-task (list 'quote number)))

(defmacro smake (&rest x)
	  (list 'nlam-smake (list 'quote x)))

(defmacro smatches (&rest rule-list)
	  (list 'nlam-smatches (list 'quote rule-list)))

(defmacro spop (&rest object)
	  (list 'nlam-spop (list 'quote object)))

(defmacro spop2 (&rest object)
	  (list 'nlam-spop2 (list 'quote object)))

(defmacro sremove (&rest z)
	  (list 'nlam-sremove (list 'quote z)))

(defmacro tabstop (&rest z)
	  (list 'nlam-tabstop (list 'quote z)))

(defmacro unpbreak (&rest z)
	  (list 'nlam-unpbreak (list 'quote z)))

(defmacro write-reflect (&rest z)
	  (list 'nlam-write-reflect (list 'quote z)))

(defmacro write1 (&rest z)
	  (list 'nlam-write1 (list 'quote z)))

(defmacro write2 (&rest z)
	  (list 'nlam-write2 (list 'quote z)))

(defmacro sp (&rest xs)
	  (list 'nlam-sp (list 'quote xs)))

(defmacro spm (&rest z)
	  (list 'nlam-spm (list 'quote z)))

(defmacro spo (&rest object)
	  (list 'nlam-spo (list 'quote object)))

(defmacro spo2 (&rest object)
	  (list 'nlam-spo2 (list 'quote object)))

(defmacro sppwm (&rest avlist)
	  (list 'nlam-sppwm (list 'quote avlist)))

(defmacro spr (&rest arg)
	  (list 'nlam-spr (list 'quote arg)))

(defmacro swm (&rest a)
	  (list 'nlam-swm (list 'quote a)))

(defmacro soar-bind (&rest z)
	  (list 'nlam-bind (list 'quote z)))

(defmacro closefile1 (&rest z)
	  (list 'nlam-closefile1 (list 'quote z)))

(defmacro compute (&rest z)
	  (list 'nlam-compute (list 'quote z)))

(defmacro crlf (&rest z)
	  (list 'nlam-crlf (list 'quote z)))

(defmacro cs (&rest z)
	  (list 'nlam-cs (list 'quote z)))

(defmacro default (&rest z)
	  (list 'nlam-default (list 'quote z)))

(defmacro excise (&rest z)
	  (list 'nlam-excise (list 'quote z)))

(defmacro external (&rest z)
	  (list 'nlam-external (list 'quote z)))

(defmacro full-matches (&rest rule-list)
	  (list 'nlam-full-matches (list 'quote rule-list)))

(defmacro make (&rest z)
	  (list 'nlam-make (list 'quote z)))

(defmacro memories (&optional number)
	  (list 'nlam-memories (list 'quote number)))

(defmacro openfile1 (&rest z)
	  (list 'nlam-openfile1 (list 'quote z)))

(defmacro p (&rest z)
	  (list 'nlam-p (list 'quote z)))

(defmacro pbreak (&rest z)
	  (list 'nlam-pbreak (list 'quote z)))

(defmacro pm (&rest z)
	  (list 'nlam-pm (list 'quote z)))

(defmacro ppwm (&rest avlist)
	  (list 'nlam-ppwm (list 'quote avlist)))

(defmacro rjust (&rest z)
	  (list 'nlam-rjust (list 'quote z)))

(defmacro run (&rest z)
	  (list 'nlam-run (list 'quote z)))

(defmacro tabto (&rest z)
	  (list 'nlam-tabto (list 'quote z)))

(defmacro watch (&rest z)
	  (list 'nlam-watch (list 'quote z)))

(defmacro wm (&rest a)
	  (list 'nlam-wm (list 'quote a)))

(defmacro back-trace (&rest objects)
	  (list 'nlam-back-trace (list 'quote objects)))

(defmacro learn (&rest z)
	  (list 'nlam-learn (list 'quote z)))

(defmacro init-context (&optional goal-name problem-space state operator)
	  (list 'nlam-init-context (list 'quote goal-name)
		(list 'quote problem-space)
		(list 'quote state)
		(list 'quote operator)))


;;; Concatenated from type module "interface" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/interface/release/interface.lisp".
;;; -*-mode: lisp; package: user -*-   

(defun nlam-call1 (z) ; Randy.Gobbel 13-Jun-86 14:31 
       (prog (f)
	     (setq f (car z))
	     (eval-args (cdr z))
	     (soarapply f)))

(defun nlam-call2 (z) ; Randy.Gobbel 13-Jun-86 14:31 
       (cond ((not *in-rhs*)
	      (soarwarn "Cannot be called at top level " 'call2)
	      nil)
	     ((not (symbolp (car z)))
	      (soarwarn "Call2: illegal argument " (car z))
	      nil)
	     (t (eval (eval-args z)))))



(defun conv-input-wme (input) ; Randy.Gobbel 16-Jun-86 17:56 
       (cond ((numberp input)
	      (get-wm (list input)))
	     ((and (soarlistp input)
		   (neq (car input)
			'preference))
	      (soarmapconc #'sswm (sp-info-expand input nil)))
	     ((soarlistp input)
	      (sswm input))
	     (t (sswm (list input)))))






(defun nlam-d (dc) ; John.Laird 15-Aug-84 12:02 
       (cond ((atom dc)
	      (run))
	     (t (eval (list 'run (car dc)
			    'd)))))






(defun excise-chunks nil  ; John.Laird 17-Oct-85 09:47 
       (prog (chunks)
	     (setq chunks (append *chunks* nil))
	     (soarmapc #'(lambda (x)
				 (setq *tracep-list* (dremove x *tracep-list*)))
		       chunks)
	     (eval (cons 'excise chunks))))





(defun excise-task nil  ; Randy.Gobbel 19-Aug-86 14:41 
       (prog (pnames)
	     (setq pnames (append *user-pnames* nil))
	     (soarmapc #'(lambda (x)
				 (setq *tracep-list* (dremove x *tracep-list*)))
		       pnames)
	     (eval (cons 'excise pnames))
	     (setq *instance-attributes* nil)))




(defun nlam-label-bind (z) ; John.Laird 18-Jun-86 11:48 
       (prog (val new-id)
	     (cond ((not *in-rhs*)
		    (soarwarn "Cannot be called at top level"
			      'label-bind)
		    (return nil))
		   ((< (length z)
		      2)
		    (soarwarn "Label-Bind: Wrong number of arguments to" z)
		    (return nil))
		   ((not (variablep (car z)))
		    (soarwarn "Label-Bind: illegal argument" (car z))
		    (return nil))
		   (t (setq val (cadr (eval-args z)))))
	     (cond ((not (symbolp val))
		    (soarwarn "Label-Bind: illegal argument" val)
		    (return nil))
		   ((soarassq val *label-bindings*)
		    (make-var-bind (car z)
				   (cdr (soarassq val *label-bindings*))))
		   (t (setq new-id (soargensym 'x))
		      (soarpush (cons val new-id)
				*label-bindings*)
		      (make-var-bind (car z)
				     new-id)))))




(defun last-chunk nil  ; John.Laird 25-Mar-85 17:21  ; PRINT OUT TEXT OF 
 ; MOST RECENTLY CREATED CHUNK 
       (eval (cons 'spm (list (car *chunks*)))))




(defun list-chunks nil  ; John.Laird 25-Mar-85 15:24 
       (eval (cons 'spm (reverse *chunks*))))




(defun multi-attribute (x) ; Randy.Gobbel 13-Jun-86 14:33 
       (prog (class)
	     (create-new-class (car x))
	     (setq class (pop x))
	     (cond ((and (cdr x)
			 (numberp (cadr x))
			 (or (> (cadr x)
				99)
			     (< (cadr x)
			       1)))
		    (soarwarn "Illegal multi-attribute value" (cadr x))
		    (setq x (list (car x)))))
	     (soarpush (cons class x)
		       *multi-attribute*)))

(defun multi-attributes (x) ; John.Laird 25-Mar-85 15:47 
       (soarmapc #'multi-attribute x))






(defun nlam-pfired (number) ; Randy.Gobbel 12-Sep-86 12:06 
       (prog (fired-counts)
	     (setq fired-counts
		   (soarmapcar #'(lambda (x)
					 (cons (or (get x 'pfired)
						   0)
					       x))
			       *pnames*))
	     (or number (setq number (length *pnames*)))
	     (cond ((not (eqp number 0))
		    (setq fired-counts (reverse (soarcarsort fired-counts)))
		    (traceprint "Number of times each production fired:")
		    (soarwhile (< 0 number)
			       (traceprinc (caar fired-counts))
			       (traceprinc ": ")
			       (traceprint (cdar fired-counts))
			       (pop fired-counts)
			       (setq number (1- number))))
		   ((setq fired-counts (soarcarsort fired-counts))
		    (traceprint "Productions that never fired:")
		    (soarwhile (eqp 0 (caar fired-counts))
			       (traceprint (cdar fired-counts))
			       (pop fired-counts))))))




(defun pgs nil  ; Randy.Gobbel 17-Oct-86 12:41 
       (pgs-frame *context-stack* 1 nil)
       (traceprintc "Decision cycle ")
       (traceprinc *decide-count*)
       (traceterpri))




(defun
  pgs-frame
  (context-stack depth second-time) ; Randy.Gobbel  5-Nov-86 14:12 
  (prog (slots slot id context port)
	(setq port (trace-file))
	(setq context (pop context-stack))
	(cond ((null context)
	       (return nil))
	      (second-time (traceterpri)))
	(setq slots '(g p s o))
	l2
	(cond (slots (setq id (pop context))
		     (setq slot (pop slots))
		     (cond ((neq id 'undecided)
			    (traceterpri)
			    (cond (*subgoal-tabs* (do-tabto (plus 2
								  (times depth 
									 3))
							    port))
				  (t (do-tabto 5 port)
				     (traceprinc3 "(" (1- depth)
						  ")")
				     (do-tabto 11 port)))
			    (traceprinc2 slot ": ")
			    (print-id id)))
		     (go l2)))
	(setq second-time nil)
	(soarmapc #'(lambda (x)
			    (pgs-frame x (1+ depth)
				       second-time)
			    (setq second-time t))
		  context-stack)))




(defun nlam-pi (z) ; Randy.Gobbel  9-May-86 14:37 
       (cond ((null z)
	      (setq z *last-pname*))
	     (t (setq *last-arg* (setq *last-pname* (list (car z))))))
       (print-partial-instantiation (car z)
				    (cond ((cdr z)
					   (cadr z))
					  (t 1))))




(defun nlam-po (object) ; Randy.Gobbel 16-Jul-86 16:31 
       (cond ((null object)
	      (setq object *last-obj-id*))
	     (t (setq *last-arg* (setq *last-obj-id* (list (car object))))))
       (print-object (car object)))




(defun
  nlam-pop-goal
  (goals) ; John.Laird 22-May-86 11:15 
  (cond (*input-wme* (soarmapc #'remove-from-wm
			       (get (get (wme-id *input-wme*)
					 'in-id-field)
				    'non-results))
		     (soarclearprops (wme-id *input-wme*))))
  (cond
    ((null goals)
     (soarmapc #'(lambda (x)
			 (pop-subgoals x (most-recent-subgoal *context-stack*)
				       *context-stack*
				       (car *context-stack*)))
	       (cdr *context-stack*)))
    (t (soarmapc #'(lambda (y)
			   (soarmapc #'(lambda (x)
					       (pop-subgoals x y 
							    *context-stack*
							     (car 
							    *context-stack*)))
				     (cdr *context-stack*)))
		 goals)))
  nil)




(defun
  pop-subgoals
  (stack g prior-stack prior-context) ; John.Laird 25-Mar-85 16:02 
  (cond (stack (cond ((eq (caar stack)
			  g)
		      (remove-subgoal-objects stack)
		      (rplacd prior-stack nil)
		      (rplaca (soarnthcdr 4 prior-context)
			      0)
		      (rplaca (soarnthcdr 5 prior-context)
			      nil))
		     (t (soarmapc #'(lambda (x)
					    (pop-subgoals x g stack (car stack))
					    )
				  (cdr stack)))))))




(defun
  ppi3
  (p i nodes ce) ; Randy.Gobbel 12-Sep-86 12:17 
  (prog
    (saved-mems n smatch-flg)
    (cond
      ((or (eq (caar nodes)
	       '&not)
	   (eq (caar nodes)
	       '&all))
       (return (ppi3 p i (cddr nodes)
		     (1+ ce))))
      ((eq (caar nodes)
	   '&p)
       (setq n 0)
       (setq smatch-flg nil)
       (soarmapc
	 #'(lambda (ci)
		   (cond ((eq (car ci)
			      p)
			  (setq smatch-flg ce)
			  (setq n (+ n 1))
			  (cond ((equal n i)
				 (soarmapc #'(lambda (wme)
						     (ppelm wme t)
						     (soarterpri t))
					   (reverse (cdr ci)))
				 (traceterpri))))))
	 *conflict-set*)
       (traceterpri)
       (return smatch-flg))
      ((null (find-left-mem (car nodes)))
       (return nil))
      ((not (setq smatch-flg (ppi3 p i (cddr nodes)
				   (1+ ce))))
       (cond ((null (setq saved-mems (soarnth (1- i)
					      saved-mems)))
	      (return ce))
	     (t (soarmapc #'(lambda (wme)
				    (ppelm wme t)
				    (soarterpri))
			  (reverse saved-mems))
		(traceterpri)
		(return ce))))
      (t (return smatch-flg)))))




(defun pplinet (line) ; Randy.Gobbel 12-Sep-86 12:08 
       (ppline line)
       (traceterpri))




(defun print-choice (choice) ; Randy.Gobbel 12-Sep-86 12:06 
       (print-id choice)
       (traceprint " "))




(defun print-id (id) ; Randy.Gobbel 12-Sep-86 12:05 
       (traceprinc2 id " ")
       (cond ((and id (symbolp id)
		   (neq id 'undecided))
	      (and (get id 'name)
		   (traceprinc (get id 'name)))
	      (setq *print-attribute-list* (list id))
	      (print-instance id)
	      (setq *print-attribute-list* nil))))




(defun
  print-instance
  (id) ; Randy.Gobbel 17-Oct-86 12:34 
  (prog
    (flag flag2)
    (cond
      ((and (symbolp id)
	    (get id 'instance))
       (setq flag nil)
       (setq flag2 nil)
       (soarmapc
	 #'(lambda (object)
		   (cond ((and (symbolp object)
			       (get object 'name))
			  (cond ((neq '! (get object 'name))
				 (cond ((or flag flag2)
					(traceprinc " "))
				       (t (traceprinc " (")
					  (setq flag t)))
				 (traceprinc (get object 'name))
				 (cond ((not (soarmemq object 
						     *print-attribute-list*))
					(soarpush object *print-attribute-list*)
					(print-instance object))))))
			 (t (cond (*full-print-id*
				    (cond (flag (traceprinc " "))
					  (t (traceprinc "(")
					     (setq flag t)))
				    (traceprinc object)))
			    (cond ((not (soarmemq object *print-attribute-list*)
					)
				   (soarpush object *print-attribute-list*)
				   (setq flag2 (print-instance object)))))))
	 (get id 'instance))
       (and flag (traceprinc ")"))
       (return flag)))))




(defun print-object (object) ; Randy.Gobbel 16-Jul-86 16:31 
       (eval (append '(ppwm *unbound*)
		     (list object))))




(defun print-partial-instantiation (p i) ; Randy.Gobbel 12-Sep-86 12:08 
       (traceterpri)
       (ppi3 p i (get p 'backpointers)
	     2))




(defun print-status nil  ; Randy.Gobbel 12-Sep-86 12:06 
       (traceprintc "Learn status: ")
       (cond (*learning* (traceprinc "on "))
	     (*never-learn* (traceprinc "never "))
	     (t (traceprinc "off ")))
       (cond (*always-learn* (traceprinc "all-goals "))
	     (t (traceprinc "bottom-up ")))
       (cond ((eqp *print-learn* 0)
	      (traceprinc "print "))
	     (*print-learn* (traceprinc "full-print "))
	     (t (traceprinc "noprint ")))
       (cond ((and *ltrace* *tracep*)
	      (traceprint "full-trace "))
	     (*tracep* (traceprint "trace "))
	     (t (traceprint "notrace ")))
       t)




(defun printline (x) ; Randy.Gobbel 12-Sep-86 12:08 
       (soarmapc #'traceprinc x)
       (traceterpri))




(defun
  nlam-ptrace
  (arg) ; John.Laird 19-Sep-85 17:20 
  (cond
    ((atom arg)
     *tracep-list*)
    (t (setq *tracep-list*
	     (append *tracep-list*
		     (soarmapcar
		       #'(lambda (x)
				 (cond ((or (numberp x)
					    (soarlistp x))
					(car (conv-input-wme x)))
				       (t x)))
		       arg))))))




(defun restart-soar nil  ; Randy.Gobbel 13-Jun-86 14:34 
       (prog (pnames)
	     (setq pnames (append *pnames* nil))
	     (eval (cons 'excise pnames))
	     (soarmapc #'soarclearprops (append *save-class-list* 
						*used-gensym-list* *user-ids*))
	     (i-g-v)))




(defun nlam-run-task (number) ; Randy.Gobbel 22-Aug-86 10:59 
       (init-soar)
       (init-task)
       (eval (cons 'd (list number))))




(defun set-learning-choice nil  ; Randy.Gobbel  6-Aug-86 12:05 
       (eval (soar-menu "Learning choice"
			'(("Learning at all levels" (learn on all-goals))
			 ("Learning bottom-up" (learn on bottom-up))
			 ("No learning" (learn never))))))





(defun set-user-select nil  ; Randy.Gobbel  6-Aug-86 12:05 
       (and (not *select-equal*)
	    (user-select (soar-menu "User-select values"
				    '(("User control" t)
				     ("Deterministically Random" first)
				     ("Random" nil))))))





(defun nlam-smake (x) ; Randy.Gobbel 17-Jun-86 14:05 
       (eval (spm-to-make x)))





(defun nlam-smatches (rule-list) ; Randy.Gobbel 12-Sep-86 12:08 
       (cond ((null rule-list)
	      (setq rule-list *last-pname*))
	     (t (setq *last-arg* (setq *last-pname* (list (car rule-list))))))
       (traceterpri)
       (soarmapc #'(lambda (x)
			   (smatches2 x nil))
		 rule-list)
       (traceterpri))





(defun smatches2 (p full-print-flag) ; Randy.Gobbel 11-Sep-86 15:08 
       (cond ((symbolp p)
	      (setq *indent* 0)
	      (smatches3 p (get p 'production)
			 (get p 'backpointers)
			 t full-print-flag))))

(defun
  smatches3
  (name matrix nodes flag full-print-flag) ; Brian Milnes 9-Mar-87 20:00
  (prog
    (left-mem right-mem result-mem ce nomatch nresult cond)
    (cond (flag (traceprinc3 "(" name " ")
		(traceprint (get name 'type)))
	  (t (traceprint "(")))
    (setq ce 0)
    loop
    (cond ((null matrix)
	   (return result-mem)))
    (indent)
    (setq ce (1+ ce))
    (setq cond (pop matrix))
    (cond ((eq cond '-->)
	   (cond ((and full-print-flag (soarassq name *conflict-set*))
		  (traceprinc "** MATCHES FOR ALL CONDITIONS **")
		  (soarmapc #'(lambda (ci)
				      (cond ((eq (car ci)
						 name)
					     (swrite-elms (cdr ci)))))
			    *conflict-set*)
		  (traceterpri)))
	   (return))
	  ((is-negation cond)
	   (traceprinc2 cond " ")
	   (setq cond (pop matrix)))
	  (t (traceprinc "  ")))
    (cond ((soarlistp (car cond))
	   (setq *indent* (+ 6 *indent*))
	   (traceprinc "    ")
	   (setq right-mem (smatches3 name cond (pop nodes)
				      nil nil))
	   (setq *indent* (- *indent* 6))
	   (setq cond ")")
	   (traceprinc "  ")
	   (traceterpri))
	  (t (setq right-mem (pop nodes))))
    (cond (result-mem (setq left-mem result-mem)
		      (setq result-mem (pop nodes)))
	  (t (setq result-mem right-mem)))
    (cond
      (nomatch (traceprinc "    "))
      (t (cond ((eq (car result-mem)
		    '&p)
		(setq nresult 0)
		(soarmapc #'(lambda (ci)
				    (cond ((eq (car ci)
					       name)
					   (setq nresult (+ nresult 1)))))
			  *conflict-set*))
	       (t (setq nresult (cond ((eq (car result-mem)
					   '&rmem)
				       (length (find-right-mem result-mem)))
				      (t (mem-length (find-left-mem result-mem))
					 )))))
	 (cond ((zerop nresult)
		(traceprinc ">>>>"))
	       (t (traceprinc nresult)
		  (do-tabto (+ 7 *indent*)
			    (trace-file))))))
    (cond ((equal cond ")") ;; Yost Change from Patch2.
	   (pplinet cond))
	  (t (pplinet (car (p-conds-to-sp (list cond))))))
    (cond ((and (not nomatch)
		(zerop nresult)
		left-mem full-print-flag)
	   (traceprinc "** MATCHES FOR LEFT ** ")
	   (soarmapc #'swrite-elms (find-left-mem left-mem))
	   (traceterpri)
	   (traceprinc "** MATCHES FOR RIGHT ** ")
	   (soarmapc #'swrite-elms (find-right-mem right-mem))
	   (traceterpri)))
    (and (zerop nresult)
	 (setq nomatch t))
    (go loop)))

(defun soar-greeting nil  ; Randy.Gobbel 12-Sep-86 12:06 
       (traceprintc "Soar ")
       (cond (*public-version* (traceprinc "(Version ")
			       (traceprinc *version-number*)
			       (traceprinc ", Release ")
			       (traceprinc *release-number*)
			       (traceprint ")"))
	     (t (traceprinc *version-number*)
		(traceprinc ".")
		(traceprinc *minor-version*)
		(traceprint " (external release)")))
       (traceprinc "Created ")
       (traceprint *date-created*)
       (cond (*public-version* (traceprint 
		   "Bugs and questions should be sent to Soar@h.cs.cmu.edu")))
       (traceprint 
	"Copyright (c) 1986, 1985 Xerox Corporation.  All Rights Reserved.")
       (traceprint " ")
       (traceprint "Use of this software is permitted for non-commercial")
       (traceprint "research purposes, and it may be copied only for that use.")
       (traceprint "This software is made available AS IS, and Xerox")
       (traceprint "Corporation, Stanford University and Carnegie-Mellon")
       (traceprint "University make no warranty about the software or its")
       (traceprint "performance.")
       (traceprint " ")
       (traceprint "See (soarnews) for news.")
       nil)




(defun soarnews nil  ; Randy.Gobbel 12-Sep-86 12:06 
       (traceprintc "News for Soar 4.4")
       (traceprint " ")
       (traceprint "Init-context now takes four arguments:")
       (traceprint "A goal, a problem space, a state and an operator.")
       (traceprint 
     "It performs the same function as before, but with the supplied goal.")
       (traceprint " ")
       (traceprint 
	    "Init-wm is a new function.  It takes any number of arguments.")
       (traceprint "The arguments should be either a call to init-context or")
       (traceprint 
 "elements to be added added to working memory (in either P or SP format).")
       (traceprint 
"Variables can be included in the elements or in the call to init-context.")
       (traceprint " ")
       (traceprint " Soar now has two public arpanet mailing addresses:")
       (traceprint "  soar@h.cs.cmu.edu for requesting a copy of soar or general help.")
       (traceprint "  soar-bugs@h.cs.cmu.edu for bugreports.")
       nil)

(defun traceprinc (warning) ; Randy.Gobbel 12-Sep-86 12:08 
       (soarprinc warning (trace-file)))

(defun traceprinc2 (x1 x2) ; Randy.Gobbel 12-Sep-86 12:05 
       (traceprinc x1)
       (traceprinc x2))

(defun traceprinc3 (x1 x2 x3) ; Randy.Gobbel 12-Sep-86 12:05 
       (traceprinc x1)
       (traceprinc x2)
       (traceprinc x3))





(defun traceprint (warning) ; Randy.Gobbel 12-Sep-86 12:08 
       (prog2 nil (traceprinc warning)
	      (traceterpri)))





(defun traceprintc (warning) ; Randy.Gobbel 12-Sep-86 12:08 
       (traceterpri)
       (traceprinc warning))





(defun soarwarn (warning warning2) ; Randy.Gobbel 12-Sep-86 12:12 
       (prog nil (and (null *warning*)
		      (return))
	     (traceterpri)
	     (traceprinc "Warning: ")
	     (and *p-name* (traceprinc *p-name*))
	     (traceprinc "..")
	     (traceprinc warning)
	     (traceprinc " ")
	     (traceprinc warning2)
	     (return warning2)))





(defun nlam-spop (object) ; Randy.Gobbel 13-May-86 17:28 
       (cond ((null object)
	      (setq object *last-obj-id*))
	     ((not (numberp (car object)))
	      (setq *last-arg* (setq *last-obj-id* (list (car object))))))
       (eval (cons 'spop2 object)))





(defun nlam-spop2 (object) ; Randy.Gobbel 12-Sep-86 12:08 
       (setq *indent* 0)
       (setq *print-spo-list* nil)
       (cond ((or (null object)
		  (numberp (car object))))
	     ((not (numberp (soarlast object)))
	      (traceterpri)
	      (spo1 (car object)
		    *spo-default-depth* t)
	      (eval (cons 'spop2 (cdr object))))
	     (t (traceterpri)
		(spo1 (car object)
		      (soarlast object)
		      t)
		(eval (cons 'spop2 (cdr object)))))
       (setq *print-spo-list* nil))





(defun nlam-sremove (z) ; John.Laird 24-Jun-86 11:12 
       (cond ((null z)
	      (setq z *last-tag*))
	     (t (setq *last-arg* (setq *last-tag* (list (car z))))))
       (cond ((equal z '(*))
	      (process-changes nil (get-wm nil)))
	     (t (process-changes nil (get-wm z)))))





(defun sswm (avlist) ; Randy.Gobbel 16-Jun-86 17:57  ; SEARCH WORKING 
 ; MEMORY FOR MATCH TO AVLIST 
       (cond ((and (cdddr avlist)
		   (variablep (cadddr avlist))) ; REMOVE EXTRANEOUS ID 
 ; THAT SP PUTS IN WHEN NO ID IS SPECIFIED 
	      (rplacd avlist (cddddr avlist))))
       (cond ((not (conv-to-attr-nums avlist))
	      nil)
	     (t (mapwm #'sswm2)
		(prog2 nil *wm* (setq *wm* nil)))))





(defun sswm2 (elm-tag) ; John.Laird 11-Nov-85 10:15 
       (cond ((filter (car elm-tag))
	      (soarpush (car elm-tag)
			*wm*))))





(defun swrite-elms (wme-or-count) ; Randy.Gobbel 12-Sep-86 12:08 
       (cond ((soarlistp wme-or-count)
	      (traceterpri)
	      (soarmapc #'write-elms2 (reverse wme-or-count)))))





(defun nlam-tabstop (z) ; Randy.Gobbel 11-Sep-86 15:15 
       (prog (val)
	     (cond ((not *in-rhs*)
		    (soarwarn "TABSTOP can not be called at the top level" " ")
		    (return nil))
		   ((not (eqp (length z)
			      1))
		    (soarwarn "Wrong number of arguments for Tabstop:" z)
		    (return nil))
		   ((not (variablep (car z)))
		    (soarwarn "Illegal argument for Tabstop:" (car z))
		    (return nil))
		   ((eqp (length z)
			 1)
		    (setq val (+ 3 (times (get-context-depth *context*)
					  3)))))
	     (make-var-bind (car z)
			    val)))





(defun trace-attribute (x) ; John.Laird 24-Jun-86 12:48 
       (cond ((not (soarmember x *instance-attributes*))
	      (soarpush x *instance-attributes*))))





(defun trace-attributes (x) ; John.Laird 24-Jun-86 12:48 
       (soarmapc #'trace-attribute x)
       nil)





(defun trace-condition-action (wme flag) ; Randy.Gobbel 12-Sep-86 12:06 
       (cond ((null *tracep-list*))
	     ((or (soarmember wme *tracep-list*)
		  (soarmemq (wme-id wme)
			    *tracep-list*)
		  (soarmemq (get (wme-id wme)
				 'name)
			    *tracep-list*))
	      (traceprintc *p-name*)
	      (cond ((eq flag 'action)
		     (traceprinc ": -->"))
		    (t (traceprinc ": matches ")))
	      (ppline (car (p-conds-to-sp (list (clean-up-clause wme
								 'action))))))))





(defun trace-object (type id depth) ; Randy.Gobbel 12-Sep-86 12:06 
       (cond ((or (and (not *gtrace*)
		       (not *otrace*))
		  (not (trace-problem-space?))
		  (eq id 'undecided)))
	     ((and *otrace* id)
	      (traceprintc *decide-count*)
	      (cond ((and *subgoal-tabs* (not (eqp depth 0))
			  (eq type 'goal))
		     (do-tabto (plus 2 (times depth 3))
			       (trace-file))
		     (traceprinc '==>))
		    (*subgoal-tabs* (do-tabto (plus 5 (times depth 3))
					      (trace-file)))
		    (t (do-tabto 5 (trace-file))
		       (traceprinc3 "(" depth ")")
		       (do-tabto 11 (trace-file))))
	      (traceprinc2 (cond ((eq type 'goal)
				  'g)
				 ((eq type 'problem-space)
				  'p)
				 ((eq type 'state)
				  's)
				 ((eq type 'operator)
				  'o))
			   ": ")
	      (print-id id))))





(defun trace-problem-space? nil  ; John.Laird 21-Apr-86 13:43 
       (not (and *watch-free-problem-spaces*
		 (soarmemq (get (get-current *context* 'problem-space)
				'name)
			   *watch-free-problem-spaces*))))





(defun trace-production? nil  ; John.Laird 24-Jun-86 13:19 
       (or *atrace* (soarmemq *p-name* *user-pnames*)))





(defun nlam-unpbreak (z) ; John.Laird 19-Sep-85 18:05 
       (cond ((atom z)
	      (setq *brkpts* nil)
	      (setq *brknames* nil)
	      nil)
	     (t (soarmapc #'unpbreak2 z)
		nil)))


 


(defun unpbreak2 (rule) ; edited:  4-Feb-86 08:54 
       (cond ((or (not (symbolp rule))
		  (soarlistp rule))
	      (soarwarn "Illegal argument:" rule))
	     ((soarmemq rule *brkpts*)
	      (setq *brkpts* (rematm rule *brkpts*))
	      rule)
	     ((soarmemq rule *brknames*)
	      (setq *brknames* (rematm rule *brknames*))
	      rule)
	     (t nil)))





(defun unptrace nil  ; John.Laird 19-Sep-85 18:17 
       (setq *tracep-list* nil))





(defun untrace-attribute (x) ; John.Laird 24-Jun-86 12:48 
       (setq *instance-attributes* (dremove x *instance-attributes*)))





(defun untrace-attributes (x) ; John.Laird 24-Jun-86 12:48 
       (soarmapc #'untrace-attribute x)
       nil)





(defun nlam-write-reflect (z) ; Randy.Gobbel 16-Jul-86 16:40 
       (add-input z)
       (eval (list 'write1 z)))





(defun write-trace (arg1 arg2 arg3) ; Randy.Gobbel 12-Sep-86 12:06 
       (cond ((and *ptrace* (trace-problem-space?)
		   (trace-production?))
	      (traceprintc *decide-count*)
	      (traceprinc3 '":" *cycle-count* arg1)
	      (traceprinc3 arg2 '" " arg3)
	      (traceprinc '" "))))





(defun nlam-write1 (z) ; Randy.Gobbel 12-Sep-86 12:17 
       (prog (parms port x needspace)
	     (cond ((not *in-rhs*)
		    (soarwarn "Write1 cannot be called at the top level." " ")
		    (return nil)))
	     (setq parms (eval-args z))
	     (cond ((< (length parms)
		      1)
		    (soarwarn "Write1: nothing to print:" z)
		    (return nil)))
	     (setq port (default-write-file))
	     (setq x (car parms))
	     (cond ((and (symbolp x)
			 ($ofile x))
		    (setq port ($ofile x))
		    (pop parms)))
	     (setq needspace t)
	     (soarwhile parms (setq x (pop parms))
			(cond ((eq x 'crlf)
			       (setq needspace nil)
			       (soarterpri port))
			      ((eq x 'rjust)
			       (do-rjust (pop parms)
					 (pop parms)
					 port))
			      ((eq x 'tabto)
			       (setq needspace nil)
			       (do-tabto (pop parms)
					 port))
			      (t (and needspace (soarprinc " " port))
				 (setq needspace t)
				 (soarprinc x port))))))





(defun nlam-write2 (z) ; Randy.Gobbel 12-Sep-86 12:17 
       (prog (parms port x)
	     (cond ((not *in-rhs*)
		    (soarwarn "Write2 cannot be called at the top level" " ")
		    (return nil)))
	     (setq parms (eval-args z))
	     (cond ((< (length parms)
		      1)
		    (soarwarn "Write2: nothing to print" " ")
		    (return nil)))
	     (setq port (default-write-file))
	     (setq x (car parms))
	     (cond ((and (symbolp x)
			 ($ofile x))
		    (setq port ($ofile x))
		    (pop parms)))
	     (soarwhile parms (setq x (pop parms))
			(cond ((eq x 'crlf)
			       (soarterpri port))
			      ((eq x 'rjust)
			       (do-rjust (pop parms)
					 (pop parms)
					 port))
			      ((eq x 'tabto)
			       (do-tabto (pop parms)
					 port))
			      (t (soarprinc x port))))))





(defun $ifile (x)
       (cond ((symbolp x)
	      (get x 'inputfile))
	     (t nil)))





(defun $ofile (x)
       (cond ((symbolp x)
	      (get x 'outputfile))
	     (t nil)))




(defun accum-stats nil  ; Randy.Gobbel 16-May-86 10:51 
       (setq *prod-count* (+ *prod-count* 1))
       (setq *total-token* (+ *total-token* *current-token*))
       (cond ((> *current-token* *max-token*)
	      (setq *max-token* *current-token*)))
       (setq *total-wm* (+ *total-wm* *current-wm*))
       (cond ((> *current-wm* *max-wm*)
	      (setq *max-wm* *current-wm*))))

(defun add-input (token) ; John Laird April 20 th
       (cond (*input-wme* (soarmapc #'remove-from-wm
				    (get (wme-id *input-wme*)
				 'non-results))
			  (remprop (wme-id *input-wme*) 'non-results)
			  (remprop (wme-id *input-wme*) 'results)
			  (remprop (wme-id *input-wme*) 'goal-depth)))
       (setq *input-wme* (list 'input (soargensym 'i)
			       'input token))
       (setq *current-goal* (wme-id *input-wme*))
       (setq *data-matched* nil)
       (soarputprop *current-goal* 1000 'goal-depth)
       (add-to-wm *input-wme*))

(defun nlam-bind (z) ; Randy.Gobbel 13-Jun-86 14:40 
       (prog (val var)
	     (setq var (car z))
	     (cond ((not *in-rhs*)
		    (soarwarn "Cannot be called at top level" 'soar-bind)
		    (return nil))
		   ((< (length z)
		      1)
		    (soarwarn "Soar-bind: Wrong number of arguments to" z)
		    (return nil))
		   ((not (symbolp var))
		    (soarwarn "Soar-bind: illegal argument" var)
		    (return nil))
		   ((eqp (length z)
			 1)
		    (setq val (soargensym 's)))
		   (t (setq val (car (eval-args (cdr z))))))
	     (make-var-bind var val)))






(defun broken (rule) ; edited: 23-Mar-86 09:21 
       (soarmemq rule *brkpts*))





(defun nlam-closefile1 (z) ; Randy.Gobbel 13-Jun-86 14:40 
       (soarmapc #'closefile2 (eval-args z)))





(defun closefile2 (file) ; Randy.Gobbel 27-Jun-86 14:22 
       (prog (port)
	     (cond ((not (symbolp file))
		    (soarwarn "Closefile: illegal file identifier" file))
		   ((setq port ($ifile file))
		    (soarclose port)
		    (remprop file 'inputfile))
		   ((setq port ($ofile file))
		    (soarclose port)
		    (remprop file 'outputfile)))
	     (return nil)))





(defun nlam-compute (z) ; Randy.Gobbel 25-Jun-86 14:27 
       (list (ari z)))






(defun conflict-set nil  ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (cnts cs p z phase-type)
	     (setq cnts nil)
	     (setq cs *conflict-set*)
	     (setq phase-type 'decide)
	     l1
	     (and (atom cs)
		  (go l2))
	     (setq p (caar cs))
	     (and (neq (get p 'type)
		       'decide)
		  (setq phase-type 'elaborate))
	     (setq cs (cdr cs))
	     (setq z (soarassq p cnts))
	     (cond ((null z)
		    (setq cnts (cons (cons p 1)
				     cnts)))
		   (t (rplacd z (1+ (cdr z)))))
	     (go l1)
	     l2
	     (cond ((atom cnts)
		    (traceterpri)
		    (return (list 'phase phase-type))))
	     (cond ((and (eq phase-type 'elaborate)
			 (eq (get (caar cnts)
				  'type)
			     'decide))
		    (setq cnts (cdr cnts))
		    (go l2)))
	     (traceterpri)
	     (traceprinc3 (caar cnts)
			  " "
			  (get (caar cnts)
			       'type))
	     (cond ((> (cdar cnts)
		       1)
		    (traceprinc3 "	(" (cdar cnts)
				 " OCCURRENCES)")))
	     (setq cnts (cdr cnts))
	     (go l2)))





(defun nlam-crlf (z) ; Randy.Gobbel 17-Jun-86 15:31 
       (cond (z (soarwarn "CRLF: Does not take arguments" z))
	     (t '(crlf))))





(defun nlam-cs (z) ; edited: 16-Feb-84 13:48 
       (cond ((atom z)
	      (conflict-set))
	     (t 'what?)))





(defun nlam-default (z) ; Randy.Gobbel 11-Sep-86 15:24 
       (prog (args file use)
	     (setq args (eval-args z))
	     (cond ((not (eqp (length args)
			      2))
		    (soarwarn "Default: wrong number of arguments" z)
		    (return nil)))
	     (setq file (car args))
	     (setq use (cadr args))
	     (cond ((not (symbolp file))
		    (soarwarn "Default: illegal file identifier" file)
		    (return nil))
		   ((not (soarmemq use '(write accept trace)))
		    (soarwarn "Default: illegal use for a file" use)
		    (return nil))
		   ((and (soarmemq use '(write trace))
			 (not (null file))
			 (not ($ofile file)))
		    (soarwarn "Default: file has not been opened for output" 
			      file)
		    (return nil))
		   ((and (eq use 'accept)
			 (not (null file))
			 (not ($ifile file)))
		    (soarwarn "Default: file has not been opened for input" 
			      file)
		    (return nil))
		   ((eq use 'write)
		    (setq *write-file* file))
		   ((eq use 'accept)
		    (setq *accept-file* file))
		   ((eq use 'trace)
		    (setq *trace-file* file)))
	     (return nil)))





 (defun default-write-file nil  ; Randy.Gobbel  3-Apr-86 14:19 
       (prog (port)
	     (setq port *standard-output*)
	     (cond (*write-file* (setq port ($ofile *write-file*))
				 (cond ((null port)
					(soarwarn "WRITE: file has been closed" 
						  *write-file*)
					(setq port *standard-output*)))))
	     (return port)))

(defun do-continue (wmi) ; Randy.Gobbel 12-Sep-86 12:06 
       (cond (*critical* (traceprintc "WARNING: Network may be inconsistent")))
       (process-changes wmi nil)
       (main))

(defun do-rjust (width value port) ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (size)
	     (cond ((equal value "=== T A B T O ===")
		    (soarwarn "Rjust cannot precede this function"
			      'tabto)
		    (return nil))
		   ((equal value "=== C R L F ===")
		    (soarwarn "Rjust cannot precede this function"
			      'crlf)
		    (return nil))
		   ((equal value "=== R J U S T ===")
		    (soarwarn "Rjust cannot precede this function"
			      'rjust)
		    (return nil)))
	     (setq size (flatc value))
	     (cond ((> size width)
		    (soarprinc " " port)
		    (soarprinc value port)
		    (return nil)))
	     (soar-do ((k (- width size)
			  (1- k)))
		      ((not (> k 0)))
		      (soarprinc ")" port))
	     (soarprinc value port)))





(defun do-tabto (col port) ; Randy.Gobbel 12-Sep-86 12:17 
       (prog (pos)
	     (setq pos (1+ (nwritn port)))
	     (cond ((> pos col)
		    (soarterpri port)
		    (setq pos 1)))
	     (soar-do ((k (- col pos)
			  (1- k)))
		      ((not (> k 0)))
		      (soarprinc " " port))
	     (return nil)))





(defun nlam-excise (z) ; Randy.Gobbel  9-May-86 14:37 
       (cond ((null z)
	      (setq z *last-pname*))
	     (t (setq *last-arg* (setq *last-pname* (list (car z))))))
       (soarmapc #'excise-p z))





(defun excise-p (name) ; Randy.Gobbel 12-Sep-86 12:05 
       (prog nil (cond ((and (symbolp name)
			     (get name 'topnode))
			(traceprinc "#")
			(setq *pcount* (1- *pcount*))
			(remove-from-conflict-set name)
			(kill-node (get name 'topnode))
			(remprop name 'production)
			(remprop name 'backpointers)
			(remprop name 'topnode)
			(remprop name 'type)
			(remprop name 'negation-index)
			(remprop name 'autonomic)
			(setq *chunks* (dremove name *chunks*))
			(setq *pnames* (dremove name *pnames*))
			(setq *user-pnames* (dremove name *user-pnames*))
			(and (get name 'gensymed)
			     (setq *free-pname-list*
				   (soarsort (cons name *free-pname-list*))))
			(return t))
		       (t (return nil)))))





(defun nlam-external (z) ; RG: 20-Feb-86 17:57 
       (soarcatch '!error! (external2 z)))





(defun external2 (z) ; RG: 20-Feb-86 18:14 
       (soarmapc #'external3 z))





(defun external3 (x) ; Randy.Gobbel  3-Apr-86 14:29 
       (cond ((symbolp x)
	      (soarputprop x t 'external-routine))
	     (t (soarerror "Not a legal function name" x))))





(defun externalp (x) ; Randy.Gobbel  3-Apr-86 13:54 
       (cond ((symbolp x)
	      (get x 'external-routine))
	     (t (soarwarn "Not a legal function name" x)
		nil)))





(defun nlam-full-matches (rule-list) ; Randy.Gobbel 12-Sep-86 12:08 
       (cond ((null rule-list)
	      (setq rule-list *last-pname*))
	     (t (setq *last-arg* (setq *last-pname* (list (car rule-list))))))
       (traceterpri)
       (soarmapc #'(lambda (x)
			   (smatches2 x t))
		 rule-list)
       (traceterpri))





(defun halt nil  ; Randy.Gobbel  3-Apr-86 14:09 
       (cond ((not *in-rhs*)
	      (soarwarn "Cannot be called at top level" 'halt))
	     (t (setq *halt-flag* t))))






(defun ident (x y) ; Randy.Gobbel 11-Sep-86 15:26 
       (cond ((eq x y)
	      t)
	     ((not (numberp x))
	      nil)
	     ((not (numberp y))
	      nil)
	     ((eqp x y)
	      t)
	     (t nil)))





(defun nlam-make (z) ; Randy.Gobbel 11-Jun-86 15:57 
       (add-to-wm (eval-args z)))





(defun nlam-memories (number) ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (pnames memory-sizes pname)
	     (traceterpri)
	     (setq pnames *pnames*)
	     (soarwhile pnames (setq pname (pop pnames))
			(soarpush (memories2 pname (get pname
							'production)
					     (get pname 'backpointers))
				  memory-sizes))
	     (setq memory-sizes (reverse (soarcarsort memory-sizes)))
	     (traceprint "Productions with largest memories")
	     (or number (setq number 10))
	     (soarwhile (< 0 number)
			(traceprinc (caar memory-sizes))
			(traceprinc ": ")
			(traceprint (cadar memory-sizes))
			(pop memory-sizes)
			(setq number (1- number)))))




(defun
  memories2
  (pname matrix nodes) ; Randy.Gobbel 19-Jun-86 17:54 
  (prog
    (right-mem result-mem cond memory-count)
    (setq memory-count 0)
    (soarwhile
      (and matrix (neq (car matrix)
		       '-->))
      (pop matrix)
      (cond ((is-negation cond)
	     (setq cond (pop matrix))))
      (cond ((soarlistp (car cond))
	     (setq memory-count (+ memory-count (car (memories2 pname cond
								(pop nodes)))))
	     (setq cond ")"))
	    (t (setq right-mem (pop nodes))))
      (cond (result-mem (setq result-mem (pop nodes)))
	    (t (setq result-mem right-mem)))
      (cond ((neq (car result-mem)
		  '&p)
	     (setq memory-count
		   (+ memory-count
		      (cond ((eq (car result-mem)
				 '&rmem)
			     (length (find-right-mem result-mem)))
			    (t (mem-length (find-left-mem result-mem)))))))))
    (return (list memory-count pname))))





(defun nlam-openfile1 (z) ; Randy.Gobbel 11-Jun-86 15:56 
       (openfile2 (eval-args z)))





(defun openfile2 (arglist) ; Randy.Gobbel  2-May-86 16:48 
       (prog (filevar filename iomode)
	     (setq filevar (car arglist))
	     (setq filename (cadr arglist))
	     (setq iomode (selectq (caddr arglist)
				   (in 'input)
				   (out 'output)
				   (soarwarn "Openfile: illegal I/O mode" 
					     iomode)))
	     (cond ((not (symbolp filevar))
		    (soarwarn "Openfile: illegal file identifier" filevar))
		   (t (soarputprop filevar (soaropen filename iomode)
				   (soarpack iomode 'file))))
	     (return nil)))





(defun nlam-pbreak (z) ; Randy.Gobbel 12-Sep-86 12:08 
       (cond ((atom z)
	      (traceterpri)
	      (traceprinc2 "Production breaks:" *brkpts*)
	      (traceterpri)
	      (traceprinc2 "Name/identifier breaks:" *brknames*)
	      (traceterpri)
	      nil)
	     (t (soarmapc #'pbreak2 z)
		z)))





(defun pbreak2 (rule) ; edited:  4-Feb-86 09:06 
       (cond ((or (not (symbolp rule))
		  (soarlistp rule))
	      (soarwarn "Illegal argument:" rule))
	     ((or (soarmemq rule *brkpts*)
		  (soarmemq rule *brknames*))
	      nil)
	     ((get rule 'topnode)
	      (soarpush rule *brkpts*)
	      rule)
	     (t (soarpush rule *brknames*)
		rule)))





(defun nlam-pm (z) ; Randy.Gobbel  9-May-86 14:38 
       (cond ((null z)
	      (setq z *last-pname*))
	     (t (setq *last-arg* (setq *last-pname* (list (car z))))))
       (soarmapc #'(lambda (x)
			   (pprule x nil))
		 z)
       nil)





(defun ppelm (elm port) ; Randy.Gobbel 12-Sep-86 12:08 
       (soarprinc (creation-time elm)
		  port)
       (soarprinc ":" port)
       (cond ((eq (car elm)
		  'preference)
	      (setq elm (expand-preference elm))))
       (soarprinc "(" port)
       (soarprinc (pop elm)
		  port)
       (soarprinc " " port)
       (soarprinc (pop elm)
		  port)
       (soarwhile elm (soarprinc " ^" port)
		  (soarprinc (pop elm)
			     port)
		  (soarprinc " " port)
		  (soarprinc (pop elm)
			     port))
       (soarprinc ")" port))





(defun ppline (line) ; Randy.Gobbel 12-Sep-86 12:05 
       (prog nil (cond ((atom line)
			(traceprinc line))
		       (t (traceprinc "(")
			  (setq *ppline* line)
			  (ppline2)
			  (traceprinc ")")))
	     (return nil)))





(defun ppline2 nil  ; Randy.Gobbel 12-Sep-86 12:05 
       (prog (needspace)
	     (setq needspace nil)
	     top
	     (and (atom *ppline*)
		  (return nil))
	     (and needspace (traceprinc " "))
	     (cond ((eq (car *ppline*)
			'^)
		    (ppattval))
		   (t (pponlyval)))
	     (setq needspace t)
	     (go top)))





(defun pponlyval nil  ; Randy.Gobbel 12-Sep-86 12:06 
       (prog (val needspace)
	     (setq val (getval))
	     (setq needspace nil)
	     (cond ((> (+ (nwritn t)
			  (flatc val))
		       76)
		    (setq needspace nil)
		    (traceprintc " ")
		    (indent)))
	     top
	     (and (atom val)
		  (return nil))
	     (and needspace (traceprinc " "))
	     (setq needspace t)
	     (traceprinc (car val))
	     (setq val (cdr val))
	     (go top)))





(defun
  pprule
  (name flag) ; Randy.Gobbel  2-Dec-86 16:17 
  (prog
    (matrix next in-actions leader symbol)
    (and (not (symbolp name))
	 (return nil))
    (setq in-actions nil)
    (setq matrix (get name 'production))
    (and (null matrix)
	 (return nil))
    (traceprintc "(")
    (cond ((eq flag 'sp)
	   (setq matrix (p-to-sp matrix))
	   (traceprinc 'sp))
	  (t (traceprinc 'p)))
    (traceprinc2 " " name)
    (and (neq 'decide (get name 'type))
	 (traceprinc2 " " (get name 'type)))
    (cond
      ((null flag)
       (setq
	 matrix
	 (soarmapconc
	   #'(lambda (item)
		     (cond ((atom item)
			    (list item))
			   (t (cond ((listp (car item))
				     (append (soar-copy '({))
					     (soar-copy item)
					     (soar-copy '(}))))
				    (t (list (soar-copy item)))))))
	   matrix))))
    l1
    (and (atom matrix)
	 (go fin))
    (traceterpri)
    (setq leader "    ")
    l2
    (setq next (pop matrix))
    l3
    (cond
      ((soarlistp next)
       (cond ((null flag)
	      (setq next (car (p-to-sp (list next))))
	      (cond ((and in-actions (not (soarmemq (car next)
						    *soar-actions*)))
		     (setq next (cons 'make next))))))
       (setq
	 next
	 (soarmapconc
	   #'(lambda (item)
		     (cond ((atom item)
			    (list item))
			   (t (cond ((soarmemq (car item)
					       '({ <<))
				     (soar-copy item))
				    (t (list item))))))
	   next))))
    (cond ((eq next '-->)
	   (setq in-actions t)
	   (setq leader "  "))
	  ((is-negation next)
	   (setq symbol next)
	   (setq leader (soarconcat "  " symbol " "))
	   (setq next (pop matrix))
	   (cond ((eq next '{)
		  (setq leader (soarconcat " " symbol " {"))
		  (go l2))
		 (t (go l3))))
	  ((eq next '{)
	   (setq leader "    { ")
	   (go l2)))
    (traceprinc leader)
    (ppline next)
    (go l1)
    fin
    (traceprint ")")))





(defun nlam-ppwm (avlist) ; Randy.Gobbel 12-Sep-86 12:08 
       (cond (avlist (setq avlist (car (sp-form-expand nil (list avlist)
						       nil nil)))))
       (conv-to-attr-nums avlist)
       (mapwm #'ppwm2)
       (traceterpri))





(defun ppwm2 (elm-tag) ; Randy.Gobbel 12-Sep-86 12:17 
       (cond ((filter (car elm-tag))
	      (soarterpri t)
	      (ppelm (car elm-tag)
		     t))))





(defun print-stats nil  ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (cc pc dc time elab tokens)
	     (setq *break-flag* nil)
	     (setq time (time-conversion *elapsed-time*))
	     (setq dc (plus (float *decide-count*)
			    1.0E-20))
	     (setq cc (plus (float *cycle-count*)
			    1.0E-20))
	     (setq pc (plus (float *prod-count*)
			    1.0E-20))
	     (traceterpri)
	     (traceprinc "Soar ")
	     (cond (*public-version* (traceprinc "(Version ")
				     (traceprinc *version-number*)
				     (traceprinc ", Release ")
				     (traceprinc *release-number*)
				     (traceprinc ")  "))
		   (t (traceprinc *version-number*)
		      (traceprinc ".")
		      (traceprinc *minor-version*)
		      (traceprinc " (external release: created ")
		      (traceprinc *date-created*)
		      (traceprint ")  ")))
	     (printline (list "Run statistics on " (soar-date)))
	     (printline (list *pcount* " productions " (list *real-cnt*
							     (single-slash)
							     *virtual-cnt* 
							     "nodes")))
	     (cond ((eqp *decide-count* 0)
		    (traceterpri)
		    (return)))
	     (printline (list time " seconds elapsed "
			      (cond ((not *never-learn*)
				     (list (time-conversion 
						       *elapsed-build-time*)
					   "seconds chunking overhead"))
				    (t " "))))
	     (cond ((eqp time 0)
		    (setq time 1.0E-8)))
	     (printline (list *decide-count* " decision cycles "
			      (list (times 1000 (quotient time
							  (float *decide-count*)
							  ))
				    "ms per cycle")))
	     (printline (list (setq elab (- *cycle-count* *decide-count*))
			      " elaboration cycles "
			      (list (times 1000 (quotient time (float elab)))
				    "ms per cycle")
			      "
    "
			      (list (difference (quotient cc dc)
						1)
				    "e cycles/d cycle")))
	     (printline (list *prod-count* " production firings "
			      (list (times 1000 (quotient time
							  (float *prod-count*)))
				    "ms per firing")
			      "
    "
			      (and (lessp .01 (difference cc *decide-count*))
				   (list (quotient (float *prod-count*)
						   elab)
					 "productions in parallel"))))
	     (printline (list (difference *action-count* *initial-actions*)
			      " RHS actions after initialization "
			      (list (times 1000
					   (quotient time
						     (float (difference 
							     *action-count* 
							  *initial-actions*))))
				    "ms per action")))
	     (printline (list (round (quotient (float *total-wm*)
					       pc))
			      " mean working memory size "
			      (list *max-wm* "maximum," *current-wm* "current"))
			)
	     (printline (list (round (quotient (float *total-token*)
					       pc))
			      " mean token memory size "
			      (list *max-token* "maximum," *current-token* 
				    "current")))
	     (printline (list *added-tokens* " total number of tokens added "))
	     (printline (list *removed-tokens* 
			      " total number of tokens removed "))
	     (printline (list (setq tokens (+ *added-tokens* *removed-tokens*))
			      " token changes "
			      (list (times 1000 (quotient time (float tokens)))
				    "ms per change")
			      "
    "
			      (list (quotient (float tokens)
					      *action-count*)
				    "changes/action")))))





(defun printlinec (x) ; RG: 20-Feb-86 18:15 
       (soarmapc #'printlinec* x))





(defun printlinec* (y) ; Randy.Gobbel 12-Sep-86 12:08 
       (soarprinc '" ")
       (soarprinc y))





(defun nlam-rjust (z) ; Randy.Gobbel 11-Sep-86 15:31 
       (prog (val)
	     (cond ((not (eqp (length z)
			      1))
		    (soarwarn "RJUST: Wrong number of arguments" z)
		    (return nil)))
	     (setq val ($varbind (car z)))
	     (cond ((or (not (numberp val))
			(< val 1)
			(> val 127))
		    (soarwarn "RJUST: Illegal value for field width" val)
		    (return nil)))
	     (return (list 'rjust val))))





(defun nlam-run (z) ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (result)
	     (cond ((eqp *pcount* 0)
		    (traceterpri)
		    (traceprint "Please load in some productions first.")
		    (return nil)))
	     (init-wm-context)
	     (setq *brkrun* nil)
	     (start-elapsed-time)
	     (setq *remaining-decide* 1000000)
	     (setq *remaining-cycles* 1000000)
	     (cond ((atom z))
		   ((and (atom (cdr z))
			 (numberp (car z))
			 (> (car z)
			    -1))
		    (setq *remaining-cycles* (car z)))
		   ((and (numberp (car z))
			 (eq (cadr z)
			     'd))
		    (setq *remaining-decide* (car z)))
		   ((soarlistp (car z))
		    (setq *brkrun* (conv-input-wme (car z)))
		    (and (null *brkrun*)
			 (return "What?")))
		   (t (setq *brkrun* (car z))))
	     (setq result (do-continue nil))
	     (stop-elapsed-time)
	     (return result)))






(defun nlam-tabto (z) ; Randy.Gobbel 11-Sep-86 15:32 
       (prog (val)
	     (cond ((not (eqp (length z)
      1))
		    (soarwarn "TABTO: Wrong number of arguments" z)
		    (return nil)))
	     (setq val ($varbind (car z)))
	     (cond ((or (not (numberp val))
			(< val 1)
			(> val 127))
		    (soarwarn "TABTO: Illegal column number" val)
		    (return nil)))
	     (return (list val 'tabto))))






(defun time-tag-print (data port) ; Randy.Gobbel 12-Sep-86 12:08 
       (cond ((not (null data))
	      (time-tag-print (cdr data)
			      port)
	      (soarprinc " " port)
	      (soarprinc (creation-time (car data))
			 port))))





(defun trace-file nil  ; Randy.Gobbel  1-Apr-86 16:37 
       *trace-file*)





(defun nlam-watch (z) ; John.Laird 26-Mar-85 16:57 
       (cond ((equal z '(-1))
	      (setq *wtrace* nil)
	      (setq *ptrace* nil)
	      (setq *otrace* nil)
	      -1)
	     ((equal z '(0))
	      (setq *wtrace* nil)
	      (setq *ptrace* nil)
	      (setq *otrace* t)
	      0)
	     ((equal z '(.5))
	      (setq *wtrace* nil)
	      (setq *ptrace* t)
	      (setq *ttrace* nil)
	      (setq *otrace* t)
	      .5)
	     ((equal z '(1))
	      (setq *wtrace* nil)
	      (setq *ptrace* t)
	      (setq *ttrace* t)
	      (setq *otrace* t)
	      1)
	     ((equal z '(1.5))
	      (setq *wtrace* t)
	      (setq *ttrace* nil)
	      (setq *ptrace* t)
	      (setq *otrace* t)
	      1.5)
	     ((equal z '(2))
	      (setq *wtrace* t)
	      (setq *ttrace* t)
	      (setq *ptrace* t)
	      (setq *otrace* t)
	      2)
	     ((equal z '(3))
	      (setq *otrace* t)
	      (setq *wtrace* t)
	      (setq *ptrace* t)
	      '(2 -- conflict set trace not supported))
	     ((and (atom z)
		   (null *otrace*))
	      -1)
	     ((and (atom z)
		   (null *ptrace*))
	      0)
	     ((and (atom z)
		   (null *wtrace*))
	      1)
	     ((atom z)
	      2)
	     (t 'what?)))





(defun nlam-wm (a) ; Randy.Gobbel 12-Sep-86 12:17 
       (cond ((null a)
	      (setq a *last-tag*))
	     (t (setq *last-arg* (setq *last-tag* (list (car a))))))
       (soarmapc #'(lambda (z)
			   (soarterpri t)
			   (ppelm z t))
		 (get-wm a))
       (soarterpri t)
       nil)





(defun write-elms2 (x) ; Randy.Gobbel 12-Sep-86 12:08 
       (soarprinc " ")
       (soarprinc (creation-time x)))




;;; Concatenated from type module "interface" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/interface/release/sp.lisp".
;;; -*-mode: lisp; package: user -*-

(defun
  build-sp-cond
  (avlist extralist) ; Randy.Gobbel 13-Jun-86 11:54 
  (prog (result)
	(soarmapc #'(lambda (x)
			    (setq result
				  (append (cond ((is-negation (car x))
						 (list (pop x)
						       '^
						       (car x)))
						(t (list '^ (car x))))
					  (append (nreverse (cdr x))
						  result))))
		  avlist)
	(setq result (append (nreverse extralist)
			     result))
	(return result)))





(defun check-goal-ctx-test (conds) ; Randy.Gobbel 13-Jun-86 14:59 
       (cond ((or (null conds)
		  (eq (car conds)
		      '-->))
	      t)
	     ((and (soarlistp (car conds))
		   (eq (caar conds)
		       'goal))
	      nil)
	     (t (check-goal-ctx-test (cdr conds)))))





(defun collect-preference (in-actions) ; Randy.Gobbel 23-Jun-86 16:49 
       (prog (pref-list attrib val)
	     (setq pref-list
		   (soar-copy (cond (in-actions '(preference nil nil nil nil nil nil 
							nil nil))
			       (t '(preference *unbound* *unbound* *unbound* 
					       *unbound* *unbound* *unbound* 
					       *unbound* *unbound*)))))
	     (soarwhile *sp-forms*
			(cond ((not (eq (car *sp-forms*)
					'^))
			       (soarerror "Didn't find a ^ when expected" 
					  *sp-forms*)))
			(pop *sp-forms*)
			(setq attrib (collect-value nil))
			(setq val (collect-value nil))
			(selectq attrib (object (setf-wme-id pref-list val))
				 (role (setf-wme-attribute pref-list val))
				 (value (setf-wme-value pref-list val))
				 (reference (setf-wme-reference pref-list val))
				 (goal (setf-wme-goal pref-list val))
				 (problem-space (setf-wme-problem-space 
								  pref-list val)
						)
				 (state (setf-wme-state pref-list val))
				 (operator (setf-wme-operator pref-list val))
				 (soarerror "Unrecognized preference field: " 
					    attrib)))
	     (return pref-list)))





(defun
  collect-value
  (in-id-field) ; Randy.Gobbel 13-Jun-86 14:36 
  (prog
    (compound-val end-atom)
    (setq compound-val (list (car *sp-forms*)))
    (setq end-atom (cond ((eq (car *sp-forms*)
			      '<<)
			  '>>)
			 ((eq (car *sp-forms*)
			      '{)
			  '})
			 (t nil)))
    (cond
      (end-atom
	(pop *sp-forms*)
	(soarwhile (and *sp-forms* (not (eq (car *sp-forms*)
					    end-atom)))
		   (cond (in-id-field
			   (cond ((and (not *pand-id*)
				       (not (predicatep (car *sp-forms*))))
				  (setq *pand-save-id* (car *sp-forms*)))
				 ((predicatep (car *sp-forms*))
				  (setq *pand-id* t))
				 (t (setq *pand-id* nil)))))
		   (soarpush (pop *sp-forms*)
			     compound-val)
		   (cond (*pand-save-id* (setq *pand-id* t))))
	(cond ((null *sp-forms*)
	       (soarerror "Didn't find terminator" end-atom)))
	(soarpush (pop *sp-forms*)
		  compound-val)
	(return (dreverse compound-val)))
      ((predicatep (car compound-val))
       (pop *sp-forms*)
       (return (append compound-val (list (collect-value in-id-field)))))
      (t (return (pop *sp-forms*))))))





(defun
  compose-p-conds
  (cl) ; Randy.Gobbel  2-Dec-86 16:36 
  (soar-do
    ((avlist)
     (c)
     (extravals)
     (cl1 cl (skip-whole-cond cl1))
     (av)
     (type (car (get-pos-cond cl)))
     (id-var (get-term (cdr (get-pos-cond cl)))))
    ((null cl1)
     (cons type (append (cond ((eq (car id-var)
				   '*unbound*)
			       nil)
			      (t id-var))
			(build-sp-cond avlist extravals))))
    (setq c (get-pos-cond cl1)) ; DROP ANY MAKES 
    (cond ((eq (car c)
	       'make)
	   (setq c (cdr c))))
    (soar-do
      ((c1 (cddr c)
	   (cdr c1))
       (attr)
       (val)
       (x))
      ((null c1)
       (cond
	 (attr  ; Check if same attribute has already been encountered 
	       (setq x (soarassoc attr avlist))
	       (cond (val (cond (x (rplacd x (append val (cdr x))))
				(t (setq av (cons attr val))
				   (cond ((is-negation (car cl1))
					  (soarpush (car cl1)
						    av)))
				   (soarpush av avlist))))
		     (t (setq attr (list attr))
			(cond ((is-negation (car cl1))
			       (soarpush (car cl1)
					 attr)))
			(soarpush attr avlist))))
	 (val (soarpush val extravals))))
      (setq attr (pop c1))
      (setq val (nreverse (get-term c1)))
      (pop-term c1))))





(defun conv-to-attr-nums (avlist) ; Randy.Gobbel 17-Jun-86 15:07 
       (prog (next a)
	     (setq *filters* nil)
	     (setq next 1)
	     (soarwhile avlist (setq a (pop avlist))
			(cond ((variablep a)
			       (soarwarn "(S)PPWM does not take variables:" a)
			       (return nil))
			      (t (cond ((neq a '*unbound*)
					(setq *filters*
					      (cons next (cons a *filters*)))))
				 (setq next (1+ next)))))
	     (return t)))





(defun get-ids (infolist oldids) ; Randy.Gobbel 13-Jun-86 14:37 
       (prog (newids pair)
	     (soarmapc #'(lambda (x)
				 (setq pair (cons (cadr x)
						  (car x)))
				 (cond ((and (or (not oldids)
						 (soarmember pair oldids))
					     (not (soarmember pair newids)))
					(soarpush pair newids))))
		       infolist)
	     (return newids)))





(defun get-pos-cond (cl) ; Randy.Gobbel 13-Jun-86 14:45  ; In a condition 
 ; list gets next condition skipping over a possible minus sign 
       (cond ((is-negation (car cl))
	      (cadr cl))
	     (t (car cl))))





(defun get-similar-conds (cond cl) ; Randy.Gobbel 13-Jun-86 14:45  ; 
 ; RETURNS LIST OF CONDITIONS SIMILAR TO COND IN CONDITION LIST CL AND A 
 ; SMASHED CL WITH THOSE CONDITIONS REMOVED 
       (prog (rest result)
	     (setq rest (cons nil cl))
	     (soar-do ((type (car cond))
		       (id-var (get-term (cdr cond)))
		       (negflag)
		       (cl1 rest)
		       (c))
		      ((or (null cl)
			   (eq (car cl)
			       '-->)))
		      (setq c (pop cl))
		      (cond ((is-negation c)
			     (setq negflag c)
			     (setq c (pop cl)))
			    (t (setq negflag nil)))
		      (cond ((eq (car c)
				 'make)
			     (setq c (cdr c))))
		      (cond ((and (equal id-var (get-term (cdr c)))
				  (eq type (car c)))
			     (cond (negflag (soarpush negflag result)
					    (rplacd cl1 (cddr cl1))))
			     (soarpush c result)
			     (rplacd cl1 (cddr cl1)))
			    (t (cond (negflag (setq cl1 (cdr cl1))))
			       (setq cl1 (cdr cl1)))))
	     (return (cons (nreverse result)
			   (cdr rest)))))





(defun get-term (c) ; Dan Scales 31-Jan-85 14:39 
       (list-head c (skip-term c)))





(defun indent nil  ; Randy.Gobbel 12-Sep-86 12:05 
       (soar-do ((i *indent* (- i 1)))
		((= i 0))
		(traceprinc " ")))





(defun list-head (whole tail) ; Dan Scales 31-Jan-85 14:59 
       (soar-do ((result))
		((null whole)
		 (nreverse result))
		(cond ((eq whole tail)
		       (return (nreverse result)))
		      (t (setq result (cons (car whole)
					    result))
			 (setq whole (cdr whole))))))





(defun
  make-a-constant
  (results global-negation in-actions) ; Randy.Gobbel  2-Dec-86 16:10 
  (prog (result id value wme)
	(setq result (pop results))
	(cond ((null result)
	       (return nil))
	      ((is-negation result)
	       (return (cons result (make-a-constant results t in-actions))))
	      ((and in-actions (eq (car result)
				   'make))
	       (setq wme (cdr result)))
	      (in-actions (return (cons result (make-a-constant results nil 
								in-actions))))
	      (t (setq wme result)))
	(return (append (cond ((and *constants* (wme-value wme)
				    (neq (wme-value wme)
					 '*unbound*)
				    (not (variablep (wme-value wme)))
				    (not (soarlistp (wme-value wme)))
				    (neq 'goal (wme-class wme))
				    (or (neq 'constant (wme-class wme))
					(neq 'value (wme-attribute wme))))
			       (setq id (soargenvar 'c))
			       (setq value (wme-value wme))
			       (setf-wme-value wme id)
			       (cond (in-actions (list result
						       (list 'make
							     'constant id
							     'value value)))
				     (global-negation
				       (list (list wme (list 'constant id
							     'value value))))
				     (t (list wme (list 'constant id
							'value value)))))
			      (t (list result)))
			(make-a-constant results nil in-actions)))))





(defun p-conds-to-sp (cl) ; Randy.Gobbel  2-Dec-86 16:30 
       (prog (c cl1 result temp type negflg)
	     (setq cl1 (soar-copy cl)) ; Make copy of condition list because 
 ; get-similar-conditions will smash it 
	     l1
	     (setq negflg (cond ((is-negation (car cl1))
				 (car cl1))
				(t nil)))
	     (setq c (get-pos-cond cl1))
	     (cond ((or (null c)
			(eq c '-->))
		    (return (nreverse result)))
		   ((eq (car c)
			'make)
		    (setq c (cdr c))))
	     (setq type (car c))
	     (cond ((listp type)
		    (cond (negflg (soarpush negflg result)))
		    (soarpush '{ result)
		    (setq result (append (nreverse (p-conds-to-sp c))
					 result))
		    (soarpush '} result)
		    (setq cl1 (skip-whole-cond cl1)))
		   ((soarmemq type *soar-actions*)
		    (soarpush (ppelm1 c)
			      result)
		    (setq cl1 (cdr cl1)))
		   ((eq type 'preference)
		    (cond (negflg (soarpush negflg result)))
		    (soarpush (ppelm1 c)
			      result)
		    (setq cl1 (skip-whole-cond cl1)))
		   (t (setq temp (get-similar-conds c cl1))
		      (setq cl1 (cdr temp))
		      (setq c (car temp))
		      (cond ((and (is-negation (car c))
				  (<= (length c)
				    2))
			     (soarpush (pop c)
				       result)))
		      (soarpush (compose-p-conds c)
				result)))
	     (go l1)))





(defun p-to-sp (rule) ; Randy.Gobbel 13-May-86 14:22 
       (prog
	 (result)
	 (cond ((not (soarlistp (car rule)))
		(setq rule (list rule))))
	 (setq result (p-conds-to-sp rule))
	 (cond ((soarmemq '--> rule)
		(setq result
		      (append result (cons
				'-->
				(p-conds-to-sp (cdr (soarmemq '--> rule))))))))
	 (return result)))

(defun pp-class-ids (ids) ; Randy.Gobbel 12-Sep-86 12:08 
       (soarmapc #'(lambda (x)
			   (soarmapc #'(lambda (y)
					       (ppline y)
					       (traceterpri))
				     (p-conds-to-sp (sppwm1 (list (cdr x)
								  (car x))))))
		 ids))





(defun ppelm1 (elm) ; Randy.Gobbel 12-Jun-86 16:51 
       (prog (result val atlist vlist)
	     (setq result nil)
	     (setq vlist elm)
	     (cond ((eq (car elm)
			'preference)
		    (soarpush (pop vlist)
			      result)
		    (setq atlist
			  '(^ role ^ value ^ reference ^ goal ^ problem-space ^ 
			      state ^ operator))
		    (cond ((and (setq val (pop vlist))
				(neq val '*unbound*))
			   (soarpush val result)))
		    (soarwhile vlist (setq val (pop vlist))
			       (cond ((or (null val)
					  (eq val '*unbound*))
				      (pop atlist)
				      (pop atlist))
				     (t (soarpush (pop atlist)
						  result)
					(soarpush (pop atlist)
						  result)
					(soarpush val result)))))
		   (t (prog nil loop (cond ((null vlist)
					    (go end)))
			    (setq val (pop vlist))
			    (cond ((neq val '*unbound*)
				   (soarpush val result)))
			    (cond ((null vlist)
				   (go end)))
			    (setq val (pop vlist))
			    (cond ((neq val '*unbound*)
				   (soarpush val result)))
			    (go loop))))
	     end
	     (return (nreverse result))))

(defun skip-term (c) ; Dan Scales 31-Jan-85 14:42 
       (car (classify-term c)))

(defun skip-whole-cond (cl) ; Randy.Gobbel 13-Jun-86 14:46  ; Dan Scales 
 ; 10-Feb-85 03:17 
       (cond ((is-negation (car cl))
	      (cddr cl))
	     (t (cdr cl))))

(defun nlam-sp (xs) ; Randy.Gobbel 18-Jun-86 16:31  ; CONVERT AN SP 
 ; PRODUCTION TO P FORMAT  ; REORDER-P-CONDS DOES ALL THE WORK 
       (clear-var-list)
       (setq *xs* xs)
       (soarcatch '!error! (eval (reorder-p-conds (sp-form-expand (car *xs*)
								  *xs* nil nil))
				 ))
       nil)

(defun sp-form-expand
  (pname forms in-actions follows-negation) ; Randy.Gobbel 16-Jul-86 16:42 
  (prog (nf x result)
	(cond ((not forms)
	       (return nil)))
	(setq x (pop forms))
	(cond ((and (soarlistp x)
		    (eq (car x)
			'make))
	       (pop x)))
	(setq nf
	      (cond ((eq x '-->)
		     (setq in-actions t)
		     (list x))
		    ((atom x)
		     (list x))
		    ((and (neq (car x)
			       'preference)
			  (not (and in-actions (soarmemq (car x)
							 *soar-actions*))))
		     (create-new-class (car x))
		     (setq result (sp-info-expand x in-actions))
		     (make-a-constant result follows-negation in-actions))
		    ((and in-actions (soarmemq (car x)
					       *soar-actions*))
		     (list x))
		    (in-actions
		      (cond ((and (eq (car x)
				      'preference)
				  (cdr x)
				  (neq (cadr x)
				       '^))
			     (sp-info-expand (append '(preference ^ object)
						     (cdr x))
					     in-actions))
			    (t (sp-info-expand x in-actions))))
		    ((and (eq (car x)
			      'preference)
			  (cdr x)
			  (neq (cadr x)
			       '^))
		     (sp-info-expand (append '(preference ^ object)
					     (cdr x))
				     in-actions))
		    (t (sp-info-expand x in-actions))))
	(cond ((and follows-negation (greaterp (length nf)
					       1))
	       (soarerror "Attempt to negate a compound object" pname)))
	(return (append nf (sp-form-expand pname forms in-actions
					   (is-negation x))))))





(defun
  sp-info-expand
  (form in-actions) ; Randy.Gobbel 16-Jul-86 16:42 
  (prog
    (newform type id clist temp-form negated)
    (setq type (pop form))
    (setq *sp-forms* form)
    (setq *pand-id* nil)
    (setq *pand-save-id* nil)
    (setq negated nil)
    (cond ((and (neq type 'preference)
		(or (and (equal (car form)
				'^)
			 (> (length form)
			    3))
		    (and (is-negation (car form))
			 (> (length form)
			    4)))) ; Multiple attributes for this object 
 ; must be connected together 
	   (setq id (soargenvar 'x))
	   (setq *sp-forms* form))
	  ((or (equal (car form)
		      '^)
	       (is-negation (car form)))
	   (setq id '*unbound*)
	   (setq *sp-forms* form))
	  (t (setq id (collect-value t))
	     (cond ((null id)
		    (setq id '*unbound*)))))
    loop
    (cond ((not *sp-forms*)
	   (cond (clist (return (reverse clist)))
		 (t (return (list (list type (cond (id id)
						   (t '*unbound*)))))))))
    (setq negated nil)
    (cond ((is-negation (car *sp-forms*))
	   (setq negated (pop *sp-forms*))))
    (cond ((not (eq (car *sp-forms*)
		    '^))
	   (soarerror "Didn't find a ^ when expected" *sp-forms*)))
    (cond
      ((eq type 'preference)
       (setq newform (collect-preference in-actions)))
      (t
	(pop *sp-forms*)
	(setq newform
	      (list type (cond (*pand-id* (prog1 id
						 (progn (setq id *pand-save-id*)
							(setq *pand-id* nil))))
			       (t id))
		    (collect-value nil)))
	(setq temp-form newform)
	(cond ((and *sp-forms* (not (eq (car *sp-forms*)
					'^))
		    (not (is-negation (car *sp-forms*)))
		    (or (not (soarlistp (car *sp-forms*)))
			(not (eq (caar *sp-forms*)
				 '^))))
	       (soar-do ((newform temp-form temp-form))
			((not (and *sp-forms* (not (eq (car *sp-forms*)
						       '^))
				   (not (is-negation (car *sp-forms*)))
				   (or (not (soarlistp (car *sp-forms*)))
				       (not (eq (caar *sp-forms*)
						'^)))))
			 (go loop))
			(cond ((predicatep (car *sp-forms*))
			       (setq newform (append
				       newform
				       (list (collect-value nil)))))
			      (t (setq newform
				       (append newform
					       (list (collect-value nil))))))
			(cond ((and *sp-forms* (soarlistp (car *sp-forms*)))
			       (setq newform (append newform (pop *sp-forms*))))
			      )
			(cond (in-actions (soarpush 'make newform)))
			(cond (negated (soarpush negated clist)))
			(soarpush newform clist)))
	      (t (cond ((and *sp-forms* (soarlistp (car *sp-forms*)))
			(setq newform (append newform (pop *sp-forms*)))))))))
    (cond (in-actions (soarpush 'make newform)))
    (cond (negated (soarpush negated clist)))
    (soarpush newform clist)
    (go loop)))





(defun nlam-spm (z) ; Randy.Gobbel  9-May-86 14:38 
       (cond ((null z)
	      (setq z *last-pname*))
	     (t (setq *last-arg* (setq *last-pname* (list (car z))))))
       (setq *indent* 0)
       (soarmapc #'(lambda (x)
			   (pprule x 'sp))
		 z)
       nil)





(defun spm-to-make (form) ; edited:  4-Feb-86 08:57 
       (soarmapc #'eval (sp-info-expand form t))
       nil)





(defun nlam-spo (object) ; Randy.Gobbel 13-May-86 17:26  ; Print out the 
 ; augmentations of a list of object identifiers 
       (cond ((null object)
	      (setq object *last-obj-id*))
	     ((not (numberp (car object)))
	      (setq *last-arg* (setq *last-obj-id* (list (car object))))))
       (eval (cons 'spo2 object)))





(defun spo1 (object depth prefflag) ; Randy.Gobbel 17-Oct-86 12:32 
       (prog (out out1)
	     (cond ((soarmemq object *print-spo-list*)
		    (return))
		   (t (soarpush object *print-spo-list*)))
	     (setq out (p-conds-to-sp (sppwm1 (list '*unbound* object))))
	     (soarmapc #'(lambda (x)
				 (cond ((eq prefflag (eq (car x)
							 'preference))
					(indent)
					(pplinet x)
					(soarpush x out1))))
		       out)
	     (cond ((<= depth 1)
		    (return)))
	     (setq out out1)
	     (setq *indent* (+ *indent* 3))
	     (soar-do ((out2 out (cdr out2)))
		      ((null out2))
		      (soar-do ((out1 (cdar out2)
				      (cdr out1)))
			       ((null out1))
			       (cond ((and (symbolp (cadr out1))
					   (get (cadr out1)
						'in-id-field))
				      (spo1 (cadr out1)
					    (- depth 1)
					    prefflag)))))
	     (setq *indent* (- *indent* 3))))





(defun nlam-spo2 (object) ; Randy.Gobbel 12-Sep-86 12:08  ; Print out the 
 ; augmentations of a list of object identifiers  ; Will indent print out 
 ; at each new depth 
       (setq *indent* 0) ; *print-spo-list* contains a list of all 
 ; identifiers printed so they are only printed once 
       (cond ((or (null object)
		  (numberp (car object))) ; If its a number then punt 
	      )
	     ((not (numberp (soarlast object))) ; If there is no number at 
 ; the end use the default depth and then recursively call spo2 
	      (traceterpri)
	      (spo1 (car object)
		    *spo-default-depth* nil)
	      (eval (cons 'spo2 (cdr object))))
	     (t (traceterpri) ; If there is a number at the end it is the 
 ; depth 
		(spo1 (car object)
		      (soarlast object)
		      nil)
		(eval (cons 'spo2 (cdr object)))))
       (setq *print-spo-list* nil))





(defun nlam-sppwm (avlist) ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (ids avinfos)
	     (traceterpri)
	     (setq ids nil)
	     (setq *indent* 0)
	     (cond ((null avlist)
		    (setq *filters* nil)
		    (soarmapc #'(lambda (y)
					(ppline y)
					(traceterpri))
			      (p-conds-to-sp (sppwm1 *wmpart-list*)))))
	     (soarcatch '!error! (setq avinfos (sp-form-expand nil
							       (list avlist)
							       nil nil)))
	     l1
	     (cond ((null avinfos)
		    (go l2))) ; Get the ids and class names of all wme 
 ; satisfying next avinfo and intersect with previous ids 
	     (setq ids (get-ids (sppwm1 (pop avinfos))
				ids))
	     (cond (ids (go l1)))
	     l2
	     (pp-class-ids ids)))





(defun sppwm1 (avlist) ; Randy.Gobbel 16-Jun-86 18:00 
       (cond ((not (conv-to-attr-nums avlist))
	      nil)
	     (t (mapwm #'sppwm2)
		(prog1 *wm* (setq *wm* nil)))))





(defun sppwm2 (elm-tag) ; Randy.Gobbel 16-Jun-86 16:41 
       (cond ((filter (car elm-tag))
	      (setq *wm* (cons (car elm-tag)
			       *wm*)))))









(defun nlam-spr (arg) ; Randy.Gobbel 12-Sep-86 12:06  ; call the 
 ; appropriate display function based on the type of argument 
       (cond ((null arg)
	      (cond (*last-arg* (setq arg *last-arg*))
		    (t (traceprint "Error: no saved argument"))))
	     (t (setq *last-arg* (list (car arg)))))
       (cond ((get (car arg)
		   'in-id-field) ; object identifier 
	      (eval (cons 'spo arg)))
	     ((soarmemq (car arg)
			*pnames*) ; production name 
	      (eval (cons 'spm arg)))
	     ((numberp (car arg)) ; working-memory element time-tag 
	      (eval (cons 'swm arg)))
	     (t  ; assume it is sp form 
		(eval (cons 'sppwm arg)))))





(defun nlam-swm (a) ; Randy.Gobbel  9-May-86 14:36 
       (cond ((null a)
	      (setq a *last-tag*))
	     (t (setq *last-arg* (setq *last-tag* (list (car a))))))
       (pp-class-ids (soarmapcar #'(lambda (x)
					   (cons (cadr x)
						 (car x)))
				 (get-wm a))))





;;; Concatenated from type module "interface" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/interface/release/init.lisp".
;;; -*-mode: lisp; package: user -*-

(defun nlam-init-context (goal-name problem-space state operator) ; 
 ; John.Laird 19-May-86 15:25  ; Initialize the top context to the values 
 ; provided  ; A null object is replaced by undecided 
       (cond ((null goal-name)
	      (setq goal-name (setq problem-space
				    (setq state (setq operator
						      'undecided)))))
	     ((null problem-space)
	      (setq problem-space (setq state (setq operator 'undecided))))
	     ((null state)
	      (setq state (setq operator 'undecided)))
	     ((null operator)
	      (setq operator 'undecided))) ; Replace variables with their 
 ; bindings - or gensym new values 
       (setq *context* (list ($varbind goal-name)
			     ($varbind problem-space)
			     ($varbind state)
			     ($varbind operator)
			     0 nil nil nil nil nil 0))
       (setq *context-stack* (list *context*)))

(defun init-concat nil  ; Randy.Gobbel 17-Oct-86 12:42 
       (setq *carray* (makevector 9))
       (putvector *carray* 0 '*c1*)
       (putvector *carray* 1 '*c2*)
       (putvector *carray* 2 '*c3*)
       (putvector *carray* 3 '*c4*)
       (putvector *carray* 4 '*c5*)
       (putvector *carray* 5 '*c6*)
       (putvector *carray* 6 '*c7*)
       (putvector *carray* 7 '*c8*)
       (putvector *carray* 8 '*c9*)
       (soarmapc
	 #'(lambda (x)
		   (soarmapc #'(lambda (y)
				       (soarputprop x (soarpack t x y)
						    y))
			     '(a b s n)))
	 '(ge gt le lt ne eq xx))
       (soarputprop 'eqnil 'teqnilb 'b))

(defun init-soar nil  ; John.Laird 24-Jun-86 11:11 
       (setq *in-rhs* nil)
       (sremove *)
       (setq *init-wm* nil)
       (setq *conflict-set* nil)
       (setq *wmpart-list* nil)
       (setq *p-name* nil)
       (setq *phase* 'elaborate)
       (setq *total-token* (setq *max-token* (setq *current-token* 0)))
       (setq *added-tokens* (setq *removed-tokens* 0))
       (setq *total-wm* (setq *max-wm* (setq *current-wm* 0)))
       (setq *prod-count* (setq *decide-count* (setq *cycle-count* 0)))
       (setq *first-action* nil)
       (setq *action-count* 0)
       (gensyminit)
       (setq *current-production-trace* 'cpt)
       (soarmapc #'soarclearprops *user-ids*)
       (setq *label-bindings* nil)
       (setq *user-ids* nil)
       (setq *input-wme* nil)
       (setq *trace-number* 0)
       (setq *current-wm* (setq *max-wm* (setq *total-wm* 0)))
       (setq *context* (list (soargensym 'goal)
			     'undecided
			     'undecided
			     'undecided 1 nil nil nil nil nil 0))
       (setq *context-stack* (list *context*))
       (setq *data-matched* nil)
       (setq *current-goal* nil)
       (cond (*pfired* (soarmapc #'(lambda (x)
					   (soarputprop x 0 'pfired))
				 *pnames*))))

(defun nlam-init-wm (z) ; Randy.Gobbel 18-Jun-86 13:28 
       (setq *in-rhs* t)
       (setq *p-name* 'init-wm)
       (setq *current-goal* nil)
       (setq *data-matched* nil)
       (setq *variable-memory* nil)
       (soarmapc #'(lambda (x)
			   (cond ((neq (car x)
				       'init-context)
				  (soarmapc #'eval (sp-form-expand *p-name*
								   (list x)
								   t nil)))
				 (t (eval x))))
		 z)
       (setq *in-rhs* nil))

(defun init-wm-context nil  ; Randy.Gobbel 13-Jun-86 14:57 
       (prog (cg cp cs co)
	     (cond ((not *init-wm*)
		    (setq *phase* 'decide)
		    (setq cg (get-current *context* 'goal))
		    (setq cp (get-current *context* 'problem-space))
		    (setq cs (get-current *context* 'state))
		    (setq co (get-current *context* 'operator))
		    (soarputprop cg 1 'goal-depth)
		    (and *learning* (learn))
		    (make-info 'goal cg 'problem-space cp)
		    (trace-object 'goal cg 0)
		    (trace-object 'problem-space cp 0)
		    (make-info 'goal cg 'state cs)
		    (trace-object 'state cs 0)
		    (make-info 'goal cg 'operator co)
		    (trace-object 'operator co 0)
		    (setq *init-wm* t)
		    (setq *phase* 'elaborate)
		    (setq *initial-actions* *action-count*)
		    (setq *elapsed-time* 0)
		    (setq *elapsed-build-time* 0)))))





(defun i-g-v nil  ; Randy.Gobbel  3-Dec-86 11:19 
       (prog nil (soarsyntax)
	     (setq *soar-actions*
		   '(soar-bind build call1 call2 cbind closefile1 default halt 
			  label-bind openfile1 tabstop tabto write-reflect 
			  write1 write2))
	     (setq *prop-names*
		   '(a all-preferences autonomic b backpointers bound 
		       &bus-branch condition conflicts context creation-time 
		       external-routine gensymed goal-depth id-test in-id-field 
		       inputfile instance n name negation-index 
		       nil-goal-preferences non-results one-time-results 
		       outputfile pfired predicate preferences production 
		       production-trace results s supergoals tested topnode 
		       type var-count wm-cycle wmpart*))
	     (init-concat)
	     (setq *accept-file* nil)
	     (setq *accepting-input* nil)
	     (setq *added-tokens* (setq *removed-tokens* 0))
	     (setq *always-learn* t)
	     (setq *atrace* t)
	     (setq *brknames* nil)
	     (setq *brkpts* nil)
	     (setq *brkrun* nil)
	     (setq *cav* nil)
	     (setq *cavi* nil)
	     (setq *char-mode* t)
	     (setq *chunks* nil)
	     (setq *chunk-all-paths* nil)
	     (setq *chunk-classes* '(problem-space state operator))
	     (setq *chunk-free-problem-spaces* nil)
	     (setq *constants* nil)
	     (setq *critical* nil)
	     (setq *current-goal* nil)
	     (setq *decide-trace* nil)
	     (setq *default-multi* 5)
	     (setq *default-user-select* t)
	     (setq *elapsed-time* 0)
	     (setq *execute-path* nil)
	     (setq *free-gensym-list* (list (list 'free-list)))
	     (setq *free-pname-list* nil)
	     (setq *full-print-id* t)
	     (setq *global-pair* (list nil))
	     (setq *gtrace* nil)
	     (setq *impasse-subset-not-equal* nil)
	     (setq *in-rhs* nil)
	     (setq *indent* 0)
	     (setq *instance-attributes* nil)
	     (setq *last-node-on-bus* nil)
	     (setq *last-obj-id* (setq *last-pname* (setq *last-tag*
							  (setq *last-arg* nil))
				       ))
	     (setq *learn-ids* nil)
	     (setq *learning* nil)
	     (setq *limit-cs* 1000000)
	     (setq *limit-token* 1000000)
	     (setq *loading-default* nil)
	     (setq *ltrace* nil)
	     (setq *max-chunk-conditions* 200)
	     (setq *max-elaborations* 100)
	     (setq *max-recurse* 2)
	     (setq *mem-array-size* 10)
	     (setq *multi-attribute* nil)
	     (setq *necessity-preference-values*
		   '(acceptable prohibit reject require))
	     (setq *never-learn* t)
	     (setq *new-chunks* nil)
	     (setq *order-trace* nil)
	     (setq *parallel-not-suspend* t)
	     (setq *path* nil)
	     (setq *pcount* 0)
	     (setq *pfired* nil)
	     (setq *pnames* (setq *user-pnames* nil))
	     (setq *print-learn* 0)
	     (setq *print-pname* nil)
	     (setq *print-spo-list* nil)
	     (setq *real-cnt* (setq *virtual-cnt* 0))
	     (setq *recording* nil)
	     (setq *refracts* nil)
	     (setq *remaining-cycles* 1000000)
	     (setq *remaining-decide* 1000000)
	     (setq *save-class-list* nil)
	     (setq *select-equal* 'first)
	     (setq *spo-default-depth* 1)
	     (setq *subgoal-tabs* t)
	     (setq *print-learn* 0)
	     (setq *tracep* t)
	     (setq *tracep-list* nil)
	     (setq *ttrace* nil)
	     (setq *used-char-list* nil)
	     (setq *used-gensym-list* nil)
	     (setq *used-var-list* nil)
	     (setq *user-ids* nil)
	     (setq *warning* t)
	     (setq *watch-free-problem-spaces* nil)
	     (setq *wme-list-stack* nil)
	     (setq *wmpart-list* nil)
	     (setq *write-file* nil)
	     (machine-dependent-init)
	     (multi-attributes '((problem-space operator)
				(goal item)
				(state evaluation 2)))
	     (trace-attributes '((goal role)
				(goal impasse)
				(goal desired)
				(goal superoperator)
				(operator object)
				(operator instance)))
	     (init-gensym)
	     (create-new-class 'preference)
	     (soarmapc #'(lambda (x)
				 (soarputprop x t 'predicate))
		       '(< <= <=> <> = > >=))
	     (setq *current-branch* (make-branch))
	     (make-bottom-node)
	     (watch 0)
	     (setq *version-number* "4")
	     (setq *release-number* "4")
	     (setq *minor-version* "4")
	     (setq *date-created* "April 19, 1987")
	     (setq *public-version* nil)
	     (soar-greeting)
	     (init-soar)))


;;; Concatenated from type module "decide" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/decide/release/decide.lisp".
;;; -*-mode: lisp; package: user -*- 

(defun collect-context-fields (old-context slot difficulty result) ; 
 ; John.Laird  5-Mar-86 14:50 
       (prog (slots aslot found-slot return-list)
	     (setq slots '(goal problem-space state operator))
	     (setq return-list nil)
	     (setq found-slot nil)
	     l1
	     (setq aslot (pop slots))
	     (cond ((null aslot)
		    (return return-list)))
	     (cond (found-slot (setq return-list (append return-list
							 '(undecided))))
		   ((and difficulty (eq aslot slot)
			 (neq difficulty 'no-change))
		    (setq found-slot t)
		    (setq return-list (append return-list '(undecided))))
		   ((eq aslot slot)
		    (setq found-slot t)
		    (setq return-list (append return-list (list result))))
		   (t (setq return-list
			    (append return-list
				    (list (get-current old-context aslot))))))
	     (go l1)))


(defun compute-choice (choice) ; John.Laird 21-Nov-85 11:41 

; compute the choice for an indifferent selection

       (prog (instance-list)
	     (cond ((and (get choice 'name)
			 (get choice 'instance))
		    (setq instance-list (remove-nil
			    (soarmapcar #'compute-choice (get choice
							      'instance))))
		    (cond (instance-list (return (cons (get choice
							    'name)
						       instance-list)))
			  (t (return (get choice 'name)))))
		   ((get choice 'name)
		    (return (get choice 'name)))
		   ((get choice 'instance)
		    (setq instance-list (remove-nil
			    (soarmapcar #'compute-choice (get choice
							      'instance))))
		    (return instance-list))
		   ((get choice 'gensymed)
		    (return nil))
		   (t (return choice)))))


(defun create-difficulty-context (old-context slot difficulty result) ; 
 ; edited:  1-May-86 12:27 
       (list nil slot difficulty result (get-context-super-operator old-context)
	     (get-context-subgoaled old-context)
	     (get-context-depth old-context)))

(defun create-goal-info (data-matched) ; edited:  4-Feb-86 08:39 
       (setq *data-matched* data-matched)
       (setq *first-action* t))


(defun create-rejection-context (old-context slot difficulty result) ; 
 ; hts: 29-Jan-86 09:58 
       (append (collect-context-fields old-context slot difficulty result)
	       (create-difficulty-context old-context slot difficulty result)))



(defun find-acceptable-preference (preferences id) ; Randy.Gobbel 
 ; 21-Apr-86 12:13 
       (prog (preference)
	     l0
	     (cond ((null preferences)
		    (return nil)))
	     (setq preference (pop preferences))
	     (cond ((and (eq 'preference (wme-class preference))
			 (eq id (wme-id preference))
			 (eq 'acceptable (wme-value preference)))
		    (return preference)))
	     (go l0)))



(defun find-selected-result (choice results) ; John.Laird 30-Sep-85 10:03 

;  used for selecting when indifferent choices

       (prog (result)
	     (cond ((numberp choice)
		    (cond ((or (< choice 1)
			       (> choice (length results)))
			   (return nil)))
		    (return (soarnth (1- choice)
				     results))))
	     l1
	     (setq result (pop results))
	     (cond ((test-selected-result choice result)
		    (and (null *select-equal*)
			 (setq *select-equal* *default-user-select*))
		    (return result))
		   ((null results)
		    (return nil)))
	     (go l1)))


(defun mark-in-id (wme) ; Randy.Gobbel 12-Sep-86 12:05  ; Process every 
 ; element that is added to working memory. Test its legality, keep track 
 ; of the subgoals in which identifiers were created, etc. Most of this is 
 ; so that the results of subgoals can be determined and chunks can be 
 ; built             
 ; taken from Soar2 
 ; Processed to untweak a lucid compiler bug by Laird, installed by bgm at 3/13/87.
       (prog (id att value)
	     (setq id (wme-id wme))
	     (setq att (wme-attribute wme))
	     (setq value (wme-value wme))
	     (and (not id)
		  (return nil))
	     (cond ((and (neq *phase* 'decide)
			 (eq 'goal (wme-class wme))
			 (soarmemq att
				   '(problem-space state operator item choices 
						   supergoal)))
		    (soarwarn "Illegal attempt to modify context" wme)
		    (return t))
		   ((eq *phase* 'decide)
		    (setq *current-goal* (get-current *context* 'goal)))
		   ((not *current-goal*)
			 (setq *current-goal* 'undecided)))
	     (cond ((and (not (get id 'gensymed))
			 (not (soarmemq id *user-ids*)))
		    (soarpush id *user-ids*)))
	     (cond ((not (get id 'in-id-field))
		    (soarputprop id *current-goal* 'in-id-field)))
	     (cond ((not (get id 'id-test))
		    (soarputprop id t 'id-test)))
	     (cond ((and value (symbolp value)
			 (not (get value 'in-id-field)))
		    (soarputprop value *current-goal* 'in-id-field)))
	     (cond ((and att (symbolp att)
			 (not (get att 'in-id-field)))
		    (soarputprop att *current-goal* 'in-id-field)))
	     (cond ((and *constants* (eq att 'value)
			 (soarputprop id value 'name)))
		   ((and *constants* (eq att 'name)
			 (soaraddprop id value 'instance)))
		   ((and (not *constants*)
			 (eq att 'name))
		    (soarputprop id value 'name))
		   ((soarmember (list (wme-class wme)
				      att)
				*instance-attributes*)
		    (soaraddprop id value 'instance)))
	     (cond (*first-action* (cond ((and *ptrace* *ttrace* (not *wtrace*)
					       (trace-production?)
					       (trace-problem-space?))
					  (traceprinc "-->")))
				   (setq *first-remove* t)))
	     (save-wme-as-result-or-non-result wme *current-goal*)
	     (cond ((not *never-learn*)
		    (save-production-trace wme *current-goal*)))
	     (setq *first-action* nil)
	     (cond ((eq (wme-class wme)
			'preference)
		    (save-preference wme)))
	     (return nil)))


    
(defun most-recent-subgoal (context-stack) ; John.Laird 27-Mar-85 13:32 

;  find most recent subgoal in context stack

       (cond ((null (cdr context-stack))
	      (caar context-stack))
	     (t (most-recent-subgoal (cadr context-stack)))))

                                            

(defun munge-results (r-and-d current slot) ; Randy.Gobbel 11-Sep-86 15:12 

;    post-process the results of the decision procedure based on the
;    current value.

       (prog (results difficulty temp)
	     (setq difficulty (car r-and-d))
	     (setq results (cdr r-and-d))
	     (cond ((eq difficulty 'no-change)
		    (return nil))
		   ((eq difficulty 'suspend-current)
		    (setq temp (cadr results))
		    (setq results (process-results (car results)
						   current))
		    (and (symbolp current)
			 (setq current (list current)))
		    (cond ((and (eq slot 'operator)
				(neq results (car current)))
			   (setq results (union (list results)
						temp))
			   (and (eqp (length results)
				     1)
				(setq results (car results)))))
		    (setq difficulty nil))
		   ((not difficulty)
		    (setq results (process-results results current)))
		   ((eq difficulty 'parallel)
		    (setq difficulty nil)))
	     (cond ((and (not difficulty)
			 (symbolp results)
			 (not *parallel-not-suspend*)
			 (soarlistp current)
			 (soarmemq results (cdr current)))
		    (setq difficulty 'activate-suspend)))
	     (return (cons difficulty results))))




(defun process-instance (instance) ; Randy.Gobbel  1-May-86 10:38 
       (prog nil (setq *first-action* t)
	     (accum-stats)
	     (eval-rhs (car instance)
		       (cdr instance))
	     (check-limits)
	     (and (broken (car instance))
		  (setq *break-flag* t))))




(defun remove-current (goal slot old-value new-value) ; Randy.Gobbel 
 ; 13-Jun-86 14:58 

;    remove from working memory all old context slots


       (prog nil (cond ((and (soarlistp new-value)
			     (soarlistp old-value))
			(setq old-value (soardifference old-value new-value))))
	     (cond ((soarlistp old-value)
		    (soarmapc #'(lambda (x)
					(remove-from-wm (list 'goal goal slot x)
							))
			      old-value))
		   ((and (soarlistp new-value)
			 (not *parallel-not-suspend*)
			 (eq slot 'operator)))
		   (t (remove-from-wm (list 'goal goal slot old-value))))
	     (return (cond ((soarlistp new-value)
			    (soarmapcar #'(lambda (x)
						  (list 'goal goal slot x))
					new-value))
			   (t (list (list 'goal goal slot new-value)))))))




(defun remove-subgoal-objects (stack) ; Randy.Gobbel 17-Nov-86 14:36 

;    remove unconnected subgoal elements.

       (cond (stack (soarmapc #'remove-subgoal-objects (cdr stack))
		    (soarmapc #'remove-from-wm
			      (append (get (caar stack)
					   'non-results)
				      (flattop (get (caar stack)
						    'results))))
		    (soarclearprops (caar stack)))))




(defun add-to-wm (wme) ; Randy.Gobbel 12-Sep-86 12:06 
       (prog (hash part timetag port) ; MODIFY ADD-TO-WM SO IT ONLY ADDS 
 ; ONE COPY OF A WME ON A GIVEN ELAB CYCLE 
	     (setq port (trace-file))
	     (setq hash (wm-hash wme))
	     (setq part (get hash 'wmpart*))
	     (cond ((soarassoc wme part)
		    (and *chunk-all-paths* (mark-in-id wme))
		    (return nil))
		   ((mark-in-id wme)
		    (return nil))
		   ((soarmember wme *brkrun*)
		    (setq *break-flag* t)))
	     (trace-condition-action wme 'action)
	     (setq *critical* t)
	     (setq *action-count* (1+ *action-count*))
	     (setq *current-wm* (1+ *current-wm*))
	     (cond ((> *current-wm* *max-wm*)
		    (setq *max-wm* *current-wm*)))
	     (cond ((and (not part)
			 (not (soarmemq hash *wmpart-list*)))
		    (soarpush hash *wmpart-list*)))
	     (setq timetag *action-count*)
	     (soarputprop hash (cons (cons wme timetag)
				     part)
			  'wmpart*)
	     (soaraddprop hash (cons wme *cycle-count*)
			  'wm-cycle)
	     (match 'new wme)
	     (setq *critical* nil)
	     (cond ((or (not *in-rhs*)
			(not *ptrace*)
			(not (trace-problem-space?))
			(not (trace-production?))))
		   ((and (not *wtrace*)
			 *ttrace*)
		    (time-tag-print (list wme)
				    port))
		   (*wtrace* (traceprintc "=>WM: ")
			     (ppelm wme port)))))




(defun conflict-resolution nil  ; Randy.Gobbel  8-May-86 16:37 

;    decide which productions to fire.


       (prog (best best-names instance new-cs)
	     (setq best-names (setq best nil))
	     (and (or (null *conflict-set*)
		      (all-autonomic *conflict-set*))
		  (return nil))
	     loop
	     (setq instance (pop *conflict-set*))
	     (cond ((and (eq (get (car instance) 'type) 'serial)
			 (soarmemq (car instance) best-names))
		    (soarpush instance new-cs))
		   (t (soarpush instance best)
		      (soarpush (car instance) best-names)))
	     (and *conflict-set* (go loop))
	     (setq *conflict-set* new-cs)
	     (return best)))

(defun insertcs (name data) ; Randy.Gobbel 16-Jul-86 16:44 

       (prog (instan)
	     (and (refracted name data)
		  (return nil))
	     (setq instan (cons name data))
	     (and (atom *conflict-set*)
		  (setq *conflict-set* nil))
	     (return (setq *conflict-set* (cons instan *conflict-set*)))))


(defun main nil  ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (input phase-set r)
	     (setq *halt-flag* nil)
	     (setq *break-flag* nil)
	     (setq *elaborations-count* 0)
	     (setq phase-set nil)
	     ;; This next clause does nothing so far as I can tell. BGM
	     (and *accepting-input* (control t))
	     dil
	     (cond (*halt-flag* (setq r "End -- Explicit Halt")
				(go finis))
		   ((or (zerop *remaining-decide*) (zerop *remaining-cycles*))
		    (setq *break-flag* t)))
	     (cond ((or *break-flag* (soarlisten))
		    (setq r '***break***)
		    (go finis)))
	     (and *accepting-input* 
		  (cond ((readp t)
			 (cond (*char-mode* (setq input (readc)))
			       (t (stop-elapsed-time)
				  (setq input (ratom))
				  (start-elapsed-time)))
			 (setq *first-action* t)
			 (add-input input))))
	     (setq *first-remove* (setq *first-action* t))
	     (setq phase-set (conflict-resolution))
	     (cond ((eq *phase* 'decide)
		    (setq *remaining-cycles* (1- *remaining-cycles*))
		    (setq *remaining-decide* (1- *remaining-decide*))
		    (setq *cycle-count* (1+ *cycle-count*))
		    (setq *decide-count* (1+ *decide-count*))
		    (setq *p-name* nil)
		    (setq *elaborations-count* 0)
		    (setq *in-rhs* t)
		    (process-decide)
		    (setq *in-rhs* nil)
		    (setq *phase* 'elaborate))
		   ((not phase-set) (setq *phase* 'decide))
		   (t (setq *cycle-count* (1+ *cycle-count*))
		      (cond ((and *wtrace* (trace-problem-space?))
			     (traceprintc "--Elaboration Phase--")))
		      (soarmapc #'process-instance phase-set)
		      (create-production)
		      (setq *remaining-cycles* (1- *remaining-cycles*))
		      (setq *elaborations-count* (1+ *elaborations-count*))
		      (cond ((eqp *max-elaborations* *elaborations-count*)
			     (soarwarn 
	   "Exceeded *max-elaborations*. Proceeding to decision procedure."
				       *max-elaborations*)
			     (setq *phase* 'decide)))))
	     (go dil)
	     finis
	     (and *accepting-input* (control nil))
	     (setq *p-name* nil)
	     (traceterpri)
	     (return r)))



(defun process-changes (adds dels) ; Randy.Gobbel 19-Mar-86 12:49 
       (prog (x)
	     process-deletes
	     (and (atom dels)
		  (go process-adds))
	     (setq x (car dels))
	     (setq dels (cdr dels))
	     (remove-from-wm x)
	     (go process-deletes)
	     process-adds
	     (and (atom adds)
		  (return nil))
	     (setq x (car adds))
	     (setq adds (cdr adds))
	     (add-to-wm x)
	     (go process-adds)))




(defun record-refract (rule data) ; edited: 16-Feb-84 13:20 
       (and *recording* (setq *record* (cons '<=refract
					     (cons rule (cons data *record*)))))
       )



(defun refracted (rule data) ; RG: 20-Feb-86 20:57 
       (prog (z)
	     (and (null *refracts*)
		  (return nil))
	     (setq z (cons rule data))
	     (return (soarmember z *refracts*))))




(defun remove-from-conflict-set (name)
       (prog (cs entry)
	     l1
	     (setq cs *conflict-set*)
	     l2
	     (cond ((atom cs)
		    (return nil)))
	     (setq entry (car cs))
	     (setq cs (cdr cs))
	     (cond ((eq name (car entry))
		    (setq *conflict-set* (dremove entry *conflict-set*))
		    (go l1))
		   (t (go l2)))))



(defun remove-from-wm (wme) ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (hash z part port) ; Remove-from-wm uses equal not eq to 
 ; determine if wme is present 
	     (setq port (trace-file))
	     (setq wme (eliminate-trailing-nil wme))
	     (setq hash (wm-hash wme))
	     (setq part (get hash 'wmpart*))
	     (setq z (cond ((eq (wme-class wme)
				'goal)
			    (soarassoc wme part))
			   (t (soarassq wme part))))
	     (or z (return nil))
	     (setq wme (car z))
	     (cond ((and *in-rhs* *first-remove*)
		    (and *ptrace* *ttrace* (trace-problem-space?)
			 (trace-production?)
			 (not *wtrace*)
			 (soarprinc " <--" port))
		    (setq *first-action* t)
		    (setq *first-remove* nil)))
	     (cond ((or (not *in-rhs*)
			(not (trace-problem-space?))
			(not (trace-production?))))
		   ((and (not *wtrace*)
			 *ptrace* *ttrace*)
		    (time-tag-print (list wme)
				    port))
		   (*wtrace* (traceprintc "<=WM: ")
			     (ppelm wme port)))
	     (setq *action-count* (1+ *action-count*))
	     (setq *critical* t)
	     (setq *current-wm* (1- *current-wm*))
	     (match nil wme)
	     (soarputprop hash (dremove z part)
			  'wmpart*)
	     (cond ((and (eq (wme-class wme)
			     'preference)
			 (null (wme-goal wme)))
		    (soarputprop (top-goal)
				 (dremove wme
					  (get (top-goal)
					       'nil-goal-preferences))
				 'nil-goal-preferences)))
	     (setq *critical* nil)))



(defun remove-items (items context) ; Randy.Gobbel 13-Jun-86 15:01 
       (cond (items (remove-from-wm (list 'goal (get-current context
							     'goal)
					  'item
					  (car items)))
		    (remove-items (cdr items)
				  context))))




(defun removecs (name data) ; John.Laird 17-Dec-84 13:48 
       (prog (cr-data inst cs) ; CONFLICT RESOLUTION  ; EACH CONFLICT SET 
 ; ELEMENT HAD A LIST OF THE FOLLOWING FORM  ; ((P-NAME) (SORTED 
 ; WM-RECENCY) SPECIAL-CASE-NUMBER)  ; NOW THEY HAVE  ; (P-NAME) 
	     (setq cr-data (cons name data))
	     (setq cs *conflict-set*)
	     l
	     (cond ((null cs)
		    (record-refract name data)
		    (return nil)))
	     (setq inst (car cs))
	     (setq cs (cdr cs))
	     (and (not (top-levels-eq inst cr-data))
		  (go l))
	     (setq *conflict-set* (dremove inst *conflict-set*))))




(defun all-autonomic (cs) ; Randy.Gobbel 24-Jun-86 12:50 
       (cond ((null cs)
	      t)
	     ((and (get (caar cs)
			'autonomic)
		   (all-autonomic (cdr cs))))))



(defun ask-for-choice (choice-list) ; Randy.Gobbel 31-Jul-86 15:16 
       (prog (choice)
	     (stop-elapsed-time)
	     (setq choice (soar-menu "Select one" choice-list))
	     (start-elapsed-time)
	     (return choice)))




(defun decide-trace (arg) ; Randy.Gobbel 26-Mar-86 21:41 
       (setq *decide-trace* arg))




(defun decide-tracer (label list) ; Randy.Gobbel 12-Sep-86 12:08  ; 
 ; John.Laird 26-Sep-85 08:50 
       (cond (*decide-trace* (traceterpri)
			     (traceprinc3 " " label " ")
			     (soarmapc #'print-id list))))




(defun decision-procedure (slot-preferences current) ; Randy.Gobbel 
 ; 11-Sep-86 18:02  ; APPLY THE DECISION PROCEDURE TO THE OBJECTS IN THE 
 ; VALUE LIST 
       (prog (new-values saved-values suspend-current required-values 
			 intersection) ; CURRENT IS THE CURRENT VALUE OF 
 ; THE SLOT  ; RETURN (RESULT-TYPES RESULT1 RESULT2) 
	     (setq *decision-preferences* nil)
	     (setq required-values (process-require slot-preferences))
	     (decide-tracer 'require required-values)
	     (cond ((null required-values))
		   ((setq intersection (process-prohibit slot-preferences 
							 required-values))
		    (return (cons 'constraint-failure (soarsort intersection))))
		   ((eqp (length required-values)
			 1)
		    (return (cons nil required-values)))
		   (required-values (return (cons 'constraint-failure
						  (soarsort required-values)))))
	     (setq new-values (find-unary-preference-values 'acceptable 
							   slot-preferences))
	     (decide-tracer 'acceptable new-values)
	     (cond ((null new-values)
		    (cond ((and (neq current 'undecided)
				(null (process-rejects (flatten (list current))
						       slot-preferences current)
				      ))
			   (return '(rejection))))
		    (return '(no-change))))
	     (setq new-values (process-rejects new-values slot-preferences 
					       current))
	     (decide-tracer 'post-reject new-values)
	     (cond ((null new-values)
		    (return '(rejection)))) ; SUSPEND-CURRENT IS SET IF 
 ; THE CURRENT OBJECT IS NOT REJECTED NOR IS IT UNDECIDED 
	     (setq suspend-current (cond (*parallel-not-suspend* nil)
					 ((and (soarlistp current)
					       (intrq current new-values)))
					 ((and (symbolp current)
					       (soarmemq current new-values))
					  (list current))
					 (t nil)))
	     (setq saved-values new-values)
	     (setq new-values (process-worse new-values slot-preferences))
	     (decide-tracer 'post-worse new-values)
	     (cond ((null new-values)
		    (return (cons 'conflict (soarsort saved-values)))))
	     (setq new-values (process-best new-values slot-preferences))
	     (decide-tracer 'post-best new-values)
	     (cond ((eqp (length new-values)
			 1)
		    (setq new-values (cons 'all-indifferent new-values)))
		   ((neq (car new-values)
			 'all-best)
		    (setq new-values (process-worst new-values slot-preferences)
			  )
		    (decide-tracer 'post-worst new-values)
		    (cond ((neq (car new-values)
				'all-worst)
			   (setq new-values (process-indifferent new-values 
							   slot-preferences))
			   (decide-tracer 'post-indifferent new-values)
			   (cond ((neq (car new-values)
				       'all-indifferent)
				  (return (process-parallel (soarsort 
								 new-values)
							    slot-preferences
							    'tie)))))))
		   (t (setq new-values (process-worst (cdr new-values)
						      slot-preferences))
		      (decide-tracer 'post-worst new-values)
		      (cond ((neq (car new-values)
				  'all-worst)
			     (setq new-values (cons 'all-best new-values))))))
	     (cond (suspend-current (return (list 'suspend-current
						  (cdr new-values)
						  suspend-current)))
		   (t (return (process-parallel (cdr new-values)
						slot-preferences nil))))))



(defun find-binary-preference-values (value slot-preferences) ; edited: 
 ;  2-May-86 09:05 
       (cdr (soarassq value slot-preferences)))



(defun find-no-change-slot (context) ; John.Laird 26-Mar-85 15:39 
       (cond ((eq (cadr context)
		  'undecided)
	      'goal)
	     ((eq (caddr context)
		  'undecided)
	      'problem-space)
	     ((eq (cadddr context)
		  'undecided)
	      'state)
	     (t 'operator)))



(defun find-unary-preference-values (value slot-preferences) ; edited: 
 ;  1-May-86 11:24 
       (soarmapcar #'car (cdr (soarassq value slot-preferences))))



(defun intersection-values (list a-list) ; edited:  2-May-86 15:01 
       (cond ((null list)
	      nil)
	     ((soarassq (car list)
			a-list)
	      (cons (soarassq (car list)
			      a-list)
		    (cdr list)))
	     (t (intersection-values (cdr list)
				     a-list))))



(defun make-info (info goal-id attribute value) ; Randy.Gobbel 
 ; 23-Apr-86 16:02 
       (prog (wme)
	     (setq wme (list info goal-id attribute value))
	     (add-to-wm wme)
	     (return wme)))



(defun post-process-results (goal) ; Randy.Gobbel  2-Sep-86 16:04 
       (cond ((get goal 'one-time-results)
	      (cond ((and (not *never-learn*)
			  (or *always-learn*
			      (not (get-context-subgoaled (get goal
							       'context)))))
		     (start-build-time)
		     (handle-back-trace-actions goal)
		     (stop-build-time)))
	      (soaraddprop goal (get goal 'one-time-results)
			   'results)
	      (soarputprop goal nil 'one-time-results)))
       (cond ((get goal 'supergoals)
	      (post-process-results (car (get goal 'supergoals))))))



(defun process-best (new-values slot-preferences) ; edited: 
 ;  2-May-86 14:48  ; John.Laird 11-Sep-85 14:54 
       (prog (return-list best-list)
	     (setq best-list (find-unary-preference-values 'best 
							   slot-preferences))
	     (setq return-list (intrq best-list new-values))
	     (cond (return-list (setq *decision-preferences* nil)
				(return (cons 'all-best return-list)))
		   (t (return new-values)))))



(defun process-decide nil  ; John.Laird 21-Apr-86 12:55  ; CONTEXT-STACK 
 ; ORGANIZATION  ; (OLDEST-CONTEXT (NEXT-OLDEST ETC))  ; (GOAL-ID 
 ; PROBLEM-SPACE STATE OPERATOR NUMBER-OF-PREFERENCES SLOT DESIRED RESULTS) 
       (accum-stats)
       (setq *new-chunks* nil)
       (process-context-stack *context-stack* nil))



(defun process-indifferent (new-values slot-preferences) ; Randy.Gobbel 
 ; 11-Sep-86 18:03 
       (prog (new-equals unary-equal-list binary-equal-list equal-pair 
			 untested-objects)
	     (cond ((eqp (length new-values)
			 1)
		    (return (cons 'all-indifferent new-values))))
	     (setq unary-equal-list (find-unary-preference-values '
							  unary-indifferent 
							   slot-preferences))
	     (setq binary-equal-list (find-binary-preference-values
		     'indifferent slot-preferences))
	     (setq new-equals (intrq unary-equal-list new-values))
	     (setq untested-objects (soardifference new-values unary-equal-list)
		   )
	     (cond ((and (null new-equals)
			 (null binary-equal-list))
		    (return new-values))
		   ((and (null binary-equal-list)
			 (null (soardifference new-values new-equals)))
		    (return (cons 'all-indifferent new-values)))
		   ((null new-equals)
		    (setq new-equals (list (pop untested-objects)))))
	     l1
	     (cond ((null untested-objects)
		    (return (cons 'all-indifferent new-values))))
	     (setq equal-pair (pop binary-equal-list))
	     (cond ((soarmemq (car equal-pair)
			      new-equals)
		    (cond ((soarmemq (cadr equal-pair)
				     new-equals)
			   (setq untested-objects (remove (cadr equal-pair)
							  untested-objects))
			   (go l1))
			  ((test-indifferent-to-all (cadr equal-pair)
						    new-equals 
						    binary-equal-list)
			   (soarpush (cadr equal-pair)
				     new-equals)
			   (setq untested-objects (remove (cadr equal-pair)
							  untested-objects))
			   (go l1))))
		   ((soarmemq (cadr equal-pair)
			      new-equals)
		    (cond ((test-indifferent-to-all (car equal-pair)
						    new-equals 
						    binary-equal-list)
			   (soarpush (car equal-pair)
				     new-equals)
			   (setq untested-objects (remove (car equal-pair)
							  untested-objects))
			   (go l1)))))
	     (return new-values)))



(defun
  process-parallel
  (results slot-preferences difficulty) ; Randy.Gobbel 11-Sep-86 18:04  ; 
 ; John.Laird 20-Jan-86 16:47 
  (prog (parallel-list parallel-results new-results unary-list) ; RETURN 
 ; LIST WITH PARALLEL FOLLOWED BY RESULTS 
	(setq parallel-list (find-binary-preference-values 'parallel 
							   slot-preferences))
	(setq unary-list (find-unary-preference-values 'unary-parallel 
						       slot-preferences))
	(setq parallel-results (intrq unary-list results))
	(cond ((or (eqp (length results)
			1)
		   (and (null parallel-list)
			(null parallel-results)))
	       (cond ((eq difficulty 'tie)
		      (setq *decision-preferences*
			    (append *decision-preferences*
				    (soarmapcar #'caddr
						(find-binary-preference-values
						  'acceptable slot-preferences))
				    ))))
	       (return (cons difficulty results))))
	(setq new-results (soardifference results unary-list))
	l1
	(cond ((null new-results)
	       (decide-tracer 'parallel results)
	       (return (cons 'parallel results))))
	(cond ((soarmember (list (car new-results)
				 (car parallel-results))
			   parallel-list)
	       (soarpush (pop new-results)
			 parallel-results)
	       (go l1))
	      (t (return (cons difficulty results))))))




(defun process-prohibit (slot-preferences required-values) ; edited: 
 ; 13-May-86 09:04 
       (prog (prohibited-values return-list intersection)
	     (setq prohibited-values (find-binary-preference-values
		     'prohibit slot-preferences))
	     (setq return-list (soarmapcar #'car prohibited-values))
	     (cond ((null prohibited-values)
		    (return nil))
		   ((setq intersection (intrq required-values return-list))
		    (setq *decision-preferences*
			  (append *decision-preferences*
				  (soarmapcar #'caddr
					      (intersection-values intersection 
							  prohibited-values))))
		    (return intersection)))
	     (setq *decision-preferences* (append *decision-preferences*
						  (soarmapcar #'caddr 
							  prohibited-values)))
	     (return intersection)))



#|

(defun process-rejects (new-values slot-preferences current) ; 
 ; Randy.Gobbel  2-Dec-86 16:00 
       (prog (return-list reject-list intersection prohibited-values 
			  prohibited-objects)
	     (setq reject-list (find-unary-preference-values 'reject 
							   slot-preferences))
	     (setq prohibited-objects (find-binary-preference-values
		     'prohibit slot-preferences))
	     (cond (prohibited-objects
		     (setq *decision-preferences*
			   (append *decision-preferences*
				   (soarmapcar #'caddr
					       (intersection-values 
							       intersection 
							  prohibited-values))))
		     (setq prohibited-values (soarmapcar #'car 
							 prohibited-objects))
		     (setq reject-list (append reject-list prohibited-values))))
	     (setq return-list (soardifference new-values reject-list))
	     (cond (return-list (return return-list))
		   ((soarlistp current)
		    (return (soardifference current reject-list)))
		   ((and (neq current 'undecided)
			 (not (soarmemq current reject-list)))
		    (return (list current)))
		   (t (setq *decision-preferences*
			    (append *decision-preferences*
				    (soarmapcar #'caddr
						(find-binary-preference-values
						  'reject slot-preferences))))
		      (return nil)))))
Old process rejects New follows.

|#

(defun process-rejects (new-values slot-preferences current) ; 
 ; Randy.Gobbel  2-Dec-86 16:00 
       (prog (return-list reject-list intersection prohibited-values 
			  prohibited-objects)
	     (setq reject-list (find-unary-preference-values 'reject 
							   slot-preferences))
	     (setq prohibited-objects (find-binary-preference-values
		     'prohibit slot-preferences))
	     (cond (prohibited-objects
		     (setq *decision-preferences*
			   (append *decision-preferences*
				   (soarmapcar #'caddr prohibited-values)))
		     (setq prohibited-values (soarmapcar #'car 
							 prohibited-objects))
		     (setq reject-list (append reject-list prohibited-values))))
	     (setq return-list (soardifference new-values reject-list))
	     (cond (return-list (return return-list))
		   ((soarlistp current)
		    (return (soardifference current reject-list)))
		   ((and (neq current 'undecided)
			 (not (soarmemq current reject-list)))
		    (return (list current)))
		   (t (setq *decision-preferences*
			    (append *decision-preferences*
				    (soarmapcar #'caddr
						(find-binary-preference-values
						  'reject slot-preferences))))
		      (return nil)))))

(defun process-require (slot-preferences) ; Randy.Gobbel 16-Jul-86 16:46 
       (prog (required-preferences)
	     (setq required-preferences (find-binary-preference-values
		     'require slot-preferences))
	     (and (null required-preferences)
		  (return nil))
	     (setq *decision-preferences* (soarmapcar #'caddr 
						      required-preferences))
	     (return (soarmapcar #'car required-preferences))))




(defun process-results (results id) ; John.Laird 19-May-86 15:27  ; RETURN 
 ; ONE RESULT 
       (cond ((null (cdr results))
	      (car results))
	     ((and (soarlistp id)
		   (soarmemq (car id)
			     results))
	      (car id))
	     ((soarmemq id results)
	      id)
	     ((eq *select-equal* 'first)
	      (car results))
	     ((soarlistp *select-equal*)
	      (cond ((find-selected-result (pop *select-equal*)
					   results))
		    (t (setq *select-equal* t)
		       (ask-for-choice results))))
	     (*select-equal* (ask-for-choice results))
	     (t (soarnth (random (1- (length results)))
			 results))))



(defun process-worse (new-values slot-preferences) ; edited: 
 ; 13-May-86 09:10  ; John.Laird 17-Aug-84 15:14 
       (prog (worse-list current-worse bad-objects)
	     (setq worse-list (find-binary-preference-values 'worse 
							   slot-preferences))
	     (setq bad-objects nil)
	     l1
	     (cond ((null worse-list)
		    (return (soardifference new-values bad-objects))))
	     (setq current-worse (pop worse-list))
	     (cond ((and (soarmemq (cadr current-worse)
				   new-values)
			 (soarmemq (car current-worse)
				   new-values))
		    (soarpush (car current-worse)
			      bad-objects)))
	     (go l1)))




(defun process-worst (new-values slot-preferences) ; edited: 
 ;  2-May-86 09:48  ; John.Laird 18-Apr-85 10:27 
       (prog (worst-list return-list)
	     (setq worst-list (find-unary-preference-values 'worst 
							   slot-preferences))
	     (setq return-list (soardifference new-values worst-list))
	     (cond (return-list (return return-list))
		   (t (return (cons 'all-worst (intrq new-values worst-list)))))
	     ))




(defun test-indifferent-to-all (item list binary-equal-list) ; John.Laird 
 ; 13-May-86 13:11 
       (cond ((null list)
	      t)
	     ((soarmember (list item (car list))
			  binary-equal-list)
	      (test-indifferent-to-all item (cdr list)
				       binary-equal-list))
	     ((soarmember (list (car list)
				item)
			  binary-equal-list)
	      (test-indifferent-to-all item (cdr list)
				       binary-equal-list))
	     (t nil)))




(defun test-selected-result (choice result) ; John.Laird 15-May-86 10:14  ; 
 ; John.Laird 24-Sep-85 12:08 
       (cond ((null result)
	      nil)
	     ((null choice)
	      t)
	     ((soarlistp choice)
	      (and (test-selected-result (car choice)
					 result)
		   (test-selected-result (cdr choice)
					 result)))
	     ((soarlistp result)
	      (or (test-selected-result choice (car result))
		  (test-selected-result choice (cdr result))))
	     (t (or (eq choice result)
		    (and (symbolp result)
			 (or (eq choice (get result 'name))
			     (soarmemq choice (get result 'instance))
			     (and (get result 'instance)
				  (test-selected-result choice (get result 'instance)))))))))




(defun top-goal nil  ; Randy.Gobbel 24-Jun-86 12:51 
       (get-current (car *context-stack*)
		    'goal))




(defun user-select (x) ; Randy.Gobbel 26-Mar-86 18:59  ; John.Laird 
 ; 24-Sep-85 09:21 
       (setq *select-equal* x))




(defun
  change-context
  (difficulty results old-context slot context-stack) ; Randy.Gobbel 
 ; 12-Sep-86 12:06  ; MAKE THE NECESSARY CHANGE TO THE NEW CONTEXT - FLUSH 
 ; ANY SUBCONTEXTS UNLESS GOAL SUSPENDED 
  (prog
    (new-context)
    (cond ((and *wtrace* (cdr context-stack)
		(trace-production?)
		(trace-problem-space?))
	   (traceprintc "--Removal Phase--")))
    (setq new-context (create-new-context difficulty results old-context slot))
    (soarputprop (get-current new-context 'goal)
		 new-context
		 'context)
    (process-parallel-operators new-context context-stack)
    (cond ((and *wtrace* (trace-production?)
		(trace-problem-space?))
	   (traceprintc "--Decision Phase--")))
    (cond
      ((eq difficulty 'activate-suspend)
       (create-production)
       (return new-context))
      (difficulty
	(write-trace " " difficulty slot)
	(and (eq difficulty 'no-change)
	     (eq slot 'operator)
	     (not *parallel-not-suspend*)
	     (soarlistp results)
	     (setq results (car results)))
	(change-to-new-context new-context old-context)
	(cond ((and *parallel-not-suspend* (eq difficulty 'no-change)
		    (eq slot 'operator)
		    (soarlistp results))
	       (rplacd context-stack
		       (soarmapcar #'(lambda (x)
					     (create-goal-context difficulty 
								  slot x 
								old-context))
				   results)))
	      (t (rplacd context-stack
			 (cons (create-goal-context difficulty slot results 
						    old-context)
			       (cdr context-stack)))))
	(return new-context))
      (t (write-trace " DECIDE " slot results)
	 (change-to-new-context new-context old-context)
	 (recompute-preferences new-context)
	 (return new-context)))))




(defun change-to-new-context (new-context old-context) ; John.Laird 
 ; 13-May-86 17:28  ; John.Laird 22-Jan-86 14:39 
       (prog (slots new-contexts slot depth context-changes) ; FIRST 
 ; REMOVE OUT-OF-DATE GOAL-CONTEXT-INFOS THEN ADD IN NEW ONES 
	     (setq slots '(operator state problem-space))
	     (setq new-contexts nil)
	     (setq *p-name* 'decision-procedure)
	     (setq *context* new-context)
	     (setq depth (get-context-depth new-context))
	     (setq context-changes nil)
	     l1
	     (setq slot (pop slots))
	     (cond ((null slot)
		    (soarmapc #'(lambda (x)
					(trace-object (car x)
						      (cdr x)
						      depth))
			      context-changes)
		    (soarmapc #'(lambda (x)
					(cond ((neq (wme-value x)
						    'undecided)
					       (setq *data-matched* 
						     *decision-preferences*)
					       (setq *first-action* t))
					      (t (setq *data-matched* nil)
						 (setq *first-action* t)))
					(add-to-wm x))
			      new-contexts)
		    (return))
		   ((neq (get-current new-context slot)
			 (get-current old-context slot))
		    (setq new-contexts
			  (nconc (remove-current (get-current old-context
							      'goal)
						 slot
						 (get-current old-context slot)
						 (get-current new-context slot))
				 new-contexts))
		    (soarpush (cons slot (get-current new-context slot))
			      context-changes)))
	     (go l1)))




(defun create-goal-context (desired slot results context) ; Randy.Gobbel 
 ; 17-Nov-86 13:56  ; Creates all of the working-memory elements for a new 
 ; goal. It adds properties to the goal symbol It must also save 
 ; production traces for some of the changes to working memory. 
       (prog (goal-id created-list result superoperator temp supergoal)
	     (setq *p-name* 'decision-procedure)
	     (setq *data-matched* nil)
	     (setq supergoal (get-current context 'goal))
	     (setq goal-id (soargensym 'g))
	     (soarputprop goal-id (cons supergoal (get supergoal
						       'supergoals))
			  'supergoals)
	     (soarputprop goal-id (1+ (get supergoal 'goal-depth))
			  'goal-depth)
	     (setq *context* (list goal-id 'undecided 'undecided
				   'undecided 0 nil nil nil results nil
				   (1+ (get-context-depth context))))
	     (soarputprop goal-id *context* 'context)
	     (setq created-list (list (make-info 'goal goal-id
						 'problem-space
						 'undecided)
				      (make-info 'goal goal-id
						 'supergoal supergoal)
				      (make-info 'goal goal-id 'state
						 'undecided)
				      (make-info 'goal goal-id 'operator
						 'undecided)))
	     (cond ((eq desired 'rejection)
		    (setq slot
			  (cadr (soarmemq slot
					  '(operator state problem-space goal)))
			  )))
	     (cond ((or (eq desired 'rejection)
			(eq desired 'constraint-failure))
		    (create-goal-info *decision-preferences*)))
	     (nconc created-list
		    (list (make-info 'goal goal-id 'choices
				     (cond ((or (eq desired 'no-change)
						(eq desired 'rejection))
					    'none)
					   ((eq desired
						'constraint-failure)
					    'constraint-failure)
					   (t 'multiple)))))
	     (nconc created-list (list (make-info 'goal goal-id 'impasse 
						  desired)))
	     (setq superoperator 'undecided)
	     (setq temp nil)
	     (cond ((eq desired 'no-change)
		    (cond ((eq slot 'operator)
			   (setq superoperator results))
			  ((eq slot 'state)
			   (setq temp (list (list 'goal supergoal
						  'operator
						  'undecided))))
			  ((eq slot 'problem-space)
			   (setq temp (list (list 'goal supergoal 'state
						  'undecided))))
			  ((eq slot 'goal)
			   (setq temp (list (list 'goal supergoal
						  'problem-space
						  'undecided)))))))
	     (create-goal-info temp)
	     (nconc created-list (list (make-info 'goal goal-id 'role slot)))
	     (create-goal-info (list (list 'goal supergoal 'operator 
					   superoperator)))
	     (nconc created-list (list (make-info 'goal goal-id
						  'superoperator superoperator))
		    )
	     l4
	     (cond ((atom results)
		    (soarputprop goal-id (nreverse created-list)
				 'non-results)
		    (trace-object 'goal goal-id
				  (1+ (get-context-depth context)))
		    (return (list *context*))))
	     (setq result (pop results))
	     (create-goal-info (list (find-acceptable-preference 
						     *decision-preferences* 
								 result)))
	     (nconc created-list (list (make-info 'goal goal-id 'item result)))
	     (go l4)))




(defun create-new-context (difficulty result old-context slot) ; 
 ; John.Laird 21-Apr-86 13:50 
       (prog nil (cond ((eq difficulty 'activate-suspend)
			(return (append (list (get-current old-context
							   'goal)
					      (get-current old-context
							   'problem-space)
					      (get-current old-context
							   'state)
					      result)
					(create-difficulty-context
					  old-context slot 'no-change result))))
		       )
	     (cond (difficulty (return (create-rejection-context old-context 
								 slot 
								 difficulty 
								 result))))
	     (return (append (collect-context-fields old-context slot 
						     difficulty result)
			     (list nil nil nil nil
				   (get-context-super-operator old-context)
				   (get-context-subgoaled old-context)
				   (get-context-depth old-context))))))




(defun find-slot-preferences (context slot) ; John.Laird 21-Apr-86 10:15 
       (cdr (soarassq slot (get (car context)
				'preferences))))




(defun get-context-depth (context) ; Randy.Gobbel 26-Mar-86 20:18  ; 
 ; John.Laird 26-Mar-85 10:30 
       (soarnth 10 context))




(defun get-context-difficulty (context) ; Randy.Gobbel 26-Mar-86 20:19 
       (soarnth 6 context))




(defun get-context-pnumber (context) ; Randy.Gobbel 26-Mar-86 20:20 
       (soarnth 4 context))




(defun get-context-results (context) ; Randy.Gobbel 26-Mar-86 20:21 
       (soarnth 7 context))




(defun get-context-slot (context) ; Randy.Gobbel 26-Mar-86 20:22 
       (soarnth 5 context))




(defun get-context-subgoaled (context) ; Randy.Gobbel 26-Mar-86 20:23  ; 
 ; John.Laird 26-Mar-85 10:30 
       (soarnth 9 context))




(defun get-context-super-operator (context) ; Randy.Gobbel 26-Mar-86 20:24  ; 
 ; John.Laird 26-Mar-85 10:29 
       (soarnth 8 context))




(defun get-current (context slot) ; Randy.Gobbel 26-Mar-86 20:25  ; 
 ; John.Laird 18-Apr-85 10:28 
       (soarnth (cdr (soarassq slot '((goal . 0)
				(problem-space . 1)
				(state . 2)
				(operator . 3))))
		context))


(defun neq-result (difficulty result context slot context-stack) ; 
 ; John.Laird 19-May-86 17:11  ; John.Laird 21-Jan-86 12:11 
       (cond (difficulty (or (neq difficulty (get-context-difficulty context))
			     (neq slot (get-context-slot context))
			     (not (test-results-subset result context 
						       context-stack))))
	     ((soarlistp result)
	      (not (test-slot-subset result context)))
	     ((soarlistp (get-current context slot))
	      (not (soarmemq result (get-current context slot))))
	     (t (neq result (get-current context slot)))))




(defun process-a-parallel-operator (context-stack results) ; John.Laird 
 ; 16-Jun-86 16:06 
       (and (symbolp results)
	    (setq results (list results)))
       (cond ((soarmemq (get-context-super-operator (car context-stack))
			results)
	      (list context-stack))
	     (t (process-sub-contexts context-stack)
		nil)))




(defun process-context (context context-stack) ; Randy.Gobbel 
 ;  2-Dec-86 16:04  ; Collect all the preferences for the current context 
 ; and then loop through each slot until a change is found and return t if 
 ; no change is found return nil 
       (prog (slot-preferences slots slot ids results difficulty current) ; 
 ; Context preferences should be of form (slot value id compared)  ; Set 
 ; context-preference-change flag back to nil 
	     (rplaca (soarnthcdr 4 context)
		     nil)
	     (setq slots '(problem-space state operator))
	     (setq ids (cdr context))
	     l1
	     (setq current (pop ids))
	     (setq slot (pop slots))
	     (cond ((null slot)
		    (setq difficulty 'no-change)
		    (setq slot (find-no-change-slot context))
		    (setq current (get-current context slot))
		    (cond ((neq-result difficulty current context slot 
				       context-stack)
			   (rplaca context-stack
				   (change-context difficulty current context 
						   slot context-stack))
			   (return t))
			  ((and (soarlistp current)
				(eq slot 'operator)
				*parallel-not-suspend*)
			   (process-parallel-operators context context-stack)
			   (return nil))
			  (t (return nil)))))
	     (cond (*decide-trace* (traceprintc slot)
				   (traceprinc " ")
				   (print-id current)))
	     (setq slot-preferences (find-slot-preferences context slot))
	     (setq results (munge-results (decision-procedure slot-preferences 
							      current)
					  current slot))
	     (and (null results)
		  (go l1))
	     (setq difficulty (car results))
	     (setq results (cdr results))
	     (cond ((neq-result difficulty results context slot context-stack)
		    (and (broken2 results)
			 (setq *remaining-decide* 0))
		    (rplaca context-stack (change-context difficulty results 
							  context slot 
							  context-stack))
		    (return t))
		   (difficulty (return))
		   ((eq results 'undecided)
		    (setq slots nil)))
	     (go l1)))




(defun process-context-stack (context-stack super-context) ; John.Laird 
 ; 21-Apr-86 11:16  ; John.Laird 22-Jan-86 16:16 
       (prog (context)
	     (setq context (car context-stack))
	     (cond ((test-suspended-context context super-context)
		    (return nil))
		   ((null (cdr context-stack))
		    (process-context context context-stack)
		    (check-limits)
		    (return t))
		   ((test-context context super-context)
		    (cond ((process-context context context-stack)
			   (check-limits)
			   (return t))))
		   (t))
	     (cond ((t-in-list?
		      (soarmapcar #'(lambda (x)
					    (process-context-stack x context))
				  (cdr context-stack)))
		    (return t))
		   (t (process-context context context-stack)
		      (check-limits)
		      (return t)))))




(defun process-parallel-operators (new-context context-stack) ; John.Laird 
 ; 16-Jun-86 16:07  ; John.Laird 21-Jan-86 14:16 
       (rplacd context-stack
	       (soarmapconc #'(lambda (x)
				      (process-a-parallel-operator
					x
					(get-current new-context
						     'operator)))
			    (cdr context-stack))))




(defun process-sub-contexts (context-stack) ; Randy.Gobbel  8-Oct-86 14:47  ; 
 ; John.Laird 21-Jan-86 13:57 
       (prog (old-context subgoal)
	     (setq old-context (car context-stack))
	     (cond ((cdr context-stack)
		    (soarmapcar #'process-sub-contexts (cdr context-stack))))
	     (setq subgoal (get-current old-context 'goal))
	     (soarmapc #'remove-from-wm (get subgoal 'non-results))
	     (soarmapc #'(lambda (x)
				 (cond ((eq subgoal (get (wme-id x)
							 'in-id-field))
					(soarclearprops (wme-id x)))))
		       (get subgoal 'non-results))
	     (soarclearprops subgoal)))




(defun recompute-preferences (context) ; edited:  1-May-86 11:13 
       (prog (goal preferences)
	     (setq goal (get-current context 'goal))
	     (setq preferences (append (get goal 'all-preferences)
				       (get (top-goal)
					    'nil-goal-preferences)))
	     (remprop goal 'preferences)
	     (soarmapc #'(lambda (x)
				 (test-and-save-context-preference context goal
								   (car x)
								   (cdr x)))
		       preferences)))




(defun save-preference (wme) ; edited:  1-May-86 16:54 
       (prog (preference)
	     (cond ((and (eq (wme-value wme)
			     'indifferent)
			 (not (wme-reference wme)))
		    (setq preference (list 'unary-indifferent (wme-id wme)
					   nil)))
		   ((and (eq (wme-value wme)
			     'parallel)
			 (not (wme-reference wme)))
		    (setq preference (list 'unary-parallel (wme-id wme)
					   nil)))
		   ((eq (wme-value wme)
			'better)
		    (setq preference (list 'worse (wme-reference wme)
					   (wme-id wme))))
		   ((soarmemq (wme-value wme)
			      *necessity-preference-values*)
		    (setq preference (list (wme-value wme)
					   (wme-id wme)
					   (wme-reference wme)
					   wme)))
		   (t (setq preference (list (wme-value wme)
					     (wme-id wme)
					     (wme-reference wme)))))
	     (cond ((null (wme-goal wme))
		    (soaraddprop (top-goal)
				 (cons wme preference)
				 'nil-goal-preferences)))
	     (save-preference-with-goal *context-stack* wme preference)))




(defun save-preference-with-goal (context-stack wme preference) ; edited: 
 ;  1-May-86 11:18 
       (prog (context goal)
	     (setq context (car context-stack))
	     (setq goal (get-current context 'goal))
	     (cond ((eq goal (wme-goal wme))
		    (soaraddprop goal (cons wme preference)
				 'all-preferences)
		    (test-and-save-context-preference context goal wme 
						      preference)
		    (return))
		   ((null (wme-goal wme))
		    (test-and-save-context-preference context goal wme 
						      preference)))
	     (soarmapc #'(lambda (x)
				 (save-preference-with-goal x wme preference))
		       (cdr context-stack))))




(defun test-and-save-context-preference (context goal wme preference) ; 
 ; Randy.Gobbel 15-Jul-86 17:50 
       (prog (problem-space state operator slot slot-preferences 
			    value-preferences)
	     (setq problem-space (wme-problem-space wme))
	     (setq state (wme-state wme))
	     (setq operator (wme-operator wme))
	     (cond ((and (or (null problem-space)
			     (eq problem-space (get-current context
							    'problem-space)))
			 (or (null state)
			     (eq state (get-current context 'state)))
			 (or (null operator)
			     (eq operator (get-current context 'operator))))
		    (setq slot (wme-attribute wme))
		    (setq slot-preferences
			  (soarassq slot (get (get-current context
							   'goal)
					      'preferences))) ; Set 
 ; context-preference-change flag to t 
		    (rplaca (soarnthcdr 4 context)
			    t)
		    (cond ((null slot-preferences)
			   (soaraddprop goal (list slot (list (car preference)
							      (cdr preference)))
					'preferences))
			  (t (setq value-preferences
				   (soarassq (car preference)
					     (cdr slot-preferences)))
			     (cond ((null value-preferences)
				    (rplacd slot-preferences
					    (cons (list (car preference)
							(cdr preference))
						  (cdr slot-preferences))))
				   (t (rplacd value-preferences
					      (cons (cdr preference)
						    (cdr value-preferences))))))
			  )))))




(defun test-context (context super-context) ; John.Laird 21-Apr-86 10:11  ; 
 ; John.Laird 22-Jan-86 14:55 
       (and (get-context-pnumber context)
	    (not (test-suspended-context context super-context))))





(defun test-if-subset (subset set) ; John.Laird 14-May-86 11:43  ; 
 ; John.Laird 6-Jun-85 11:32 
       (cond ((null subset)
	      t)
	     ((atom set)
	      nil)
	     ((soarmember (car subset)
			  set)
	      (test-if-subset (cdr subset)
			      set))
	     (t nil)))




(defun test-results-subset (result context context-stack) ; Randy.Gobbel 
 ;  2-Dec-86 16:03  ; Return t if result is a subset of the results saved 
 ; for the contexts  ; If *impasse-subset-not-equal* then the results must 
 ; be equal - not just a subset 
       (cond (*impasse-subset-not-equal* (equal result
						(get-context-results context)))
	     ((cond ((not (soarlistp (get-context-results context)))
		     (eq result (get-context-results context)))))
	     ((soarlistp result)
	      (cond ((test-if-subset result (get-context-results context)) ; 
 ; Remove any items that are no longer in the impasse 
		     (remove-items (soardifference (get-context-results context)
						   result)
				   (caadr context-stack))
		     (rplaca (soarnthcdr 7 context)
			     result)
		     t)
		    (t nil)))
	     ((soarmemq result (get-context-results context))
	      (remove-items (soardifference (get-context-results context)
					    (list result))
			    (caadr context-stack))
	      (rplaca (soarnthcdr 7 context)
		      result)
	      t)
	     (t nil)))




(defun test-slot-subset (result context) ; John.Laird 6-May-85 11:41 
       (cond ((test-if-subset result (get-current context 'operator))
	      (rplaca (soarnthcdr 3 context)
		      result)
	      t)
	     (t nil)))




(defun test-suspended-context (context super-context) ; Randy.Gobbel 
 ; 26-Mar-86 20:44  ; John.Laird 22-Jan-86 16:01 
       (and (not *parallel-not-suspend*)
	    (or (not (get-context-difficulty super-context))
		(eq (get-context-difficulty super-context)
		    'no-change))
	    (or (not (get-context-slot super-context))
		(eq (get-context-slot super-context)
		    'operator))
	    (soarlistp (get-current super-context 'operator))
	    (neq (get-context-super-operator context)
		 (car (get-current super-context 'operator)))))




;;; Concatenated from type module "chunk" module-version "release".

;;; Concatenated from file "/usr/milnes/soar/soar4-4/src/chunk/release/chunk.lisp".
;;; -*-mode: lisp; package: user -*-
;;;  Chunk.lisp

(defun learning-test (context) ; Randy.Gobbel 16-Jul-86 16:30 
       (or (and (not *always-learn*)
		(get-context-subgoaled context))
	   (not *learning*)
	   (soarmemq (get (get-current context 'problem-space)
			  'name)
		     *chunk-free-problem-spaces*)))



(defun soargenpname (x) ; edited:  4-Feb-86 08:54  ; USE AN OLD PRODUCTION 
 ; NAME IF ONE IS AVAILABLE, OTHERWISE GENERATE A NEW ONE 
       (prog (sym)
	     (cond (*free-pname-list* (setq sym (pop *free-pname-list*)))
		   (t (setq sym (soargensymbol (soarnthchar x 1)))))
	     (soarputprop sym t 'gensymed)
	     (return sym)))




(defun untest (element) ; Randy.Gobbel 13-Jun-86 14:35 
       (cond ((and element (variablep (car element)))
	      (soarputprop (car element)
			   (1- (get (car element)
				    'tested))
			   'tested)
	      (untest (cdr element)))
	     (element (untest (cdr element)))))




(defun build-copies (p-list wme-list) ; John.Laird 19-Jun-86 17:35 
       (cond (p-list (build-a-production (caar p-list)
					 (rebind-action-ids
					   (cadar p-list)
					   (find-constant-ids (cadar p-list)))
					 wme-list)
		     (build-copies (cdr p-list)
				   wme-list))))

(defun
  build-duplicates
  (action-list class-list duplicate-variable duplicates negations 
	       save-condition save-action) ; Randy.Gobbel 11-Sep-86 15:04 
  (prog
    (cond-list condition class condition-list)
    (cond ((null duplicates)
	   (return (list (list (append save-condition negations)
			       save-action)))))
    (setq action-list (prune-actions save-action duplicate-variable))
    (cond ((null action-list)
	   (return (list (list (append save-condition negations)
			       save-action)))))
    (setq condition-list nil)
    (soarwhile class-list (setq class (pop class-list))
	       (setq cond-list (get class 'condition))
	       (remprop class 'condition)
	       (soarwhile cond-list (setq condition (pop cond-list))
			  (cond ((not (soarassq condition duplicates))
				 (soarpush condition condition-list)))))
    (setq condition-list (append (nreverse condition-list)
				 negations))
    (return
      (cons (list condition-list action-list)
	    (soarmapcar
	      #'(lambda (x)
			(list (append (soar-copy condition-list)
				      (soarmapcar #'cdr (soar-copy action-list))
				      (list (car x)))
			      (list (cdr x))))
	      duplicates)))))





(defun
  compactify-conditions
  (condition-list negations action-list) ; Randy.Gobbel 28-Jul-86 14:00 
  (prog
    (class-list class-name same-class condition duplicates duplicate-variable 
		sclass save-condition save-action)
    (setq duplicates nil)
    (setq save-condition condition-list)
    (setq save-action action-list)
    (setq class-list nil)
    (setq duplicate-variable nil)
    (soarwhile
      condition-list
      (setq condition (pop condition-list))
      (setq class-name (wme-class condition))
      (and (not (soarmemq class-name class-list))
	   (remprop class-name 'condition))
      (cond ((soarlistp class-name)
	     (soaraddprop 'conj condition 'condition)
	     (and (not (soarmemq 'conj class-list))
		  (soarpush 'conj class-list)))
	    (t (setq same-class (get class-name 'condition))
	       (setq *suspected-duplicates* nil)
	       (soarwhile (and same-class (not *suspected-duplicates*))
			  (setq sclass (pop same-class))
			  (cond ((compare-conditions condition sclass)
				 (and (not (soarassq sclass duplicates))
				      (soarpush (cons sclass
						      (dup-action sclass 
								action-list))
						duplicates))
				 (setq duplicate-variable
				       (append duplicate-variable 
					       *suspected-duplicates*)))
				(t (setq *suspected-duplicates* nil))))
	       (cond ((not *suspected-duplicates*)
		      (soaraddprop class-name condition 'condition)
		      (and (not (soarmemq class-name class-list))
			   (soarpush class-name class-list)))))))
    (return (build-duplicates action-list (reverse class-list)
			      duplicate-variable duplicates negations 
			      save-condition save-action))))





(defun compare-conditions (elm1 elm2) ; Randy.Gobbel 11-Sep-86 15:23 
       (cond ((and (null elm1)
		   (null elm2))
	      t)
	     ((or (null elm1)
		  (null elm2))
	      nil)
	     ((eqp (car elm1)
		   (car elm2))
	      (compare-conditions (cdr elm1)
				  (cdr elm2)))
	     ((and (eqp (get (car elm1)
			     'tested)
			1)
		   (eqp (get (car elm2)
			     'tested)
			1))
	      (soarpush (car elm1)
			*suspected-duplicates*)
	      (soarpush (car elm2)
			*suspected-duplicates*)
	      (compare-conditions (cdr elm1)
				  (cdr elm2)))
	     (t nil)))




(defun add-negations (current-goal data-matched) ; John.Laird 
 ;  9-Jun-86 20:35 
       (prog (var-list)
	     (cond ((or (null *p-name*)
			(null (get *p-name* 'negation-index)))
		    (return nil)))
	     (setq var-list (caddr (get *p-name* 'negation-index)))
	     (return (append (instantiate-remaining-conditions
			       (car (get *p-name* 'negation-index))
			       current-goal var-list data-matched '-)
			     (instantiate-remaining-conditions
			       (cadr (get *p-name* 'negation-index))
			       current-goal var-list data-matched '*)))))




(defun nlam-back-trace (objects) ; Randy.Gobbel 12-Sep-86 12:06 
       (prog (wme goal)
	     (cond (*never-learn* (traceprintc 
			      "Learning was disabled.  Can not back-trace.")
				  (traceprint " ")
				  (return)))
	     (cond ((and (not (numberp (car objects)))
			 (get (car objects)
			      'goal-depth))
		    (setq goal (pop objects)))
		   ((null objects)
		    (setq goal (most-recent-subgoal *context-stack*))))
	     (cond ((null objects)
		    (soarmapcar #'(lambda (x)
					  (back-trace-conditions goal x t))
				(get goal 'results)))
		   (t (setq wme (conv-input-wme (pop objects)))
		      (setq goal (cond ((null objects)
					(most-recent-subgoal *context-stack*))
				       (t (car objects))))
		      (back-trace-conditions goal wme t)))
	     (traceprint " ")))

(defun
  back-trace-conditions
  (current-goal conditions trace-flag) ; Randy.Gobbel 17-Nov-86 15:07  ; 
 ; USING THE PRODUCTION TRACES FOR THE CURRENT-GOAL - PERFORM A DEPENDENCY 
 ; ANALYSIS TO DETERMINE THE CONDITIONS OF THE A CHUNK 
  (prog (grounded-potentials)
	(cond (conditions
		(cond (trace-flag (traceprint " ")
				  (traceprinc 
				      "Backtracing to determine conditions")
				  (traceprinc " for goal: ")
				  (traceprint current-goal)
				  (traceprint 
				    "Working-memory elements being traced:")
				  (soarmapc #'pplinet (p-to-sp conditions))
				  (traceprint " ")
				  (traceprinc 
			       "Productions and conditions traced through:")))
		(setq *previously-traced* nil)
		(setq *potential-conditions* nil)
		(setq conditions (back-trace-production-conditions
			current-goal trace-flag (get current-goal
						     'production-trace)
			conditions nil 1)) ; GROUNDED-POTENTIALS ARE WME 
 ; THAT ARE RESULTS OF THE SUBGOAL BUT FOR WHICH THERE WAS NOT A DIRECT 
 ; ACCESS PATH FROM OTHER RESULTS OR SUPERSTRUCTURE                      
 		(setq grounded-potentials (handle-potentials conditions))
 		(cond ((or grounded-potentials *potential-conditions*)
		       (setq conditions (append grounded-potentials conditions))
			(cond (trace-flag (traceprint " ") 
			      (traceprint "Potentials that become conditions:" )
		             (soarmapc #'(lambda (x)
					   (trace-new-condition x trace-flag 1))
				 grounded-potentials)
			      (traceprint " ") 
			      (traceprint "Traceback through ungrounded potentials:" )))
		       (setq conditions
			     (append (back-trace-production-conditions
				       current-goal trace-flag
				       (get current-goal
					    'production-trace)
				       *potential-conditions* nil 1)
				     conditions))))
		(return conditions)))))


(defun back-trace-production-conditions (current-goal trace-flag 
						      production-traces 
						      intermediate-wmes 
						      conditions depth) ; 
 ; Randy.Gobbel 12-Sep-86 12:05 
       (prog (wme production-trace)
	     (cond ((null intermediate-wmes)
		    (return conditions)))
	     (setq wme (pop intermediate-wmes))
	     (cond ((eq wme 'mark)
		    (setq depth (1- depth))
		    (go l0)))
	     (setq production-trace (find-back-production wme production-traces)
		   )
	     (cond ((null production-trace)
		    (go l0)))
	     (cond ((soarmemq (car production-trace)
			      *previously-traced*)
		    (go l0)))
	     (soarpush (car production-trace)
		       *previously-traced*) ; PRINT PRODUCTION NAME BEING 
 ; TRACED 
	     (cond (trace-flag (do-tabto (times 3 depth)
					 (trace-file))
			       (traceprinc (caddr production-trace))
			       (traceprinc ": ")
			       (pplinet (car (p-to-sp wme))))) ; PRINT 
 ; THOSE WME THAT WERE RESULTS OR PREEXISTING STRUCTURE AND WILL BE 
 ; CONDITIONS 
	     (soarmapc #'(lambda (x)
				 (trace-new-condition x trace-flag depth))
		       (soardifference (caadr production-trace)
				       conditions))
	     (setq conditions (union-with-negations conditions
						    (caadr production-trace)))
	     (setq intermediate-wmes (union (cadadr production-trace)
					    (cons 'mark intermediate-wmes)))
	     (setq *potential-conditions*
		   (union *potential-conditions*
			  (car (cddadr production-trace))))
	     (setq depth (1+ depth))
	     l0
	     (return (back-trace-production-conditions current-goal trace-flag 
						       production-traces 
						       intermediate-wmes 
						       conditions depth))))





(defun build-a-production (condition-list action-list wme-list) ; 
 ; Randy.Gobbel 12-Sep-86 12:06 
       (prog (new-chunk)
	     (cond ((null action-list)
		    (soarwarn 
			 "No chunk was built because there were no actions"
			      " ")
		    (return nil))
		   ((> (length condition-list)
		       *max-chunk-conditions*)
		    (soarwarn 
	  "No chunk was built because *max-chunk-conditions* was exceeded:"
			      (length condition-list))
		    (return nil)))
	     l1
	     (setq *p-name* (soargenpname 'p))
	     (cond ((soarmemq *p-name* *pnames*)
		    (go l1)))
	     (setq *unbound* nil)
	     (setq condition-list (nreverse (knotify-conditions
					      (re-order-conditions 
							     condition-list))))
	     (and *unbound* (setq condition-list (re-order-conditions 
							     condition-list)))
	     (setq new-chunk (list (list 'quote *p-name*)
				   (list 'quote 'elaborate)
				   (list 'quote
					 (nconc (nconc condition-list
						       (list '-->))
						(test-connected-actions 
								action-list)))))
	     (cond ((cdrmember new-chunk *new-chunks*)
		    (and *print-learn* (traceprintc "Duplicate chunk"))
		    (return t)))
	     (and *tracep* (soarpush *p-name* *tracep-list*))
	     (soarpush *p-name* *chunks*)
	     (soarpush new-chunk *new-chunks*)
	     (soarpush wme-list *wme-list-stack*)
	     (return t)))





(defun build-terse (conditions) ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (return-conditions change-flag element)
	     (and *ltrace* (traceprint "Conditions that are tersed out: "))
	     (setq change-flag t)
	     (soarwhile change-flag (setq change-flag nil)
			(setq return-conditions nil)
			(soarwhile
			  conditions
			  (setq element (pop conditions))
			  (cond ((and (neq (wme-class element)
					   'preference)
				      (free-condition (wme-value element)))
				 (setq change-flag t)
				 (and *ltrace* (pplinet (car (p-to-sp element)))
				      )
				 (untest (cdr element)))
				((and (eq (wme-class element)
					  'preference)
				      (free-condition (wme-class element)))
				 (setq change-flag t)
				 (and *ltrace* (pplinet (car (p-to-sp element)))
				      )
				 (untest (cdr element)))
				(t (soarpush element return-conditions))))
			(setq conditions (nreverse return-conditions)))
	     (and *ltrace* (traceterpri))
	     (return conditions)))

(defun build-variable-list (element) ; Randy.Gobbel 11-Sep-86 15:09 
       (and element (symbolp element)
	    (get element 'id-test)
	    (not (soarassq element *learn-ids*))
	    (setq *learn-ids* (cons (cons element (soargenvar element))
				    *learn-ids*))))

(defun change-non-results-to-results (id current-goal goal) 
 ; John Laird April 20th 87
 ; Find all of the non-results for the current-goal 
 ; that become results because id is now from the supergoal 
       (prog (supergoal non-results non-result new-results)
	     (setq supergoal (or (car (get current-goal 'supergoals)) goal)) 
	     (and supergoal (soarputprop id supergoal 'in-id-field))
	     (setq non-results (get current-goal 'non-results))
	     l1
	     (cond ((null non-results)
		    (soarmapc #'(lambda (x)
					(process-subgoal-result x current-goal))
			      new-results)
		    (return)))
	     (setq non-result (car non-results))
	     (cond ((or (and (eq (car non-result)
				 'preference)
			     (closure-preference non-result current-goal))
			(and (neq (car non-result)
				  'preference)
			     (eq (wme-id non-result)
				 id)))
		    (pop non-results)
		    (soarputprop current-goal (remove non-result
						      (get current-goal
							   'non-results))
				 'non-results)
		    (soarpush non-result new-results))
		   (t (pop non-results)))
	     (go l1)))


(defun clean-up-action (action) ; John.Laird 25-Mar-85 17:05 
       (cons 'make (clean-up-clause action 'action)))

(defun clean-up-clause (condition c-or-a) ; John.Laird 16-Jun-86 17:19 
       (prog (new-condition item element)
	     (cond ((is-negation condition)
		    (return condition))
		   ((soarlistp (car condition))
		    (return (soarmapcar #'(lambda (x)
						  (clean-up-clause x c-or-a))
					condition))))
	     (setq new-condition (list (pop condition)))
	     (soarwhile condition (setq item (pop condition))
			(cond ((setq element (soarassq item *learn-ids*))
			       (setq item (cdr element))
			       (cond ((eq c-or-a 'condition)
				      (soarputprop item
						   (1+ (or (get item
								'tested)
							   0))
						   'tested)))))
			(soarpush item new-condition))
	     (return (reverse new-condition))))





(defun clear-var-list nil  ; John.Laird  4-Dec-84 09:14 
       (setq *learn-ids* nil)
       (soarmapc #'soarclearprops *used-var-list*)
       (soarmapc #'(lambda (x)
			   (soarputprop x 1 'var-count))
		 *used-char-list*)
       (setq *used-var-list* nil))





(defun closure-preference (wme subgoal) ; Randy.Gobbel 21-Apr-86 18:03 
       (and (test-preference-field (wme-goal wme)
				   subgoal)
	    (test-preference-field (wme-problem-space wme)
				   subgoal)
	    (test-preference-field (wme-state wme)
				   subgoal)
	    (test-preference-field (wme-operator wme)
				   subgoal)))





(defun create-production nil  ; Randy.Gobbel 12-Sep-86 12:08 
       (prog (chunk wme-list)
	     (and (null *new-chunks*)
		  (return))
	     (start-build-time)
	     (setq *new-chunks* (nreverse *new-chunks*))
	     (setq *wme-list-stack* (nreverse *wme-list-stack*))
	     (soarwhile *new-chunks* (setq chunk (pop *new-chunks*))
			(setq wme-list (pop *wme-list-stack*))
			(eval (cons 'compile-production chunk))
			(cond ((eqp *print-learn* 1)
			       (traceterpri)
			       (eval (list 'spm (cadar chunk))))
			      (*print-learn* (traceprintc "Build:")
					     (traceprinc (cadar chunk))))
			(refract-new-p chunk wme-list))
	     (setq *wme-list-stack* nil)
	     (stop-build-time)))





(defun find-action-closure (listx) ; John.Laird 18-Apr-85 10:21 
       (soarmapcar #'(lambda (x)
			     (cdr (soarassq x *learn-ids*)))
		   (find-closure listx)))





(defun find-back-production (action traces) ; Randy.Gobbel 12-Sep-86 12:06 
       (cdr (cond ((eq (wme-class action)
		       'goal)
		   (soarassoc action traces))
		  ((soarassq action traces))
		  (t (traceprint "ERROR")
		     (traceprint action)))))





(defun find-closure (listx) ; John.Laird 17-Jun-86 09:12 
       (cond ((null listx)
	      listx)
	     ((is-negation (car listx))
	      (find-closure (cdr listx)))
	     ((soarlistp (caar listx))
	      (union (find-closure (car listx))
		     (find-closure (cdr listx))))
	     (t (union (cdar listx)
		       (find-closure (cdr listx))))))





(defun find-constant-ids (action-list) ; John.Laird 12-Jun-86 14:59 
       (prog (action-ids id action)
	     (setq action-ids nil)
	     (soarwhile action-list (setq action (pop action-list))
			(setq id (wme-id (cdr action)))
			(cond ((and (not (variablep id))
				    (not (soarassq id action-ids)))
			       (soarpush (cons id (soargenvar id))
					 action-ids))))
	     (return action-ids)))





(defun flattop (l) ; Randy.Gobbel 17-Nov-86 14:36  ; Turn a list-of-lists 
 ; into a list (one level shallower) 
       (cond ((null l)
	      l)
	     (t (append (car l)
			(flattop (cdr l))))))





(defun free-condition (id) ; Randy.Gobbel 11-Sep-86 17:59 
       (and id (not (numberp id))
	    (eqp (get id 'tested)
		 1)
	    (not (soarmemq id *action-closure*))))





(defun get-cav-value (class attribute) ; John.Laird 18-Apr-85 10:21 
       (cdr (soarassq attribute (cdr (soarassq class *cav*)))))





(defun get-cavi-id (class attribute value) ; John.Laird 18-Apr-85 10:22 
       (cdr (soarassq value (cdr (soarassq attribute
					   (cdr (soarassq class *cavi*)))))))



(defun handle-back-trace-actions (goal) ; Randy.Gobbel  2-Sep-86 16:20 
       (prog (condition-list negation-list wme-list action-list supergoal 
			     context)
	     (clear-var-list)
	     (cond ((get goal 'supergoals)
		    (setq supergoal (car (get goal 'supergoals))))
		   (t (return)))
	     (setq action-list (get goal 'one-time-results))
	     (setq wme-list (back-trace-conditions goal action-list *ltrace*))
	     (setq *p-name* goal)
	     (setq context (get goal 'context))
	     (setq *first-action* t)
	     (setq *data-matched* (reverse wme-list))
	     (soarmapc #'(lambda (x)
				 (save-production-trace x supergoal))
		       action-list)
	     (cond ((learning-test context)
		    (return)))
	     (soarmapc #'build-variable-list (find-closure wme-list))
	     (setq *action-closure* (find-action-closure action-list))
	     (setq condition-list
		   (soarmapcar #'(lambda (x)
					 (clean-up-clause x 'condition))
			       wme-list))
	     (soarmapc #'build-variable-list (find-closure action-list))
	     (setq action-list (soarmapcar #'clean-up-action action-list))
	     (setq condition-list (process-negations condition-list))
	     (setq negation-list (cdr condition-list))
	     (setq condition-list (build-terse (car condition-list)))
	     (cond ((test-chunk-for-content condition-list)
		    (and *ltrace* (soarmapc #'pplinet condition-list)))
		   (t (set-chunk-bit-t goal)
		      (build-copies (compactify-conditions condition-list 
							   negation-list 
							   action-list)
				    wme-list)))
	     (clear-var-list)))





(defun handle-potentials (conditions) ; Randy.Gobbel 17-Nov-86 15:09 
       (prog (condition potential results id potential-conds)
	     (setq results nil)
	     (soarwhile (and conditions *potential-conditions*)
			(setq condition (pop conditions))
			(cond ((is-negation condition)
			       (setq condition (pop conditions))))
			(cond ((eq (wme-class condition)
				   'preference)
			       (setq id (wme-object condition)))
			      (t (setq id (wme-value condition))))
			(setq potential-conds *potential-conditions*)
			(setq *potential-conditions* nil)
			(soarwhile potential-conds (setq potential
							 (pop potential-conds))
				   (cond ((eq 'preference (wme-class potential))
					    (cond ((or (not (wme-goal potential))
						       (eq (wme-id condition)
								 (wme-goal potential))) 
						   (soarpush potential results)
						   (setq conditions
							(append conditions
								(list potential))))
					   (t (soarpush potential 
						     *potential-conditions*))))
					  ((eq id (wme-id potential))
					   (soarpush potential results)
					   (setq conditions
						(append conditions
							(list potential))))
					  (t (soarpush potential 
						     *potential-conditions*)))))
	     (return results)))

(defun knotify (val variables) ; John.Laird  9-Jun-86 09:31 
       (prog (result)
	     (setq result '({))
	     (soarwhile variables (setq result (append result
						       (list '<>
							     (car variables))))
			(pop variables))
	     (cond ((eq val '<unbound>)
		    (setq *unbound* t)
		    (return (append result '(}))))
		   (t (return (append result (list val '})))))))





(defun
  knotify-conditions
  (conditions) ; John.Laird 19-Jun-86 10:50 
  (prog
    (condition attribute return-conditions class value values id ids idds 
	       variable-list variable-pairs)
    (setq variable-list (setq return-conditions
			      (setq *cav* (setq *cavi*
						(setq variable-pairs nil)))))
    (soarwhile
      conditions
      (setq condition (pop conditions))
      (cond ((is-negation condition)
	     (soarpush condition return-conditions)
	     (soarpush (pop conditions)
		       return-conditions))
	    ((eq (wme-class condition)
		 'preference)
	     (soarpush condition return-conditions))
	    (t (setq class (wme-class condition))
	       (cond ((> (length condition)
			 2)
		      (setq id (wme-id condition))
		      (setq attribute (cond ((variablep (wme-attribute 
								  condition))
					     'variable)
					    (t (wme-attribute condition))))
		      (setq value (cond ((> (length condition)
					    3)
					 (wme-value condition))
					(t 'unbound)))))
	       (cond ((and (> (length condition)
			      3)
			   (variablep value))
		      (setq values (get-cav-value class attribute))
		      (cond ((null values)
			     (put-cav-attribute class attribute)
			     (put-cav-value class attribute (list value))
			     (and (not (soarmemq value variable-list))
				  (soarpush value variable-list)))
			    ((and (not (soarmemq value values))
				  (not (soarmemq value variable-list)))
			     (put-cav-value class attribute (cons value values))
			     (rplaca (cdddr condition)
				     (knotify value values))
			     (soarpush (cons value values)
				       variable-pairs)
			     (setq variable-list (cons value variable-list)))
			    ((not (soarmemq value values))
			     (put-cav-value class attribute (cons value values))
			     ))))
	       (cond ((> (length condition)
			 2)
		      (setq ids (get-cavi-id class attribute value))
		      (setq idds (cons id ids))
		      (cond ((null ids)
			     (put-cavi-attribute class attribute)
			     (put-cavi-value class attribute value)
			     (put-cavi-id class attribute value idds))
			    ((and (not (soarmemq id ids))
				  (not (soarassoc id variable-pairs)))
			     (put-cavi-id class attribute value idds)
			     (soarpush idds variable-pairs)
			     (rplaca (cdr condition)
				     (knotify id ids))))))
	       (soarpush condition return-conditions))))
    (setq *cav* (setq *cavi* nil))
    (return return-conditions)))





(defun nlam-learn (z) ; Randy.Gobbel 12-Sep-86 12:08 
       (cond ((atom z)
	      (print-status))
	     (t (cond ((eq (car z)
			   'on)
		       (cond ((and *never-learn* (not (eqp *cycle-count* 0)))
			      (traceterpri)
			      (traceprint 
		   "Error: Can not turn learning on.  Must init-soar first"))
			     (t (setq *learning* t)
				(setq *never-learn* nil))))
		      ((eq (car z)
			   'off)
		       (cond ((and *never-learn* (not (eqp *cycle-count* 0)))
			      (traceterpri)
			      (traceprint 
	       "Error: Can not turn learning to off.  Must init-soar first"))
			     (t (setq *learning* nil)
				(setq *never-learn* nil))))
		      ((eq (car z)
			   'never)
		       (setq *learning* nil)
		       (setq *never-learn* t))
		      ((eq (car z)
			   'full-print)
		       (setq *print-learn* 1))
		      ((eq (car z)
			   'print)
		       (setq *print-learn* 0))
		      ((eq (car z)
			   'noprint)
		       (setq *print-learn* nil))
		      ((eq (car z)
			   'full-trace)
		       (setq *ltrace* t)
		       (setq *tracep* t))
		      ((eq (car z)
			   'trace)
		       (setq *tracep* t)
		       (setq *ltrace* nil))
		      ((eq (car z)
			   'notrace)
		       (setq *tracep* nil)
		       (setq *ltrace* nil))
		      ((eq (car z)
			   'all-goals)
		       (setq *always-learn* t))
		      ((eq (car z)
			   'bottom-up)
		       (setq *always-learn* nil))
		      (t (traceterpri)
			 (traceprint "Error: unknown option")))
		(eval (cons 'learn (cdr z))))))





(defun negated-chunk-condition-test (wme current-goal) ; John.Laird 
 ; 16-Jun-86 15:34 
       (or (and (neq (wme-class wme)
		     'preference)
		(wme-id wme)
		(get (wme-id wme)
		     'in-id-field)
		(neq current-goal (get (wme-id wme)
				       'in-id-field)))
	   (and (eq (wme-class wme)
		    'preference)
		(closure-preference wme current-goal))))





(defun process-negations (conditions) ; John.Laird 13-Jun-86 11:43 
       (prog (return-conditions negations foralls)
	     (setq negations nil)
	     (setq foralls nil)
	     (soarwhile conditions (cond ((eq (car conditions)
					      '-)
					  (pop conditions)
					  (soarpush (pop conditions)
						    negations))
					 ((eq (car conditions)
					      '*)
					  (pop conditions)
					  (soarpush (pop conditions)
						    foralls))
					 (t (soarpush (pop conditions)
						      return-conditions))))
	     (setq negations (variablize-negations negations))
	     (setq foralls (variablize-negations foralls))
	     (return (cons (nreverse return-conditions)
			   (append (nreverse (tersify-negations negations
								'-))
				   (nreverse (tersify-negations foralls
								'*)))))))

(defun process-subgoal-result (wme current-goal) ; John.Laird April 20th.
       (prog (class id attribute value goal)
	     (setq class (wme-class wme))
	     (setq id (wme-id wme))
	     (cond ((and id (symbolp id))
		    (setq goal (get id 'in-id-field)))
		   (t (setq goal nil)))
	     (cond ((and (eq class 'preference)
			 (eq goal current-goal)
			 id)
		    (change-non-results-to-results id current-goal goal)))
	     (setq value (wme-value wme))
	     (cond ((and (neq class 'preference)
			 value
			 (symbolp value)
			 (or (not (get value 'in-id-field))
			     (eq (get value 'in-id-field)
				 current-goal)))
		    (change-non-results-to-results value current-goal goal)))
	     (setq attribute (wme-attribute wme))
	     (cond ((and (neq class 'preference)
			 attribute
			 (symbolp attribute)
			 (or (not (get attribute 'in-id-field))
			     (eq (get attribute 'in-id-field)
				 current-goal)))
		    (change-non-results-to-results attribute current-goal goal)))
;	     (save-wme-as-result-or-non-result wme (or (car (get current-goal
;							     'supergoals)) goal))
;John's fix for this bug 06/03/87 11:57:08 [au]
;Replace the call to save-wme-as-result-or-non-result in process-subgoal-result
;with the following (just to make it very clear):
	     (cond ((get current-goal 'supergoals)
		    (save-wme-as-result-or-non-result wme 
						      (car (get current-goal 'supergoals))))
		   ((not (eq current-goal goal)) (save-wme-as-result-or-non-result wme goal))
		   )
	     (soaraddprop current-goal wme 'one-time-results)))

(defun prune-actions (action-list duplicate-variable) ; John.Laird 
 ; 25-Mar-85 17:29 
       (prog (action save-action return-list)
	     (setq return-list nil)
	     l1
	     (and (null action-list)
		  (return return-list))
	     (setq action (pop action-list))
	     (setq save-action action)
	     l2
	     (cond ((null action)
		    (soarpush save-action return-list)
		    (go l1))
		   ((soarmemq (pop action)
			      duplicate-variable)
		    (go l1)))
	     (go l2)))





(defun put-cav-attribute (class attribute) ; John.Laird 18-Apr-85 10:23 
       (cond ((soarassq class *cav*)
	      (rplacd (soarassq class *cav*)
		      (cons (list attribute)
			    (cdr (soarassq class *cav*)))))
	     (t (setq *cav* (cons (cons class (list (list attribute)))
				  *cav*)))))





(defun put-cav-value (class attribute value) ; John.Laird 18-Apr-85 10:23 
       (rplacd (soarassq attribute (cdr (soarassq class *cav*)))
	       value))





(defun put-cavi-attribute (class attribute) ; John.Laird 18-Apr-85 10:24 
       (cond ((soarassq attribute (cdr (soarassq class *cavi*))))
	     ((soarassq class *cavi*)
	      (rplacd (soarassq class *cavi*)
		      (cons (list attribute)
			    (cdr (soarassq class *cavi*)))))
	     (t (setq *cavi* (cons (cons class (list (list attribute)))
				   *cavi*)))))





(defun put-cavi-id (class attribute value id) ; John.Laird 18-Apr-85 10:24 
       (rplacd (soarassq value (cdr (soarassq attribute
					      (cdr (soarassq class *cavi*)))))
	       id))





(defun put-cavi-value (class attribute value) ; John.Laird 18-Apr-85 10:25 
       (rplacd (soarassq attribute (cdr (soarassq class *cavi*)))
	       (cons (list value)
		     (cdr (soarassq attribute (cdr (soarassq class *cavi*)))))))





(defun rebind-action-ids (action-list copy-list) ; John.Laird 
 ; 18-Apr-85 10:25 
       (prog (element new-element return-list val val2)
	     (setq return-list nil)
	     l1
	     (and (null action-list)
		  (return return-list))
	     (setq element (pop action-list))
	     (setq new-element nil)
	     l2
	     (and (null element)
		  (go l3))
	     (setq val2 (pop element))
	     (setq val (cdr (soarassq val2 copy-list)))
	     (or val (setq val val2))
	     (soarpush val new-element)
	     (go l2)
	     l3
	     (soarpush (nreverse new-element)
		       return-list)
	     (go l1)))





(defun refract-new-p (new-p wme-list) ; Randy.Gobbel 16-Jul-86 16:45 
       (prog (element remaining-elements)
	     (setq remaining-elements nil)
	     (setq new-p (cadar new-p))
	     (soarwhile
	       (setq element (soarassq new-p *conflict-set*))
	       (cond ((or (null wme-list)
			  (> (get (find-max-goal-depth (cdr element))
				  'goal-depth)
			     (get (find-max-goal-depth wme-list)
				  'goal-depth))
			  (test-if-subset (cdr element)
					  wme-list))
		      (setq *conflict-set* (dremove element *conflict-set*)))
		     (t (setq *conflict-set* (dremove element *conflict-set*))
			(soarpush element remaining-elements))))
	     (setq *conflict-set* (append *conflict-set* remaining-elements))))





(defun save-production-trace (wme current-goal) ; John.Laird 
 ; 17-Mar-86 13:21  ; Save a production trace in the production-trace 
 ; property of the current-goal. The trace has a trace-number, the 
 ; data-matched - split into results and non-results, and the production 
 ; name 
       (cond (*data-matched*
	       (cond (*first-action*
		       (setq *current-production-trace*
			     (list (setq *trace-number* (1+ *trace-number*))
				   (split-apart-conditions (reverse 
							     *data-matched*)
							   current-goal)
				   *p-name*))
		       (setq *first-action* nil)))
	       (soaraddprop current-goal (cons wme *current-production-trace*)
			    'production-trace)
	       wme)))





(defun save-wme-as-result-or-non-result (wme current-goal) ; John.Laird 
 ; 16-May-86 14:58  ; Determine if a just created wme is a result or not. 
 ; If it is a result, then it might influence the status of other wmes, so 
 ; change-non-results-to-results is called 
       (prog (goal class id)
	     (and (null current-goal)
		  (return))
	     (setq class (wme-class wme))
	     (setq id (wme-id wme))
	     (cond (id (setq goal (get id 'in-id-field)))
		   (t (setq goal nil)))
	     (cond ((or (and (eq class 'preference)
			     (closure-preference wme current-goal))
			(and (neq class 'preference)
			     (neq goal current-goal)))
		    (process-subgoal-result wme current-goal))
		   (t (soaraddprop current-goal wme 'non-results)))))





(defun set-chunk-bit-t (goal) ; John.Laird 17-Jun-86 09:06 
       (cond (goal (rplaca (soarnthcdr 9 (get goal 'context))
			   t)
		   (soarmapc #'(lambda (x)
				       (rplaca (soarnthcdr 9
							   (get x
								'context))
					       t))
			     (get goal 'supergoals)))))





(defun
  split-apart-conditions
  (data-matched current-goal) ; Randy.Gobbel  3-Dec-86 11:15  ; Separate 
 ; wme elements into those that are pre-existing structure or results and 
 ; those that are local to the subgoal 
  (prog
    (wme results non-results possible-results negated-results negsym 
	 external-ids flag)
    (setq results nil)
    (setq negated-results nil)
    (setq non-results nil)
    (setq external-ids nil)
    (setq possible-results nil)
    (setq flag t)
    l1
    (cond ((and (null data-matched)
		flag)
	   (setq flag nil)
	   (setq data-matched (reverse possible-results))
	   (setq possible-results nil)))
    (cond ((and (null data-matched)
		(null flag))
	   (return (list (reverse (append results negated-results))
			 non-results
			 (dreverse possible-results)))))
    (setq wme (pop data-matched))
    (cond ((is-negation wme)
	   (setq negsym wme)
	   (setq wme (pop data-matched))
	   (cond ((soarlistp (car wme))
		  (cond ((negated-chunk-condition-test (car wme)
						       current-goal)
			 (soarpush negsym negated-results)
			 (soarpush wme negated-results))))
		 ((negated-chunk-condition-test wme current-goal)
		  (soarpush negsym negated-results)
		  (soarpush wme negated-results))))
	  ((eq (wme-class wme)
	       'goal)
	   (cond ((soarmember wme (get current-goal 'non-results))
		  (soarpush wme non-results))
		 (t (setq external-ids (union external-ids (extract-ids wme)))
		    (soarpush wme results)
		    (setq flag t))))
	  (t (cond ((soarmemq wme (get current-goal 'non-results))
		    (soarpush wme non-results))
		   ((soarmemq wme (append (get current-goal
					       'one-time-results)
					  (flattop (get current-goal
							'results))))
		    (cond ((or (and (eq (wme-class wme)
					'preference)
				    (test-for-connected-preference wme 
							       external-ids))
			       (and (neq (wme-class wme)
					 'preference)
				    (soarmemq (wme-id wme)
					      external-ids)))
			   (soarpush wme results)
			   (setq external-ids (union external-ids
						     (extract-ids wme)))
			   (setq flag t))
			  (t (soarpush wme possible-results))))
		   (t (soarpush wme results)
		      (setq external-ids (union external-ids
						(extract-ids wme)))
		      (setq flag t)))))
    (go l1)))





(defun start-build-time nil  ; John.Laird 19-Jun-86 11:56 
       (setq *begin-build-time* (alwaystime)))





(defun stop-build-time nil  ; John.Laird 19-Jun-86 11:56 
       (setq *elapsed-build-time* (iplus *elapsed-build-time*
					 (time-difference (alwaystime)
							  *begin-build-time*))))





(defun tersify-negations (negations symbol) ; Randy.Gobbel 11-Sep-86 18:01 
       (prog (condition return-list)
	     (setq return-list nil)
	     (soarwhile negations (setq condition (pop negations))
			(cond ((and (not (soarlistp (car condition)))
				    (eqp (get (wme-id condition)
					      'tested)
					 1))
			       (setf-wme-id condition '<unbound>)))
			(cond ((not (soarmember condition return-list))
			       (soarpush symbol return-list)
			       (soarpush condition return-list))))
	     (return return-list)))





(defun test-chunk-for-content (conditions) ; John.Laird 12-Jun-86 12:42 
       (prog (condition flag)
	     (setq flag t)
	     (soarwhile (and conditions flag)
			(setq condition (pop conditions))
			(cond ((eq condition '-))
			      ((soarlistp (car condition))
			       (or (test-chunk-for-content condition)
				   (setq flag nil)))
			      ((soarmemq (wme-class condition)
					 *chunk-classes*)
			       (setq flag nil))))
	     (and flag (soarwarn 
  "No chunk was built because no conditions had a class in *chunk-classes*"
				 " "))
	     (return flag)))





(defun
  test-connected-actions
  (actions) ; Randy.Gobbel 16-Jul-86 16:46 
  (prog (changed-flag action save-action saved-actions found-variable 
		      return-list make-flag label-binds)
	(setq changed-flag t)
	(setq label-binds nil)
	(setq make-flag nil)
	(soarwhile
	  (and actions changed-flag)
	  (setq saved-actions nil)
	  (setq changed-flag nil)
	  (soarwhile actions (setq action (pop actions))
		     (cond ((eq (car action)
				'make)
			    (setq save-action action)
			    (setq make-flag t)
			    (setq action (cdr action))
			    (cond ((and *constants* (wme-value action)
					(eq (wme-attribute action)
					    'value)
					(eq (wme-class action)
					    'constant)
					(not (soarassq (wme-id action)
						       label-binds)))
				   (soarpush (cons (wme-id action)
						   (wme-value action))
					     label-binds)))
			    (setq found-variable nil)
			    (soarwhile (and action (not (soarmemq (car action)
								  
							   *condition-vars*)))
				       (cond ((variablep (car action))
					      (setq found-variable t)))
				       (pop action))
			    (cond ((and (null action)
					found-variable)
				   (soarpush save-action saved-actions))
				  ((soarmemq (car action)
					     *condition-vars*)
				   (setq *condition-vars*
					 (union (find-variables save-action)
						*condition-vars*))
				   (setq changed-flag t)
				   (soarpush save-action return-list))
				  (t (soarpush save-action return-list))))
			   (t (soarpush action return-list))))
	  (setq actions saved-actions))
	(soarwhile label-binds (setq return-list
				     (append return-list
					     (list (list 'label-bind
							 (caar label-binds)
							 (cdar label-binds)))))
		   (pop label-binds))
	(cond ((and make-flag (null changed-flag))
	       (soarwarn "Unconnected actions in production" (p-to-sp actions))
	       (return (append (nreverse return-list)
			       actions)))
	      ((null actions)
	       (return (nreverse return-list))))))





(defun test-connected-field (field external-ids) ; Randy.Gobbel 
 ;  3-Dec-86 11:15 
       (or (null field)
	   (eq field 'undecided)
	   (soarmemq field external-ids)))





(defun test-for-connected-preference (wme external-ids) ; Randy.Gobbel 
 ;  3-Dec-86 11:14 
       (and (test-connected-field (wme-goal wme)
				  external-ids)
	    (test-connected-field (wme-problem-space wme)
				  external-ids)
	    (test-connected-field (wme-state wme)
				  external-ids)
	    (test-connected-field (wme-operator wme)
				  external-ids)))





(defun test-preference-field (field subgoal) ; John.Laird 23-Jul-85 10:29 
       (or (null field)
	   (eq field 'undecided)
	   (neq (get field 'in-id-field)
		subgoal)))





(defun trace-new-condition (action trace-flag depth) ; Randy.Gobbel 
 ; 12-Sep-86 12:05 
       (prog (matrix)
	     (cond ((and trace-flag (is-negation action))
		    (do-tabto (1- (times 3 depth))
			      (trace-file))
		    (traceprinc action))
		   (trace-flag (do-tabto (times 3 depth)
					 (trace-file))
			       (setq matrix (p-to-sp action))
			       (pplinet (cond ((> (length matrix)
						  1)
					       matrix)
					      (t (car matrix))))))))





(defun union-with-negations (list2 list1) ; Randy.Gobbel  6-Aug-86 09:49 
       (prog (element symbol)
	     l0
	     (cond ((null list1)
		    (return list2)))
	     (setq element (pop list1))
	     (cond ((is-negation element)
		    (setq symbol element)
		    (setq element (pop list1))
		    (cond ((not (soarmember element list2))
			   (setq list2 (append list2 (list symbol element))))))
		   ((not (soarmember element list2))
		    (setq list2 (append list2 (list element)))))
	     (go l0)))





(defun
  variablize-negations
  (negations) ; Randy.Gobbel 22-Aug-86 11:00 
  (prog
    (condition return-list new-condition id value)
    (setq return-list nil)
    (soarwhile
      negations
      (setq condition (pop negations))
      (cond ((soarlistp (car condition))
	     (setq return-list (cons (variablize-negations condition)
				     return-list)))
	    (t (setq new-condition (list (pop condition)))
	       (soarwhile condition (setq id (pop condition))
			  (setq value (cdr (soarassq id *learn-ids*)))
			  (cond (value (soarputprop value
						    (1+ (or (get value
								 'tested)
							    0))
						    'tested))
				(t (setq value id)))
			  (soarpush value new-condition))
	       (setq new-condition (nreverse new-condition))
	       (cond ((not (soarmember new-condition return-list))
		      (soarpush new-condition return-list))))))
    (return (reverse return-list))))


;;; Initialize soar here as it is the last file loaded.

(eval-when (load) (start-soar))

