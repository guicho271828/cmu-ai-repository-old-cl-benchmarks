;;; -*- MODE:COMMON-LISP; READTABLE:COMMON-LISP; PACKAGE:BB1; BASE:10  -*-

;;;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;                                                                                                                    +
;;;                                  BB1 Version 1.2                                                                  +
;;;                                                                                                                    +
;;;  Copyright (c) 1986,1987  Stanford University, Department of Computer Science, Knowledge Systems Laboratory  +
;;;                                                                                                                    +
;;;  All rights reserved.  This software is distributed under license and may only be used under the                   +
;;;  conditions of that license.                                                                                         +
;;;                                                                                                                    +
;;;  BB1 was designed by Dr. Barbara Hayes-Roth.                                                                    +
;;;                                                                                                                    +
;;;  Micheal Hewett, Alan Garvey, M. Vaughan Johnson Jr., Robert Schulman, and Reed Hastings                       +
;;;  have worked on this implementation.                                                                              +
;;;                                                                                                                    +
;;;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; 
;;;                                  FILE INFO
;;; 
;;; 
;;;
;;;  BB1UTILITIES.LISP      -     Miscellaneous useful functions
;;;
;;;  Mike Hewett     02/09/86
;;;                    04/14/86  Added REMOVE-INNER-LISTS                                         -mh
;;;                    05/10/86  Modified PACK* to use FORMAT so that it works with numbers.      -mh
;;;                    07/21/86  Added PUTASSOC                                                     -mh
;;;                    07/22/86  Added MAKE-PACKED                                                 -mh
;;;                    09/29/86  Added MKLIST                                                        -mh
;;;                    10/02/86  Modified EVAL-ATTRIBUTE-VALUE-LIST to handle atoms ($EVAL them) -mh
;;;                    10/02/86  Added COPY-ANYTHING                                               -mh
;;;                    10/07/86  Fixed bug in PUTASSOC                                               -mh
;;;                    10/07/86  Added SORT-SIMPLE-LIST                                             -mh
;;;                    10/22/86  Modified PACK* to intern in BB1 package                             -mh
;;;                    10/22/86  Added USER-PACK* to intern in USER package                       -mh
;;;                    11/29/86  Added COPY-HASH-TABLE from PMT-EDIT                            -mh
;;;                    11/29/86  Modified COPY-ANYTHING to handle structures too                   -mh
;;;                    04/06/87  Added USER-NAME-STRING                                           -mh
;;;                    05/01/87  Added PARAGRAPH                                                   -mh
;;;                    05/03/87  Moved utilities from BB1EXPLAIN:  UPTO, ENUMERATE                 - bob 
;;;                    05/05/87  Added PRINT-PARAGRAPH                                            -mh
;;;                    06/02/87  Added GENERATE-BINARY-COMBINATIONS                             -mh
;;;                    07/23/87  Modified COPY-ANYTHING and added COPY-ANY-STRUCTURE          -mh


;;;  This file contains useful miscellaneous functions used by the BB1 system.
;;;  Since there are very few general functions that can't be placed in
;;;  any one file, the function you are looking for is probably not here.

;;;  Contents:
;;;             AVERAGE                        - written by Reed, moved here from PMT-EDIT
;;;             PACK*                           - Simulates InterLisp function of the same name.
;;;             USER-PACK*                     - Same as above, except interns in USER package
;;;             MAKE-PACKED                   - Build an arbitrarily long atom
;;;             MAKE-UNPACKED                 - Break up an arbitrarily long atom
;;;             DUMP-HASH-TABLE-TO-STREAM - Writes a hash table out in a LISP-readable format.
;;;             EVAL-ATTRIBUTE-VALUE-LIST    - Used in the interpreter, $FIND, etc.
;;;             GET-TIME-STRING               - Returns a string with current date and time.
;;;             ATTRIBUTE-VALUE-FROM-KS     - Returns an attribute-value list created from a KS structure
;;;             REMOVE-INNER-LISTS            - Turns a set of nested lists into one big list.
;;;             PUTASSOC                       - Adds or modifies values in association lists.
;;;             COPY-ANYTHING                 - Copies tree or array
;;;             COPY-ANY-STRUCTURE          - Copies a structure
;;;             MKLIST                          - Makes object into a list if necessary
;;;             SORT-SIMPLE-LIST               - Sorts the given list.
;;;             COPY-HASH-TABLE              - Returns a copy of the given hash table.
;;;             USER-NAME-STRING             - Returns a string with the logged-in user's name
;;;             PARAGRAPH                     - Breaks up a string into a list of strings not exceeding a certain length.
;;;             PRINT-PARAGRAPH               - Formats a string withing certain bounded margins.
;;;             ENUMERATE                     - Enumerates a list of words in a natural-language string, e.g. 'A', 'A and B', 'A, B and C' 
;;;             UPTO                           - Returns a subset of a list upto the specified key
;;;             GENERATE-BINARY-COMBINATIONS - Given '(A B), returns '((A B) (A NIL) (NIL B) (NIL NIL))  Used in BB1AGENDA.
;;;--------------------------------------------------------------------------------------------


;;;----- Package information ------

(in-package 'BB1)

;;;----------------------------


;;;  Source:



;;-------  AVERAGE  -------
;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun average (x y)
  "Written by Reed.  Moved here from PMT-EDIT."
  (truncate (/ (+ x y) 2)))


;;-------  PACK*  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun pack* (&rest symbol-elements)
  "Makes a symbol composed of the elements given, which can be atoms or strings.
Interns the symbol in the BB1 package."

;; This function INTERNs a symbol by concatenating the symbol-elements together,
;; then calling INTERN.

;; MH:  02/09/86

  (let ((*package* (find-package 'bb1))
	)
    
    (intern (apply #'concatenate
		   'string
		   (mapcar #'(lambda (elt)
			       (if (numberp elt)
				   (write-to-string elt)
				   (string-upcase elt)))     ;Both upcases a string and converts an atom to a string
			   symbol-elements))
	    )
    )
  )




;;-------  USER-PACK*  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun user-pack* (&rest symbol-elements)
  "Makes a symbol composed of the elements given, which can be atoms or strings.
Interns the symbol in the USER package and exports it to the BB1 package."

;; Hope we don't need a lot of these for different packages!

;; This function INTERNs a symbol by concatenating the symbol-elements together,
;; then calling INTERN.

;; MH:  02/09/86


  (let ((new-sym   (intern (apply #'concatenate
				  'string
				  (mapcar #'(lambda (elt)
					      (if (numberp elt)
						  (write-to-string elt)
						   (string-upcase elt)))     ;Both upcases a string and 
					  symbol-elements))                 ;      converts an atom to a string
			   'user))
	)
    
    new-sym
    )
  )




;;;-------  MAKE-PACKED  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun make-packed (pack-char-string &REST name-elements)
  "Takes each of ELEMENTS and packs them together, interspersed with pack-char-string, which
can be a string or character.  All elements and pack-char-strings are converted to upper-case.

Example:  (make-packed ''-'' 'a 'long ''name'')  ->  A-LONG-NAME"

  (let ((newname (car name-elements))                            ;MAY BE NIL
	(between (string-upcase (string pack-char-string)))
	)

    (if newname
	(setq newname (string-upcase (string newname))))
    (dolist (part (cdr name-elements))
      (setq newname (concatenate 'string newname between (string-upcase (string part)))))

      ;;Intern the new name in the USER package.

    (user-pack* newname))
  )



;;-------  DUMP-HASH-TABLE-TO-STREAM  -------

;;;Edited by Vaughan Johnson   16 Apr 87  16:49
(defun dump-hash-table-to-stream (h-table-name h-table out-stream)
  "Writes the hash table out in a format that can be read back in.
  OUT-STREAM should already be open.  H-TABLE-NAME will be declared a global variable.
  in the file.

  Note that hash table entries are printed by (WRITE  ...  :PRETTY t :ESCAPE t).  This
  should generate a readable output, but is not guaranteed.  Therefore, any records printed
  by this means should have a :PRINT-FUNCTION defined which prints it out."

   ;; This would presumably be faster if LISP would print structures in the #S(...) format
   ;; which GLS guarantees it will.  However, the TI software (and possibly others) do not
   ;; follow these directions, so we have to print it out that way ourselves so that
   ;; we can guarantee that the results can be read back in.        11/23/86  -mh


;  (format out-stream "~%(defvar ~A nil)~%~%" h-table-name)

  (format out-stream "(setf ~A (make-hash-table :size ~D))~%~%"
	  h-table-name (max 10 (hash-table-count h-table)))

  (maphash #'(lambda (key val)
	       (format out-stream "(setf (gethash (quote ~A) ~A) (quote " key h-table-name)
	       (write  val :stream out-stream
		           :escape t
			   :pretty t)
	       (format out-stream "))~%"))
	   h-table)
  )




;;;-------  EVAL-ATTRIBUTE-VALUE-LIST  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun eval-attribute-value-list (a-v-list)
  "Input is a list of lists.  Each inner list has two elements.
Output is a corresponding list of lists, with the second element of the
inner list evaluated."

  (if (atom a-v-list)
      (setq a-v-list ($Eval a-v-list)))
  
  (mapcar #'(lambda (var-val)
	      `(,(first var-val) ,($Eval (second var-val))))
	  a-v-list)
  )



;;-------  GET-TIME-STRING  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun get-time-string (&OPTIONAL (short-version? nil))
  "Interprets (get-decoded-time) and returns a string with the date and time in some nice format."

  (let ((second                    0)
	(minute                    0)
	(hour                      0)
	(date                      0)
	(month                     0)
	(year                      0)
	(day-of-week               0)
	(daylight-savings-time-p nil)
	(time-zone                 0))

    (multiple-value-setq   (second minute hour date month year day-of-week
				   daylight-savings-time-p time-zone)
      (get-decoded-time))

    (if short-version?

	(format nil "~D ~A ~D  ~D:~2,'0D"   ;1 January 1987 16:14"
		date 
		(cdr (assoc month '((1 . "January")    (2 . "February") (3 . "March")     (4 . "April")
				    (5 . "May")        (6 . "June")     (7 . "July")      (8 . "August")
				    (9 . "September") (10 . "October") (11 . "November") (12 . "December"))))
		year
		hour
		minute)

	;;Else give long verbose date and time
	
	(format nil "~A, ~D ~A ~D  ~D:~2,'0D:~2,'0D"     ;Sunday, 1 January 1986  16:14:12
		
		(cdr (assoc day-of-week '((0 . "Monday") (1 . "Tuesday")  (2 . "Wednesday") (3 . "Thursday")
					  (4 . "Friday") (5 . "Saturday") (6 . "Sunday"))))
		date
		(cdr (assoc month '((1 . "January")    (2 . "February") (3 . "March")     (4 . "April")
				    (5 . "May")        (6 . "June")     (7 . "July")      (8 . "August")
				    (9 . "September") (10 . "October") (11 . "November") (12 . "December"))))
		year
		hour
		minute
		second)
	)
    )
  )

;;;-------  ATTRIBUTE-VALUE-FROM-KS  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun attribute-value-from-ks (ks-name)
  "Creates an attribute-value list (which shouldn't be evaluated) by pulling
information from slots in the knowledge source structure associated with the name."

  (let ((ks-info (eval ks-name)))

    (if (knowledge-source-p ks-info)
	`((user::triggerconditions    ,(knowledge-source-trigger-conditions     ks-info))
	  (USER::ksarcontexts         ,(knowledge-source-context                ks-info))
	  (USER::preconditions        ,(knowledge-source-preconditions          ks-info))
	  (USER::obviationconditions  ,(knowledge-source-obviation-conditions   ks-info))
	  (USER::ksvars               ,(knowledge-source-action-variables       ks-info))
	  (USER::actions              ,(knowledge-source-actions                ks-info))
	  (USER::frombb               ,(knowledge-source-from-blackboard        ks-info))
	  (USER::tobb                 ,(knowledge-source-to-blackboard          ks-info))
	  (USER::fromlevel            ,(knowledge-source-from-level             ks-info))
	  (USER::tolevel              ,(knowledge-source-to-level               ks-info))
	  (USER::name                 ,(knowledge-source-name                   ks-info))
	  (USER::datecreated          ,(knowledge-source-date-created           ks-info))
	  (USER::description          ,(knowledge-source-description            ks-info))
	  (USER::author               ,(knowledge-source-author                 ks-info))
	  (USER::cost                 ,(knowledge-source-cost                   ks-info))
	  (USER::reliability          ,(knowledge-source-reliability            ks-info))
	  )
      ;;ELSE - isn't a knowledge source
      (bb1error 'ATTRIBUTE-VALUE-FROM-KS
		ks-name " isn't a knowledge source.")
      )
    )
  )



;;;-------  REMOVE-INNER-LISTS  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun remove-inner-lists (form)
  "Turns a list of nested lists into one big list of atoms  Removes NILs."

  ;; Modified 11/07/86 to get a little more speed      -mh

  (declare (optimize (speed 3) (safety 0)))

  (cond ((null form)   nil)
	((atom form) (list form))
	(t           (nconc (remove-inner-lists (car form)) (remove-inner-lists (cdr form)))))
  )




;;;-------  PUTASSOC  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun putassoc (element value list)
  "The pair (element . value) is added to the list if 'element' is not there already.
If element exists, its CDR is setf'ed to value."

  ;; A faster way would be to added it to the front with ACONS, then remove any old element.
  ;; But that way wouldn't preserve ordering, which is needed in the BB1RATER functions which
  ;; call this.   -mh

  (if (assoc element list)
      (setf (cdr (assoc element list)) value)
    ;;ELSE
      (nconc list `(,(cons element value)) )
      )
  )



;;;-------  MKLIST  -------

;;;Edited by Byung Suk Lee         28 Jul 87  15:07
(defun mklist (thing)

  (cond	((listp thing) thing)
	(t   	       (list thing)))
  )



;;;-------  COPY-ANYTHING  -------

;;;Edited by Mike Hewett           17 Jun 87  10:44
;;;Edited by Mike Hewett           21 Jul 87  10:00
;;;Edited by Mike Hewett           23 Jul 87  15:44
(defun copy-anything (thing)
  "Calls COPY-TREE if thing is a tree,  COPY-SEQ if it's a SEQ,
else tries to duplicate a structure if it's a structure, else returns THING."

  ;; There are many ways to write this function.  The best would be
  ;; To write a recursive function which traces the whole tree and
  ;; calls the appropriate function on each leaf node.
  ;; However, this function is primarily called in $VALUE which must
  ;; be *extremely* efficient.
  ;; So I choose to only look at the top level of the item and call COPY-TREE
  ;; if it is a list, and COPY-SEQ otherwise.

  (cond ((typep thing 'structure)  (copy-any-structure thing))   ;Have to try to copy, but it's tricky...
	((listp thing)             (copy-tree thing))
	((typep thing 'sequence)   (copy-seq thing))
	(t                         thing)
	)
  )


;;;-------  COPY-ANY-STRUCTURE  -------

;;;Edited by Mike Hewett           23 Jul 87  15:44
(defun copy-any-structure (structure-thing)
  "Tries to call the structure's COPY function."

  (let ((default-copy-fn-name (pack* 'copy- (type-of structure-thing)))
	(default-make-fn-name (pack* 'make- (type-of structure-thing)))
	)
    
    (cond ((fboundp default-copy-fn-name)    (funcall default-copy-fn-name structure-thing))
	  ((fboundp default-make-fn-name)    (read-from-string (write-to-string structure-thing)))

	        ;; Oh well, can't seem to copy it
	  (t                                 structure-thing)
	  )
    )
  )



;;;-------  MAKE-UNPACKED  -------

(defun make-unpacked (pack-char extended-name)
  "The opposite of MAKE-PACKED.  Given #\. and BB.LEVEL.OBJECT, returns (BB LEVEL OBJECT)."


  (setq extended-name (string extended-name))

  (if (stringp pack-char)
      (setq pack-char (elt pack-char 0)))
  
  (LET ((NEXT-POSN 1)
	result dummy)

   (reverse
     (loop
       
       (if (null next-posn)
	   (return result))
       
       (multiple-value-setq (dummy next-posn)
			    (find pack-char extended-name))
       
       (push (pack* (subseq extended-name 0 next-posn)) result)
       
       (if next-posn
	   (setq extended-name (subseq extended-name (1+ next-posn))))
       )
     )
   )
  )



;;;-------  SORT-SIMPLE-LIST  -------

;;;Edited by Mike Hewett   10 Apr 87  11:29
;;;Edited by Mike Hewett   10 Apr 87  15:17
(defun sort-simple-list (items &KEY (item-type :SYMBOL) (order :ASCENDING) (copy  t))
  "Input should be a simple list, all of one type.  Types currently handled are:

       :SYMBOL  :STRING  :CHARACTER  :NUMBER

When item-type is :NUMBER and the elements of ITEMS (as determined by its CAR)
is of type COMPLEX, the function ABS will be applied to the numbers before the comparison is made.
:STRING and :SYMBOL types use #'STRING-LESSP as the comparison
function, so character case is ignored.

Makes a copy of the input arg (unless :COPY NIL is specified)
because the SORT fn is destructive.

The output is a list of the same elements sorted appropriately.
Normal sort order is :ASCENDING  (1 2 3 4 ...).  Can optionally be :DESCENDING."

  ;;First, some error checking

  (ccase item-type
    (:SYMBOL      (check-type (car items) symbol "an item"))    
    (:STRING      (unless (typep (car items) 'string)
		    (ferror NIL "The argument ~A is not a list of ~As"
			    items 'string)
		    )
		  )
    (:CHARACTER   (unless (typep (car items) 'character)
		    (ferror NIL "The argument ~A is not a list of ~As"
			    items 'character)
		    )
		  )
    (:NUMBER      (unless (typep (car items) 'number)
		    (ferror NIL "The argument ~A is not a list of ~As"
			    items 'number)
		    )
		  )
    )

    
    ;; Now select the correct comparison function for the type given


  (let ((sort-key nil))
    (let ((sort-function (ccase item-type
			       (:SYMBOL      (setq sort-key #'string)
					     (ccase order
						(:ASCENDING    #'string-lessp)
						(:DESCENDING   #'string-greaterp))
					      )
			       
			       (:STRING      (ccase order
						(:ASCENDING    #'string-lessp)
						(:DESCENDING   #'string-greaterp))
					      )
			       (:CHARACTER   (ccase order
						(:ASCENDING    #'char<)
						(:DESCENDING   #'char>)
						)
					      )
			       (:NUMBER      (if (complexp (car items))
						 (setq sort-key #'abs))
					     (ccase order
					       (:ASCENDING    #'<)
					       (:DESCENDING   #'>)
					       )
					     )
			       ))
	  (sort-items     (cond (copy  (copy-list items))
				(t     items))
			  )
	  )

         ;;Finally, SORT!

      (if sort-key
	  (sort sort-items sort-function
		:key sort-key)
	  ;;ELSE - no sort-key fn
	  (sort sort-items sort-function)
	  )
      )
    )
  )



;;;-------  COPY-HASH-TABLE  -------

;;;Edited by Vaughan Johnson   17 Apr 87  18:22
(defun copy-hash-table (table &OPTIONAL copy-size)		   ;if table is nil, return an empty hash table
  "Copies the given hash table.  If argument is NIL, returns a new hash table.
Caller can optionally specify the size of the new hash table.  If size is not
specified, it will be (max 10 previous-size)."

  (if table
      (let ((new-table (make-hash-table :size (or copy-size
						  (max 10 (hash-table-count table)))))
	    )
	(maphash #'(lambda (key val)
		     (setf (gethash key new-table)
			   (copy-anything val))
		     )
		 table)
	new-table
	)

    ;;ELSE - make a new table

      (make-hash-table :size (or copy-size 10))
      )
  
  )



;;;;-------  USER-NAME-STRING  -------

;;;Edited by Mike Hewett   1 May 87  18:12
;;;Edited by Bob Schulman   3 May 87  12:34
#|(defun user-name-string ()
  "Returns a string with the logged-in user's name.  Returns null-string if name
can't be determined."

  (cond ((not (string-equal fs:user-personal-name-first-name-first ""))
            fs:user-personal-name-first-name-first)
	((and (not (equal user-id ""))
	      (not (equalp user-id "File Server")))
	    user-id)
	(t "Anonymous"))
  )
|#


;;;;-------  PARAGRAPH  -------

;;;Edited by Bob Schulman   3 May 87  12:34
;;;Edited by Mike Hewett   5 May 87  18:03
;;;Edited by Mike Hewett   7 May 87  13:02
(defun paragraph (string max-line-length &KEY (break-at :word) (break-chars nil))
  "PARAGRAPH returns a list of strings, each of which is <= max-line-length (in characters).

:BREAK-AT can be any of :WORD   - Breaks only on whitespace (space,tab,CR)
                              :SYMBOL - Breaks on any of (space, tab, CR, -, _)
                              :ANYWHERE - Doesn't worry where it breaks the string.

:BREAK-CHARS can be a list of characters used witht he :WORD or :SYMBOL options
at which points the line can be broken.

  Lines broken at :WORD and :SYMBOL will not have leading whitespace characters (space tab CR).
  Lines broken :ANYWHERE may have leading whitespace.

The strings are not guaranteed to have MAX-LINE-LENGTH characters (for example, when
the :WORD break-at option is used).

INDEX: :topic (bb1-internal) :keyword (paragraph string)
"

    ;;Guard conditions

  (assert (stringp string) (string))
  (assert (integerp max-line-length) (max-line-length))
  (assert (>= max-line-length 0) (string))


     ;;Have two different methods, depending on the :BREAK-AT option

  (if (string-equal string "")   ;Easy case which causes next part to break if not handled here...
      (list string)

      ;;ELSE - normal case
  (ccase break-at

    ((:WORD :SYMBOL) (let ((output-items         nil)
			   (next-word            "")
			   (new-string           "")
			   (next-break-position   0)
			   (break-characters      (case break-at
						     (:WORD    (or break-chars '(#\SPACE #\RETURN #\TAB)))
						     (:SYMBOL  (or break-chars
								   '( #\- #\SPACE #\RETURN #\TAB #\_)))))
			   )
		       (declare (type list    output-items break-characters)
				(type string  next-word    new-string)
				(type integer next-break-position)
				(optimize     (speed 3)
					      (safety 0))
				)

		         ;;Read from the string, stuffing the result into substrings

		       (loop
  ;;---DEBUG--->   (format *standard-output* "~%At ~D, string= ~S~%   output= ~A"
  ;;---DEBUG--->		 next-break-position string output-items)

			 (setq next-break-position (or (position-if #'(lambda (ch) (member ch break-characters
											   :test #'char=))
								    string
								    :START (position-if-not
									     #'(lambda (ch)
										 (member ch break-characters
											 :TEST #'char=))
									     string))
						       (length string)))
			 (setq next-word (subseq string 0 next-break-position))
			 (cond ((string= new-string "")         (setq new-string next-word))
			       ((<= (+ (length new-string)
				       (length next-word))
				    (1- max-line-length))            (setq new-string (concatenate 'string
									      new-string " " next-word)))
			         (t      ;;Start a new string
				(push new-string output-items)
				(setq new-string next-word)))
			 
			 (setq string (string-left-trim '(#\SPACE #\TAB #\RETURN)
							(subseq string next-break-position nil)))
			 
			 ;;But what if the next-word is > max???   ... Chop it up!
			 (when (> (length new-string) max-line-length)
			   (push (subseq new-string 0 max-line-length) output-items)
			   (setq new-string (subseq new-string max-line-length nil))
			   )
			 (when (string= string "")
			   (unless (string= new-string "")
			     (push new-string output-items)
			     (return))
			   )
			 )
		       (nreverse output-items)
		       ))

    (:ANYWHERE       (let ((current-string-length (length string))
			   (output-items          nil)
			   )
		       (declare (type list    output-items)
				(type integer current-string-length)
				(optimize     (speed 3)
					      (safety 1))
				)

		          ;;Loop, collecting a substring each time and pushing it onto OUTPUT-ITEMS.

		       (loop  (push (subseq string 0 (min current-string-length
							  max-line-length))
				    output-items)

			      (when (< current-string-length max-line-length)
				(return)
				)

			      (setq string (subseq string max-line-length nil))
			      (setq current-string-length (length string))
			      )
		       (nreverse output-items)
		       ))
    )
  )
  )
  


;;;-------  PRINT-PARAGRAPH  -------

;;;Edited by Mike Hewett   5 May 87  18:12
;;;Edited by Mike Hewett   5 May 87  18:42
;;;Edited by Mike Hewett   8 May 87  10:21
(defun print-paragraph (list-or-string-of-words
			&OPTIONAL (stream *STANDARD-OUTPUT*)
			&KEY (left-margin 0)
			(right-margin 79)
			(terpris-before 1)
			(terpris-after  0))

   "Prints the given words in a left-justified paragraph whose LEFT-MARGIN and
RIGHT-MARGIN can be specified.  If TERPRIS-BEFORE is 0, the first line
will be printed at the current cursor position of stream.

Each line except the last is printed followed by a carriage return. 
So TERPRIS-AFTER = 2 will produce a blank line after the paragraph."

  (when (listp list-or-string-of-words)
    (setq list-or-string-of-words (string-left-trim " " (format nil "~{ ~A~}" list-or-string-of-words)))
    )

  (assert (stringp list-or-string-of-words) (list-or-string-of-words)
	  "First argument to PRINT-PARAGRAPH is not a list or string of words.")
  (assert (integerp terpris-before) (terpris-before)
	  "TERPRIS-BEFORE is ~A, which is not an integer." terpris-before)
  (assert (integerp terpris-after) (terpris-after)
	  "TERPRIS-AFTER is ~A, which is not an integer." terpris-after)
  

    ;;Ready to go

  (let* ((strings-to-print (paragraph list-or-string-of-words (- right-margin left-margin)))
	 (left-margin-tab  (format nil "~~~D,0T" left-margin))
	 (last-string      (car (last strings-to-print)))
	 )

    (if (= terpris-before 0)
	(format stream "~A" (car strings-to-print))
	;;ELSE - indent the first line
	(progn
	  (dotimes (i terpris-before)
	    (terpri stream))
	  (format stream "~A" (format nil "~@?~A" left-margin-tab (car strings-to-print))))
      )
      ;;First string has been printed.  If it's not the last, print a carriage return
    (unless (= 1 (length strings-to-print))
      (terpri stream))

      ;;Print rest of strings

    (dolist (str (cdr strings-to-print))
      (format stream "~A" (format nil "~@?~A" left-margin-tab str))
      (unless (string-equal str last-string)
	(terpri stream))
      )

    (dotimes (i terpris-after)
      (terpri stream)
      )
    )
  )



;;;;-------  ENUMERATE  -------

;;;Edited by Bob Schulman   3 May 87  12:34
;;;Edited by Mike Hewett   5 May 87  18:07
(defun enumerate
       (word-list)
  "Returns the words in a natural-language string, e.g. 'A', 'A and B', 'A, B and C'" 
  (apply #'format
	 (cons nil
	       (cons
		 "~#[ <none>~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]" word-list)))
  )


;;;;-------  UPTO  -------

;;;Edited by Bob Schulman   3 May 87  12:34
;;;Edited by Mike Hewett                2 Jun 87  8:21
(defun upto
       (key list)
  "Returns a version of the list up to the target key, inclusive"
  (butlast list (or (position key (reverse list)) 0))
  )



;;;-------  GENERATE-BINARY-COMBINATIONS -------

;;;Edited by Mike Hewett                2 Jun 87  8:21
(defun generate-binary-combinations (l)
  "Returns a list of permutations of the elements of the list L. 
For example:

   (GENERATE-BINARY-COMBINATIONS '(a b)  

      ==> '((a b) (a nil) (nil b) (nil nil))

This is like enumerating all possible binary numbers <= N, where
N = (length L).
"

  (generate-binary-combinations-aux (car l) (cdr l))
  )


;;;Edited by Mike Hewett                2 Jun 87  8:21
(defun generate-binary-combinations-aux (a l)
  "See GENERATE-BINARY-COMBINATIONS."
  
  (cond ((null l)       (list (list a) (list nil)))
        (t              (mapcan #'(lambda (l2)
				    (list (cons a l2) (cons nil l2)))
				(generate-binary-combinations-aux (car l) (cdr l))))
	)
  )
