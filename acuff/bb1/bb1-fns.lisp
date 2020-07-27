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
;;;  Micheal Hewett, Alan Garvey, M. Vaughan Johnson, Jr., Robert Schulman, and Reed Hastings                      +
;;;  have worked on this implementation.                                                                              +
;;;                                                                                                                    +
;;;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; 
;;;                                  FILE INFO
;;; 
;;; 
;;;  BB1$FNS.LISP      -     User-accessible functions for BB1
;;;
;;;  Mike Hewett     03/24/86   - $EVAL and $SET
;;;                    04/01/86  - Added rest of $FNS
;;;                    04/04/86  - Modified $SET to keep a record of local variable names on the KSAR
;;;                    04/11/86  - New $EVAL, $SET which use closures.  Deleted FIND-KSAR-LOCAL-VAR and BB1EVAL
;;;                    04/13/86  - Moved $EVAL, MAKE-CURRENT-CLOSURE, MAKE-KSAR-CLOSURE to BB1EVAL
;;;                    07/21/86  - Added Vaughan's function $ISA+                                                     -mh
;;;                    11/06/86  - Hand-optimized $VALUE                                                              -mh
;;;                    11/29/86  - New $SET for new EVAL mechanism                                                 -mh
;;;                    11/29/86  - Made $EVENT-LEVEL-IS, $EVENT-TYPE-IS, etc., in to macros for CL compatibility    -mh
;;;                    12/09/86  - Added $CAUSES, $ENABLES, $ENTAILS, $INCLUDES, $MODIFIED-BY, $PROMOTES        -mh
;;;                    02/26/87  - Fix $PROMOTES to follow CAUSES links                                              -bob
;;;                    12 Mar 87 - Added $EXAMPLES-OF, $COMPRISED-BY, and $COMPRISES                           - Vaughan
;;;                    16 Mar 87 - Added Mike's new $FIND-IF and $FIND-ALL-IF fns                                   - Vaughan
;;;                    05/02/87  - Rewrote $value and $values to use error system                                     -reed
;;;                    05/05/87  - Added $INSTANCES-OF, $GENERATOR, $OBJECTS-OF-TYPE                            -bob
;;;                    05/08/87  - Added a bunch of $FNS - $LEVEL-P, $BLACKBOARD-P, $MAP-LEVEL ...                -Mike
;;;                    06/18/87  - Added $TYPES-OF                                                                    -bob


;;;  This file contains BB1 functions designed to be accessible to the user.
;;;  If the user accesses other low-level BB1 functions, they may get into trouble!


;;;  Contents:
;;;             $SET                      - called by user to create/set a variable local to a KSAR
;
;;;             $SHORT-NAME               - retrieves short name of an object
;;;             $CHANGED-ATTRIBUTE-IS    - Used in trigger conditions
;;;             $CHANGED-LINK-IS           - Used in trigger conditions
;
;;;             $MAKE-BB1-NAME           - Calls MAKE-EXTENDED-NAME
;
;   --Type checking functions
;
;;;             $BB1-TYPE-P
;;;             $KNOWLEDGE-BASE-P
;;;             $BLACKBOARD-P
;;;             $LEVEL-P
;;;             $OBJECT-P
;;;             $ATTRIBUTE-P
;;;             $LINK-P
;
;;;             $HAS-ATTRIBUTE      - see also $HAS-VARIABLE below
;;;             $HAS-LINK
;;;             
;
;   --Mapping functions
;
;;;             $MAP-SYSTEM      - Maps over blackboards
;;;             $MAP-BLACKBOARD 
;;;             $MAP-LEVEL
;;;             $MAP-ATTRIBUTES
;;;             $MAP-LINKS
;;;             $MAP-$LINKS
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Prehistoric BB1 $FNS
;
;;;             $DATA                       - Retrieves user data.
;
;;;             $EVENT-BLACKBOARD-IS     - Used in trigger conditions
;;;             $EVENT-LEVEL-IS            - Used in trigger conditions
;;;             $EVENT-TYPE-IS             - Used in trigger conditions
;
;;;            $FIND-IF
;;;            $FIND-ALL-IF
;
;;;             $OBJECTS-AT-LEVEL         - Returns a list of all objects at a given level.
;
;;;             $VALUE                      - Retrieves data from the blackboard
;;;             $VALUES                     - Retrieves data from the blackboard
;;;             $VALUE-2                    - Retrieves data from the blackboard
;;;             INHERIT-$VALUE              - Used during inheritance
;;;             INHERIT-$VALUE-2           - Used during inheritance
;;
;;;             $MODIFIED-BY
;;;             $OBJECT                     - Retrieves linked objects from the blackboard
;;;             $OBJECTS                    - Retrieves linked objects from the blackboard
;;;             $OBJECTS-2                  - Retrieves linked objects from the blackboard
;;;             INHERIT-$OBJECTS            - Used during inheritance
;;;             INHERIT-$OBJECTS-2         - Used during inheritance
;
;;;             $ALL-OBJECTS-LINKED-TO    - Returns all objects linked to a given object.  Optionally uses inheritance.
;;;             $IS-LINKED-TO               - Determines whether a given object is linked to another object.
;;;             $IS-LINKED-TO-1             - Secondary function
;;;             $IS-LINKED-TO-2             - Tertiary function
;
;;;             $PROCEDURE-VALUE          - Retrieves last value returned by a procedural attachment
;;;             $PROCEDURE-VALUES         - Retrieves all values returned by a procedural attachment
;
;;;             $FIND                         - Finds objects with certain attributes.
;;;             $FIND-ALL                     - Finds objects with certain attributes.
;
;;;             $HAS-VARIABLE               - Checks to see whether the given KSAR has a local variable by that name.
;;;             $VARIABLE                    - Returns the value of the local variable of a KSAR.
;
;;;             $LAST-OBJECT-ADDED        - Returns the last object added to a given level (or to the system.)
;;;             $NEWEST-OBJECT            - Returns the last object added to the system.
;
;;;             $NAME                        - Renames an object on the blackboard.
;
;;;             $INPUT                        - Used to input a general expression
;;;             $INPUT-ATOM                 - Used to input an atom
;;;             $INPUT-Y-OR-N               - Obvious!
;;;             $OUTPUT                      - Displays a longer message in a temporary window
;;;             $MESSAGE                     - Displays a simple message on *BB1-MESSAGE-STREAM*
;
;
; - - - - -  Functions to handle $LINKS.  The user can add more of these (see the manual)
;
;;;             $CAUSES                      - Traces CAUSES links.
;;;             $COMPRISED-BY
;;;             $COMPRISES
;;;             $ENABLES                      - Traces ENABLES links.
;;;             $ENTAILS                      - Traces ENTAILS links.
;;;             $EXAMPLES-OF
;;;             $INCLUDES                     - Traces INCLUDES links.
;;;             $INSTANCES-OF
;;;             $ISA                           - Traces ISA links.
;;;             $LINK                          - Used by $ISA, $PLAYS, and $TYPE-OF
;;;             $MODIFIED-BY                 - Traces MODIFIED-BY links.
;;;             $PLAYS                        - Traces PLAYS links.
;;;             $PROMOTES                   - Traces PROMOTES links.
;;;             $TYPE-OF                     - Called by $ISA and $PLAYS


;;;------------------------------------------------------------------------------



;;;----- Package information ------

(in-package 'BB1)

;;;----------------------------



;;; -----  $SET  -----

;;;Edited by Mike Hewett           23 Jul 87  14:07
(defmacro $SET (var val &OPTIONAL (context-var? NIL))
  "Sets the given VAR to ($EVAL val) locally in *CURRENT-KSAR*.
  Stores values in *BB1-LOCAL-VARIABLES*. Returns the value the
  variable was set to.."

  ;; VAR may be a new var or one previously set.  

  ;; CONTEXTVARS is a subset of BOUNDVARS

  ;; BOUNDVARS is a list of two elements:  The first is a list of all bound variables,
  ;; The second is a list of all their values.  So $SET takes a long time,
  ;; But $EVAL is very fast.


  `(let ((value  ($EVAL ',val)))

     (let* ((pos (position ',var (car *BOUND-VARIABLES*)))
	    )
       
       (cond (pos    (rplaca (nthcdr pos (cadr *BOUND-VARIABLES*)) value))
	     (t      (push ',var   (car  *BOUND-VARIABLES*))
		     (push value (cadr  *BOUND-VARIABLES*))
					 ))
       
       ;; If it's a new CONTEXT VAR, store it.  
       
       (when ,context-var?
	 (pushnew ',var (car *CONTEXT-VARIABLES*))
	 )
       
       )

     (if *CURRENT-KSAR*
	
	 (progn
	  
	  (store-attribute *CURRENT-KSAR* 'USER::BOUNDVARS   *BOUND-VARIABLES*)
	  (store-attribute *CURRENT-KSAR* 'USER::CONTEXTVARS *CONTEXT-VARIABLES*)

	  )
	)
    ;;--return the value the variable was set to : CRUCIAL !

    value
    )
  )





;;;-------  $SHORT-NAME  -------

(defun $SHORT-NAME (obj)
  "Returns the short name of the argument, whether it is a level, object, etc."

  (short-name obj)
  )



;;;  -------  TRIGGER CONDITION FUNCTIONS  --------  TRIGGER CONDITION FUNCTIONS  -------

(defmacro $CHANGED-ATTRIBUTE-IS (attribute)
  "Checks to see if 'attribute' was changed in the current $TRIGGER-EVENT."

  `(if (member ',attribute (mapcar #'car ($Value $TRIGGER-EVENT 'USER::Changes)))
       ',attribute
         ;;ELSE
       nil
       )
  )


(defmacro $CHANGED-LINK-IS (link)
  "Checks to see if 'link' was changed in the current $TRIGGER-EVENT."

  `(if (member ',link (mapcar #'car ($Value $TRIGGER-EVENT 'USER::Changes)))
       ',link
         ;;ELSE
       nil
       )
  )


(defmacro $EVENT-BLACKBOARD-IS (blackboard)
  "Returns 'blackboard' if $TRIGGER-EVENT occurred on that blackboard."

  `(if (eql (blackboard-of-level ($Value $TRIGGER-EVENT 'USER::Eventlevel))
	    ',blackboard)
       ',blackboard
         ;;ELSE
       nil)
  )


(defmacro $EVENT-LEVEL-IS (level)
  "Returns 'level' if $TRIGGER-EVENT occurred on that level."

  `(if (eql ($Value $TRIGGER-EVENT 'USER::Eventlevel)
	   ',level)
      ',level
        ;;ELSE
      nil)
  )


;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:12
(defmacro $EVENT-TYPE-IS (type)
  "Returns 'type' if $TRIGGER-EVENT was that type (ADD or MODIFY)"

  `(if (eql ($Value $TRIGGER-EVENT 'USER::ChangeType)
	   ',type)
      ',type
        ;;ELSE
      nil)
  )



;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $make-bb1-name (&REST name-parts)
  "Builds a 'qualified' BB1 name from name-parts.  An example would be
building the name of an object from it's blackboard and level names:

  ($MAKE-BB1-NAME 'SOLUTION 'PHONEME 'OOO) == SOLUTION.PHONEME.OOO
"
  (apply #'make-extended-name name-parts)
  )


;;;;-------  TYPE CHECKING FUNCTIONS  ------------------------------


;;;-------  $BB1-TYPE-P  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $bb1-type-p (symbol bb1-type)
  "Returns T if the given symbol represents a BB1 object of the given type.
Returns NIL otherwise.

BB1-TYPE can be:  KNOWLEDGE-BASE, BLACKBOARD, LEVEL, OBJECT, ATTRIBUTE, LINK

Typical call: ($BB1-TYPE-P 'signal.interpretation.phoneme 'object) ==> T

See also the functions $KNOWLEDGE-BASE-P, $BLACKBOARD-P, etc.
"

  (let ((type-string (symbol-name bb1-type))     ;; @#%#! package problems...
	)
    
    (cond ((string-equal type-string "KNOWLEDGE-BASE") ($KNOWLEDGE-BASE-P symbol))
	  ((string-equal type-string "BLACKBOARD")     ($BLACKBOARD-P     symbol))
	  ((string-equal type-string "LEVEL")          ($LEVEL-P          symbol))
	  ((string-equal type-string "OBJECT")         ($OBJECT-P         symbol))
	   ((string-equal type-string "ATTRIBUTE")      ($ATTRIBUTE-P      symbol))
	   ((string-equal type-string "LINK")           ($LINK-P           symbol))

	  (t      (bb1error '$BB1-TYPE-P "Unknown type: " type-string "."))
	  )
    )
  )



;;;-------  $KNOWLEDGE-BASE-P  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $knowledge-base-p (symbol)
  "Returns T if the symbol is the name of a knowledge base."

  (cond ((valid-knowledgebase? symbol)   t)
	(t                             nil)
	)
  )


;;;-------  $BLACKBOARD-P  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $blackboard-p (symbol)
  "Returns T if the symbol is the name of a blackboard."

  (cond ((valid-blackboard? symbol)   t)
	(t                          nil)
	)
  )


;;;-------  $LEVEL-P  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $level-p (symbol)
  "Returns T if the symbol is the name of an existing level."

  (cond ((valid-level? symbol)   t)
	(t                     nil)
	)
  )


;;;-------  $OBJECT-P  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $object-p (symbol)
  "Returns T if the symbol is the name of an existing object."

  (cond ((valid-object? symbol)   t)
	(t                      nil)
	)
  )


;;;-------  $ATTRIBUTE-P  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $attribute-p (symbol)
  "Returns T if the symbol is the name of an attribute belonging to the system
 (as opposed to one actually existing on an object).  For example, the symbol LENGTH
may be defined as an attribute of the system, and would return T."

  (cond ((valid-attribute-for-system? symbol *bb1-system-name*)   t)
	(t                                                      nil)
	)
  )


;;;-------  $LINK-P  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $link-p (symbol)
  "Returns T if the symbol is the name of a link belonging to the system
 (as opposed to one actually existing on an object).  For example, the symbol
BELONGS-TO may be defined as a link of the system, and would return T."

  (cond ((valid-link-for-system? symbol *bb1-system-name*)   t)
	(t                                                 nil)
	)
  )



;;;-------  $HAS-ATTRIBUTE  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $Has-attribute (object attribute)
  "Returns T if the given OBJECT currently does have the given ATTRIBUTE.
Does not check to see if the attribute could be inherited.

This function could be used as a pre-check to $VALUE:

  (if ($Has-attribute obj 'SIZE)
    ($Value obj 'SIZE)
    ;;ELSE
    'UNKNOWN)
"

  (cond ((valid-attribute-for-object? attribute object)   t)
	(t                                              nil)
	)
  )


;;;-------  $HAS-LINK  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $Has-link (object link)
  "Returns T if the given OBJECT currently does have the given LINK.
Does not check to see if the link could be inherited.

This function could be used as a pre-check to $OBJECT(s)

  (if ($Has-link obj 'MEMBER-OF)
    ($OBJECT obj 'MEMBER-OF)
    ;;ELSE
    'NOT-A-MEMBER)
"

  (cond ((valid-link-for-object? link object)   t)
	(t                                    nil)
	)
  )





;- - - - - - -  Mapping functions - - - - - - - - - - - - - - - - - - - -

;;;-------  $MAP-SYSTEM  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $map-system (map-fn system)
  "Applies MAP-FN to every blackboard of SYSTEM.  Similar to MAPC.
Returns SYSTEM."

  (if (valid-system? system)
      (prog1  system
	      (mapc map-fn (blackboards-of-system system))
	      )
      ;;ELSE
      (BB1WARNING '$MAP-SYSTEM
		  system " is not the current BB1 system.")
      )
  )



;;;-------  $MAP-BLACKBOARD   -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $map-blackboard (map-fn blackboard)
  "Applies MAP-FN to every level of BLACKBOARD.  Similar to MAPC.
Returns BLACKBOARD."

  (if (valid-blackboard? blackboard)
      (prog1  blackboard
	      (mapc map-fn (levels-of-blackboard blackboard))
	      )
      ;;ELSE
      (BB1WARNING '$MAP-BLACKBOARD
		  blackboard " is not an existing blackboard.")
      )
  )




;;;-------  $MAP-LEVEL  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $map-level (map-fn level)
  "Applies MAP-FN to every object of level.  Similar to MAPC.
Returns LEVEL."

  (if (valid-level? level)
      (prog1  level
	      (mapc map-fn (objects-of-level level))
	      )
      ;;ELSE
      (BB1WARNING '$MAP-LEVEL
		  level " is not an existing level.")
      )
  )



;;;-------  $MAP-ATTRIBUTES  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $map-attributes (map-fn object)
  "Applies MAP-FN to every attribute of OBJECT.  Similar to MAPC.
Returns OBJECT."

  (if (valid-object? object)
      (prog1  object
	      (mapc map-fn (mapcar #'car (attributes-of-object object)))
	      )
      ;;ELSE
      (BB1ERROR '$MAP-ATTRIBUTES
		object " is not an existing object.")
      )
  )



;;;-------  $MAP-LINKS  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett   8 May 87  14:10
;;;Edited by Mike Hewett   8 May 87  14:12
(defun $map-links (map-fn object)
  "Applies MAP-FN to every link of OBJECT.  Similar to MAPC.
Returns OBJECT."

  (if (valid-object? object)
      (prog1  object
	      (mapc map-fn (mapcar #'car (links-of-object object)))
	      )
      ;;ELSE
      (BB1ERROR '$MAP-LINKS
		object " is not an existing object.")
      )
  )




;;;-------  $MAP-$LINKS  -------

;;;Edited by Mike Hewett   8 May 87  11:26
(defun $map-$Links (map-fn object)
  "Applies MAP-FN to every $link of OBJECT.  Similar to MAPC.
Returns OBJECT.

Note that there is not much you can do with this information."

  (if (valid-object? object)
      (prog1  object
	      (mapc map-fn (mapcar #'car ($links-of-object object)))
	      )
      ;;ELSE
      (BB1ERROR '$MAP-$LINKS
		object " is not an existing object.")
      )
  )




;;  -------  DATA RETRIEVAL  --------  DATA RETRIEVAL  --------  DATA RETRIEVAL  -------


;;; -------  $DATA  -------

(defun $data (data-name)
  "Retrieves user-defined data assigned to the given name."

  (if (valid-data? data-name)
      (copy-tree (value-of-data data-name))
     ;;ELSE - error
    (bb1error '$DATA
	      data-name " is not part of the data for " *BB1-SYSTEM-NAME* ".")
    )
  )


;;;-------  $OBJECTS-AT-LEVEL  -------

;;;Edited by Anonymous   27 Apr 87  23:08
;;;Edited by Reed Hastings   2 May 87  22:35
(defun $objects-at-level (level)
  "Returns a list of objects existing at the given level.
Objects are represented by their 'long' names."

  (if (valid-level? level)
      (copy-tree (objects-of-level level))
    ;;;ELSE - error
    (bb1error '$objects-at-level
	      level " is not an existing level.")
    )
  )


;;;-------  $VALUE  -------
;;; This function checks for an existing procedural attachment first.
;;; It then attempts a direct retrieval.
;;; If both of these fail, it attempts to inherit the value from a linked object.
;;;Edited by Mike Hewett   7 May 87  14:02
;;;Edited by Anonymous   7 May 87  23:59
;;;Edited by HASTINGS                   20 May 87  17:14
(defun $VALUE (object attribute &OPTIONAL (suppress-error? nil) (all-values-p nil)
	       &aux inheritance-opportunity long-attribute-name)
  "Main function used to retrieve attribute values.
   Object must be in long-name format, attribute in short name.
  If an error is generated, and we simply proceed without correcting it, we 
  return two values: NIL and :error.  Returns a copy of the attribute's value.

INDEX:  :topic (bb1)  :keyword (attribute value)
"
  
  (cond ((setq long-attribute-name (valid-attribute-for-object? attribute object))
	 ;;attribute exists locally.  get it's value.
	 (let* ((attr-info  (get-bb-attribute-data long-attribute-name))
		(proc       (bb-attributeinforec-procedural-attachment attr-info)))
	   (cond (proc     ;; First, try procedural attachment
		  (let ((value (apply  #'$Eval `(,proc ',object) nil)))
		    (add-to-attribute-array long-attribute-name nil nil value nil 'update)
		    (if all-values-p
			($Procedure-Values object attribute)
			value)))
		 ;;Use direct access
		 (all-values-p   (mapcar #'copy-anything (bb-attributeinforec-values attr-info)))
		 (t  (copy-anything (car (bb-attributeinforec-values attr-info)))))))
	((not (valid-object? object))
	 (let ((error (make-condition 'object-not-defined
				      "the object, ~a, isn't in the ~a system."
				      object  *bb1-system-name*)))
	   (multiple-value-bind (proceed-type value)
	       (signal error :proceed-types (list :new-value :retry-operation :use-other-object))
	     (ccase proceed-type
	       (:new-value value)
	       (:retry-operation ($value object attribute suppress-error? all-values-p))
	       (:use-other-object ($value value attribute suppress-error? all-values-p))))))
	((not (valid-attribute-for-system? attribute *bb1-system-name*))
	 (let ((error (make-condition 'attribute-not-in-system
				      "the attribute, ~a, isn't in the ~a system."
				      attribute *bb1-system-name*)))
	   (multiple-value-bind (proceed-type value)
	       (signal error :proceed-types (list :new-value :retry-operation :use-other-attribute))
	     (ccase proceed-type
	       (:new-value value)
	       (:retry-operation ($value object attribute suppress-error? all-values-p))
	       (:use-other-attribute ($value object value suppress-error? all-values-p))))))
	;;;EVERYTHING is valid, check for inheritance...
	((setq inheritance-opportunity (inheritance-opportunity-of-attribute attribute))
	 (multiple-value-bind  (inherited? inherited-value)
	     ;; inherited-value is in the form (x . val), where x = CACHABLE or NIL
	     (Inherit-$Value attribute object nil inheritance-opportunity)
	   (cond ((not inherited?)
		  (if suppress-error?
		      (values nil :error)
		      (let ((error (make-condition
				     'no-value-for-attribute
				     "The object, ~a, does not have the attribute, ~a, ~
                                     and no value was inherited"
				     object attribute)))
			(multiple-value-bind (proceed-type value)
			    (signal error :proceed-types
				    '(:new-value :retry-operation :store-n-return-new-value))
			  (ccase proceed-type
			    (:new-value value)
			    (:retry-operation ($value object attribute suppress-error? all-values-p))
			    (:store-n-return-new-value
			     ;; add the attribute and value
			     (add-attribute-to-object attribute value object)
			     value))))))
		 ((eql (car inherited-value) ;;if cachable, cache and return
		       'CACHABLE)
		  (add-attribute-to-object
		    attribute (cdr inherited-value) object)
		  (if all-values-p
		      (list (cdr inherited-value))
		      (cdr inherited-value)))
		 ((and (listp (cdr inherited-value)) ;;if inheriting a procedure, bring it down, and recur.
		       (eql (cadr inherited-value)   ;;next time through the procedure will be used.
			    '$PROCEDURE))
		  (add-attribute-to-object
		    attribute (cdr inherited-value) object)
		  ($value object attribute suppress-error? all-values-p)) 
		 (t ;; value is not cachable, so just return it
		  (cdr inherited-value)))))
	;;;Must have an error
	(suppress-error? (values nil :error))	   ;do nothing. This "error" will be suppressed by some internal functions.
	(t (let ((error (make-condition 'no-value-for-attribute
		     "The object, ~a, does not have the attribute, ~a, and there is no way to inherit a value"
					object attribute)))
	     (multiple-value-bind (proceed-type value)
		 (signal error :proceed-types (list :new-value :retry-operation :store-n-return-new-value))
	       (ccase proceed-type
		 (:new-value value)
		 (:retry-operation ($value object attribute suppress-error? all-values-p))
		 (:store-n-return-new-value
		  ;; add the attribute and value
		  (add-attribute-to-object attribute value object)
		  value)))))))


;;; define a setf method for $value
;;; typical call:  (setf ($value 'solution.path.path1 'length) 34)
;;; does not make an event.
;;;Edited by HASTINGS   8 May 87  0:00
(defsetf $value (obj atr) (new-value)
  `(cond ((valid-attribute-for-object? ,atr ,obj)
	  (store-attribute ,obj ,atr ,new-value)
	  ,new-value)
	 (t (error "The attribute, ~a, is not defined for the object, ~a, ~
                    ~&while trying to do (setf ($value '~a '~a) ~a)"
		   ,atr ,obj ,obj ,atr ,new-value))))

;;;-------  $VALUES  -------

;;;Edited by Anonymous   27 Apr 87  23:08
;;;Edited by Reed Hastings   27 Apr 87  23:11
;;;Edited by Reed Hastings   28 Apr 87  1:46
(defun $Values (object attribute &OPTIONAL (suppress-error? nil))
  "Like $VALUE except returns a list of all values ever stored on the attribute.
If *BB1-SAVE-MEMORY-FLAG* is non-nil, only one value at a timewill be saved by
the Blackboard Manager, but $Values will return that value in a list anyway."
  ($value object attribute suppress-error? :return-all-values))


;;;-------  $VALUE-2  -------

(defun $Value-2 (object attribute &OPTIONAL (suppress-error? nil))
  "Low-level function used internally during attribute inheritance."

  ;;Just like $VALUE, except does no error checking

  (catch '$VALUE-2-VALUE        ;;;Will only catch true values being returned.  Otherwise returns whatever BB1ERROR returns.

    (let* ((long-attribute-name     (valid-attribute-for-object?        attribute object))  ;; SIDE-EFFECT
	   (proc                    (procedural-attachment-of-attribute long-attribute-name))
	   values
	   inheritance-opportunity
	   )

 ;; First, try procedural attachment

      (if proc
	  (throw '$VALUE-2-VALUE
		 (values t `($PROCEDURE ,proc)))
	)

 ;; Second, try direct retrieval

      (if (setq values (values-of-attribute long-attribute-name))
	  (throw '$VALUE-2-VALUE
		 (values t (car values)))
	)

 ;; Third, Is there really an attribute there?

      (if (valid-attribute? long-attribute-name)
	  (throw '$VALUE-2-VALUE
		 (values t nil))                                       ;; YES, and the value is NIL
	)


 ;; ELSE - We know that the object is valid, have to check for inheritance.

      (if (valid-attribute-for-system? attribute *BB1-SYSTEM-NAME)
	
	   ;;;EVERYTHING is valid, check for inheritance...
	
	  (if (setq inheritance-opportunity
		    (inheritance-opportunity-of-attribute attribute))
	      (throw '$VALUE-2-VALUE
		     (inherit-$VALUE attribute object nil inheritance-opportunity nil))
	    (or suppress-error?
		(bb1warning '$VALUE-2
			    attribute " is not an attribute of " object
			    " and is not inheritable.")
		)
	    )
	;Illegal attribute is not an error here.
	(throw '$VALUE-2-VALUE (values nil nil))
	)
      )
    )
  )



;;;-------  INHERIT-$VALUE  -------

(defun Inherit-$Value (attribute current-object objects-examined inheritance-paths
			 &OPTIONAL (suppress-error? nil)
			 &AUX inherited? inherited-value)
  "Used to inherit attributes of an object, if the attribute doesn't exist on the original object referenced."

  ;;$VALUE and $VALUES will take care of caching values

  ;;This function returns 2 values:  The first is a flag which is non-NIL if inheritance was successful
  ;;                                 The second is a list of (CACHABLE? objects).

  (if (null inheritance-paths)
      (values nil nil)
    (progn (multiple-value-setq (inherited? inherited-value)
				(Inherit-$Value-2 attribute current-object objects-examined
						  (car inheritance-paths)))
	   (if inherited?
	       (values inherited? inherited-value)
	     (Inherit-$Value attribute current-object objects-examined
			     (cdr inheritance-paths) suppress-error?)
	     )
	   )
    )
  ) ; Inherit-$Value



;;;-------  INHERIT-$VALUE-2  -------

(defun Inherit-$Value-2 (attribute current-object objects-examined
			 inheritance-path)
  "Internal function used to inherit attributes."

  ;;Returns 2 values:  1) Whether the inheritance was successful
  ;;                    2) Either NIL or a list (CACHABLE? objects)

  (if (member current-object objects-examined)    ;Don't want to get into a loop!
      (values nil nil)

    ;;ELSE - go ahead and try to inherit
    (let ((link-name    (car inheritance-path))
	  (inherit-type (cdr inheritance-path))
	  (inherited?   nil)
	  (retval       nil))

      (dolist (new-object ($Objects current-object link-name 'suppress-error))
	(multiple-value-setq (inherited? retval)
		  ($Value-2 new-object attribute 'suppress-error))
	(if inherited?
	    (return 'found)))

      (if retval
	  (values inherited? (cond ((atom retval)   (cons (if inherit-type 'CACHABLE 'NON-CACHABLE) retval))
				   ((listp retval)  (if (member (car retval) '(CACHABLE NON-CACHABLE))
							(cons (if inherit-type 'CACHABLE 'NON-CACHABLE)
							      (cdr retval))
							;;ELSE
							(cons (if inherit-type 'CACHABLE 'NON-CACHABLE)
							      retval)))
				   (t               (cons (if inherit-type 'CACHABLE 'NON-CACHABLE) retval)))
		  )
	;;ELSE - no inheritance path was viable
	nil)
      )
    )
  )



;;;-------  $OBJECT  -------

(defun $Object (object link &OPTIONAL (suppress-error? nil))
  "Returns the first object that the given object is linked to by link.
Pictorially,

       object  --- link ----------> object2
                     |
                     -------------> object3
                     |
                     ...

object2 is returned."

  (car ($Objects object link suppress-error?))        ;- and so, paradoxically, $OBJECT takes longer to run than $OBJECTS...
  )


;;;-------  $OBJECTS  -------

(defun $Objects (object link &OPTIONAL (suppress-error? nil))
  "Returns the list of objects who are linked to object via link.
Note that 'link' must be from object to the other objects."

;;Optimized  11/06/86  -mh

  (catch '$OBJECTS-VALUE        ;;;Will only catch true values being returned.  Otherwise returns whatever BB1ERROR returns.

    (let* ((long-link-name  (valid-link-for-object?  link object))  ;; SIDE-EFFECT
	   (link-info       (if long-link-name (get-bb-link-data long-link-name)))
	   )

      (if long-link-name	  ;LINK IS THERE.  RETRIEVE OBJECTS

	  (throw '$OBJECTS-VALUE
		 (copy-tree (bb-linkinforec-linked-objects link-info)))
	  )

        ;; Either have invalid object, invalid-link, or opportunity for inheritance.

      (let ((object-info  (get-bb-object-data object))
	    (link-info    (get-bb-shortlink-data link))
	    )
	
	(if object-info      ;VALID-OBJECT?
	    
	    (if link-info    ;VALID-LINK?
		
		;;;EVERYTHING is valid, check for inheritance...
		
		(let ((inheritance-opportunities
			(bb-shortlinkinforec-inheritance-opportunities link-info))
		      inherited? inherited-objects)
		  
		  (if inheritance-opportunities
		      (multiple-value-setq
			(inherited? inherited-objects)
			(Inherit-$Objects link object nil inheritance-opportunities suppress-error?))
		      ;;ELSE
		      (throw '$OBJECTS-VALUE nil))    ;no inheritance opportunities is not an error in $OBJECTS.
		  
		  (if inherited?
		      
		      ;;Objects is in the form (x . objects), where x = CACHABLE or NIL
		      (progn
			(if (member (car inherited-objects)       ;;NEED to do something different
				    '(BB1::CACHABLE USER::CACHABLE))        ;;If value is CACHABLE
			    ;; Need to do some caching
			    (dolist (to-object (cdr inherited-objects))
			      (add-link-from-object-to-object link object to-object)
			      (add-link-from-object-to-object (bb-shortlinkinforec-opplink link)
							      to-object object)
			      )
			    )
			
			(throw '$OBJECTS-VALUE
			       (copy-tree (cdr inherited-objects)))   ;NORMAL RETURN
			)

		        ;;ELSE - link wasn't able to be inherited.
		      (if suppress-error?
			  nil
			  ;;ELSE
			  (bb1warning '$OBJECTS
				      "Unable to inherit " link " for " object "."))
		      )
		  )  ;;END of LET
		
		;;ELSE - this wasn't even a valid link reference
		(bb1error '$OBJECTS
			  link " is not a link in the " *BB1-SYSTEM-NAME* " system.")
		)
	    ;;;ELSE - finally, the object they are referencing does not even exist!
	    (bb1error '$OBJECTS
		      object " is not an existing blackboard object.")
	    )
	)
      )
    )
  )


;(defun $Objects (object link &OPTIONAL (suppress-error? nil))
;  "Returns the list of objects who are linked to object via link.
;Note that 'link' must be from object to the other objects."


;  (catch '$OBJECTS-VALUE        ;;;Will only catch true values being returned.  Otherwise returns whatever BB1ERROR returns.

;    (let* ((long-link-name     (valid-link-for-object?  link object))  ;; SIDE-EFFECT
;	   objects
;	   inheritance-opportunity
;	   inherited?
;	   inherited-objects)


; ;; First, try direct retrieval

;      (if (setq objects (linked-objects-of-link long-link-name))
;	  (throw '$OBJECTS-VALUE
;		 (copy-tree objects))      ;; <- Here we return all links      ************
;	)

; ;; Second, there really a link there?

;      (if (valid-link? long-link-name)
;	  (throw '$OBJECTS-VALUE nil)               ;; YES, with no objects attached
;	)

; ;; ELSE - Either have an invalid object, invalid attribute, or an opportunity for inheritance

;      (if (valid-object? object)
;	  (if (valid-link-for-system? link *BB1-SYSTEM-NAME*)
	
;	        ;;;EVERYTHING is valid, check for inheritance...
	
;	      (if (setq inheritance-opportunity
;			(inheritance-opportunity-of-link link))
;		  (progn
;		    (multiple-value-setq
;		      (inherited? inherited-objects)
;		      (Inherit-$Objects link object nil inheritance-opportunity suppress-error?))
;		    (if inherited?
			
;			;;Objects is in the form (x . objects), where x = CACHABLE or NIL
			
;			(progn (if (eql (car inherited-objects)       ;;NEED to do something different
;					'CACHABLE)                  ;;If value is CACHABLE
				
;				    ;; Need to do some caching
;				   (dolist (to-object (cdr inherited-objects))
;				     (add-link-from-object-to-object link object to-object)
;				     (add-link-from-object-to-object (opposite-link-of-link link)
;								     to-object object)
;				     )
;				 )
			
;			       (throw '$OBJECTS-VALUE
;				      (copy-tree (cdr inherited-objects)))   ;NORMAL RETURN
;			       )

;		        ;;ELSE - link wasn't able to be inherited.
;		      (if suppress-error?
;			  nil
;			;;ELSE
;			(bb1warning '$OBJECTS
;				    "Unable to inherit " link " for " object "."))
;		      )
;		    )  ;;END of PROGN
		
;		;;ELSE - no inheritance opportunities exist  -  not an error in $OBJECTS
;		(throw '$OBJECTS-VALUE nil)
;		)
;	     ;;ELSE - this wasn't even a valid link reference
;	    (bb1error '$OBJECTS
;		      link " is not a link in the " *BB1-SYSTEM-NAME* " system.")
;	    )
;          ;;;ELSE - finally, the object they are referencing does not even exist!
;	(bb1error '$OBJECTS
;		  object " is not an existing blackboard object.")
;	)
;      )
;    )
;  )



;;;-------  $OBJECTS-2  -------

(defun $Objects-2 (object link)
  "This is a lower-level function called by INHERIT-$OBJECTS."

  ;; This is like $Objects except that no error-checking is done.
  ;; Returns a single value - a list of objects.


  (catch '$OBJECTS-2-VALUE

    (let ((long-link-name (valid-link-for-object? link object))
	  linked-objects
	  inheritance-opportunity
	  inherited?
	  inherited-objects)

      ;First, is this a valid link?

      (if (setq linked-objects (linked-objects-of-link long-link-name))
	  (throw '$OBJECTS-2-VALUE
		 linked-objects))

      ;Second, is it a valid link with no attached objects?

      (if (valid-link? long-link-name)
	  (throw '$OBJECTS-2-VALUE
		 nil))

      ;Finally, may have to inherit it

      (if (setq inheritance-opportunity
		(inheritance-opportunity-of-link link))
	  (progn
	    (multiple-value-setq
	      (inherited?  inherited-objects)
	      (Inherit-$Objects link object nil inheritance-opportunity nil))
	    (if inherited?
		(progn
		  (if (eql (car inherited-objects)
			   'CACHABLE)
		      (dolist (to-object (cdr inherited-objects))
			(add-link-from-object-to-object link object to-object)
			(add-link-from-object-to-object (opposite-link-of-link link) to-object object)
			)
		    )
		  (throw '$OBJECTS-2-VALUE
			 (cdr inherited-objects))
		  )
	      ;;ELSE, wasn't inherited on this link - not an error
	      (throw '$OBJECTS-2-VALUE nil)
	      )
	    ) ; end of PROGN
	;;ELSE - no inheritance opportunity - not an error
	(throw '$OBJECTS-2-VALUE nil)
	)
      )
    )
  )  ;$OBJECTS-2


	
;;;-------  INHERIT-$OBJECTS  -------

(defun Inherit-$Objects (link current-object objects-examined inheritance-paths
			 &OPTIONAL (suppress-error? nil)
			 &AUX inherited? inherited-objects)
  "Used to inherit objects of a link, if the link doesn't exist on the original object referenced."

  ;;Used for both $Object and $Objects
  ;;They will take care of caching values.

  ;;This function returns 2 values:  The first is a flag which is non-NIL if inheritance was successful
  ;;                                 The second is a list of (CACHABLE? objects).

  (if (null inheritance-paths)
      (values nil nil)
    (progn (multiple-value-setq (inherited? inherited-objects)
				(Inherit-$Objects-2 link current-object objects-examined
						    (car inheritance-paths)))
	   (if inherited?
	       (values inherited? inherited-objects)
	     (Inherit-$Objects link current-object objects-examined
			       (cdr inheritance-paths) suppress-error?)
	     )
	   )
    )
  ) ; Inherit-$Objects



;;;-------  INHERIT-$OBJECTS-2  -------

(defun Inherit-$Objects-2 (link current-object objects-examined
			   inheritance-path)
  "Internal function used to inherit links."

  ;;Returns 2 values:  1) Whether the inheritance was successful
  ;;                    2) Either NIL or a list (CACHABLE? objects)

  (if (member current-object objects-examined)    ;Don't want to get into a loop!
      (values nil nil)

    ;;ELSE - go ahead and try to inherit
    (let ((link-name    (car inheritance-path))
	  (inherit-type (cdr inheritance-path))
	  (retval       nil))

      (dolist (new-object ($Objects current-object link-name))
	(if (setq retval
		  ($Objects-2 new-object link))
	    (return 'found)))
      (if retval
	  (values 'inherited (cons inherit-type retval))
	;;ELSE - no inheritance path was viable
	(values nil nil))
      )
    )
  ) ; Inherit-$Objects-2



;;;-------  $ALL-OBJECTS-LINKED-TO  -------

(defun $all-objects-linked-to (object &OPTIONAL (links nil) (use-inheritance? nil))
  "Returns all objects linked to the given object.  If USE-INHERITANCE?
is non-NIL, inherited links will be counted too.
LINKS should be short names."

  (if (valid-object? object)

      (progn  (if (null links)
		  (if use-inheritance?
		      (setq links (all-links-of-system *BB1-SYSTEM-NAME*))
		    (setq links (mapcar #'car (links-of-object object))))
		)
	
	      (if use-inheritance?
		  (let ((linked-objects nil))
		    (dolist (link links)
		      (setq linked-objects
			    (union linked-objects
				   ($objects object link 'suppress-error))))
		    linked-objects)
		
	          ;;ELSE - just use direct links.
		(let ((linked-objects nil))
		  (dolist (link (remove-if-not #'(lambda (long-link-name)
						   (member (short-name-of-link long-link-name)
							   links))
					       (actual-links-of-object object)))
		    (setq linked-objects
			  (union linked-objects
				 (copy-list (linked-objects-of-link link))))
		    )
		  linked-objects
		  )
		)
	      ); PROGN-END

    ;;ELSE - don't have a valid object
    (bb1error '$ALL-OBJECTS-LINKED-TO
	      object " is not an existing blackboard object.")
    )
  )



;;;-------  $IS-LINKED-TO  -------

;;;Edited by Mike Hewett   28 Apr 87  10:36
(defun $Is-Linked-To (start-object to-object-name search-levels search-links)
  "This is a generic procedure which does a depth-first search of the blackboard
to find an object whose short-name is 'to-object-name' which is linked
to 'object' by any sequence of 'search-links'.  'To-object-name' is to
be found on any of 'search-levels'.

If 'search-levels' is NIL, all of the blackboard(s) will be searched.
If 'search-links' is NIL, all of the existing links in the system will
be used.

'Search-links' go out from start-object and into to-object-name.

This may take a long time to run if you don't restrict the search!"

  ;;This procedure was implemented as a template for $ISA, $PLAYS, etc.

  (if (valid-object? start-object)
      (if to-object-name
	  (progn
	    (if (null search-levels)
		(setq search-levels (levels-of-system *BB1-SYSTEM-NAME*)))
	
	    (if (null search-links)
		(setq search-links (all-links-of-system *BB1-SYSTEM-NAME*)))
	
	    (if (eql to-object-name
		     (short-name-of-object start-object))
		start-object                              ;;TRIVIAL CASE
	      ($is-linked-to-1 start-object to-object-name search-levels search-links)  ;;typical case
	      )
	    ) ;PROGN-END
	;;ELSE - they gave a null TO-OBJECT-NAME
	(bb1error '$IS-LINKED-TO
		  "The object to search for is NIL.")
	)
    ;;ELSE - invalid object to start with
    (bb1error '$IS-LINKED-TO
	      start-object " is not an existing blackboard object.")
    )
  )


;;;-------  $IS-LINKED-TO-1  -------

(defun $IS-Linked-To-1 (start-object goal-short-name search-levels search-links)
  "Secondary function to $IS-LINKED-TO."

  ;; First, cut down on the number of objects to search.  Name must be equal to goal, and must have links.

  (let ((goal-objects (remove-if-not #'(lambda (obj)
					 (and (eql (short-name-of-object obj)
						   goal-short-name)
					      (links-of-object obj))
					 )
				     (mapcan #'(lambda (lev)
						 (copy-list (objects-of-level lev)))
					     search-levels)
				     ))
	(next-search-objects nil)
	(found-objects nil))

    ;; GOAL-OBJECTS now has only those objects whose short name is the same as goal name and who have links.
    ;; It should be much easier now to find the desired object.


    (setq next-search-objects
	  ($All-Objects-Linked-To start-object search-links))

    (if (setq found-objects
	      (intersection goal-objects next-search-objects))
	(car found-objects)
      ;;ELSE - have to search further
      ($Is-Linked-To-2 next-search-objects goal-objects search-links (cons start-object
									   next-search-objects))
      )
    )
  )


;;;-------  $IS-LINKED-TO-2  -------

(defun $Is-Linked-To-2 (start-objects goal-objects search-links already-scanned-objects)
  "Secondary function to $IS-Linked-To-1.  Does the actual search through the blackboard.
Search is depth-first."

  (catch 'FOUND-OBJECT

    (let ((next-search-objects nil)
	  (found-objects       nil))

      (dolist (obj start-objects)
	(setq next-search-objects (set-difference ($all-objects-linked-to obj search-links nil)
						  already-scanned-objects))
	(if (setq found-objects
		  (intersection goal-objects next-search-objects))
	    (throw 'FOUND-OBJECT (car found-objects))
	   ;;ELSE - have to search further
	  (if (setq found-objects
		    ($Is-Linked-To-2 next-search-objects goal-objects search-links (cons obj
											 next-search-objects)))
	        (throw 'FOUND-OBJECT found-objects)  ;;WILL actually be only one object
	    )
	  )
	)
      nil     ;NOTHING FOUND
      )
    )
  )



;;;-------  $PROCEDURE-VALUE  -------

(defun $Procedure-Value (object attribute)
  "Returns the last value obtained from a procedural attachment."

  (let ((long-attribute-name (valid-attribute-for-object? attribute object)))

    (if (procedural-attachment-of-attribute long-attribute-name)
	(car (values-of-attribute long-attribute-name))
      ;;ELSE have error conditions
      (if (valid-attribute? long-attribute-name)
	  (bb1error '$PROCEDURE-VALUE
		    attribute " of " object " does not have a procedural attachment.")
	;;---
	(if (valid-object? object)
	    (bb1error '$PROCEDURE-VALUE
		      attribute " is not an attribute of " object ".")
	  (bb1error '$PROCEDURE-VALUE
		    object " is not a blackboard object.")
	  )
	)
      )
    )
  )   ;$PROCEDURE-VALUE


;;;-------  $PROCEDURE-VALUES  -------

(defun $Procedure-Values (object attribute)
  "Returns all values obtained from a procedural attachment.
Note that the saving of values is dependent on the value of *BB1-SAVE-MEMORY-FLAG*."

  (let ((long-attribute-name (valid-attribute-for-object? attribute object)))

    (if (procedural-attachment-of-attribute long-attribute-name)
	(copy-anything (values-of-attribute long-attribute-name))
      ;;ELSE have error conditions
      (if (valid-attribute? long-attribute-name)
	  (bb1error '$PROCEDURE-VALUES
		    attribute " of " object " does not have a procedural attachment.")
	;;---
	(if (valid-object? object)
	    (bb1error '$PROCEDURE-VALUES
		      attribute " is not an attribute of " object ".")
	  (bb1error '$PROCEDURE-VALUES
		    object " is not a blackboard object.")
	  )
	)
      )
    )
  )   ;$PROCEDURE-VALUES



;;;-------  $FIND  -------

(defun $Find (level attrs/links &OPTIONAL (all? nil))
  "Finds all objects on the given level whose attributes or links
have the given values.

  Example:  ($FIND 'BUILDINGS.HOUSES  '((LENGTH 47)
                                        (WIDTH  36)))
"
  (if (not (listp (car attrs/links)))
      (bb1error '$FIND
		"The second argument, " attrs/links ", should be an attribute-value list.")
    ;;ELSE - do normal processing

    (let ((searchnodes (if level
			   ($Objects-At-Level level)
			 ;;ELSE - search whole blackboard
			 (find-search-space attrs/links))))
      (if all?
	  (searchfor searchnodes (eval-attribute-value-list attrs/links))
	(car (searchfor searchnodes (eval-attribute-value-list attrs/links)))
	)
      )
    )
  )   ;;$FIND



;;;-------  $FIND-ALL  -------

(defun $Find-All (level attrs/links)
  "Returns all objects with the given attribute/link values.
See $FIND for an example."

  ($Find level attrs/links 'all)
  )



;;;-------  $FIND-IF   -------

(defun $find-if (predicate &key levels blackboards)
  "This fn is like $FIND except that a user-definable predicate is allowed.  A typical call might be:
    ($FIND-IF #'(LAMBDA (OBJ) (EQL ($SHORT-NAME OBJ) 'HELIX)) 'SOLUTION.SOLID NIL)"

  (find-if predicate
	   (cond 
	     (levels 
	      (mapcan #'$objects-at-level (mklist levels)))
	     (blackboards
	      (mapcan #'(lambda (bb) (objects-of-blackboard bb)) (mklist blackboards)))
	     (t
	      (objects-of-system *bb1-system-name*)))))


;;;-------  $FIND-ALL-IF   -------

;;;Edited by Bob Schulman   5 May 87  21:31
;;;Edited by Mike Hewett           30 Jun 87  15:40
(defun $find-all-if (predicate &key levels blackboards)
  "This fn is like $FIND except that a user-definable predicate is allowed.  A typical call might be:
    ($FIND-IF #'(LAMBDA (OBJ) (EQL ($SHORT-NAME OBJ) 'HELIX)) :levels 'SOLUTION.SOLID)"

  (delete-if-not   predicate
		   (cond 
		      (levels 
		       (mapcan #'$objects-at-level (mklist levels)))
		      (blackboards
		       (mapcan #'(lambda (bb) (objects-of-blackboard bb)) (mklist blackboards)))
		      (t
		       (objects-of-system *bb1-system-name*)))))


;;;-------  $GENERATOR  -------

;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Mike Hewett           30 Jun 87  11:36
;;;Edited by Mike Hewett           30 Jun 87  15:40
(defun $generator (patterns weights sort)
   "Generates a list of parameter values that satisfy the input patterns

PATTERNS is a list of the forms (is-a <modifiers> concept)  or  (link-name <modifiers> concept).
WEIGHTS are weights of the modifiers, e.g. ((long 1) (large 2))
SORT is INSTANCE, INDIVIDUAL, or TYPE
"

   ;; Given a list of patterns, the weights of the components of the phrase, and optionally, the sort of the object 
   ;; (instance, individual, or type) that can be used to refine that sentence, return the list of objects that match
  ;; the patterns, ranked from best-match down to worst-match.
  
  ;; Patterns are of the form (is-a <modifiers> concept) or (link-name <modifiers> concept).
  
  ;; The first pattern is used to generate a list of objects, the subsequent patterns are used to filter the list 
  ;; down.
  
  ;; Example: ($generator  '((long helix))) '((helix 1) (long 2))) 'INSTANCE) will return a list 
  ;;          of all helix instances. The list will start with the longest helix, and end with the shortest.
  ;;          It is assumed that the modifier "long" can be applied to instances.
  
  (let ((generator-phrase (reverse (cdar patterns)))
	(link-name (caar patterns))
	blackboards levels)
    (apply #'generator-rater
	   (generator-filter (cond
			       ((eql link-name 'USER::IS-A)
				($objects-of-type (let ((type (car generator-phrase)))
						    (cond ((valid-object? type)
							   type)
							  (t (long-name type))))
						  (or sort 'USER::ALL)))
			       (t
				;; Need to follow some other link
				(setq blackboards (blackboards-of-system *bb1-system-name*))
				(setq levels (mapcan #'(lambda (bb)
							 (copy-list (levels-of-blackboard bb)))
						      blackboards))
				($find-all-if #'(lambda (obj)
						  (apply link-name
							 (list obj (car generator-phrase))))
					      :levels levels)))
			     (cdr patterns)
			     (cdr generator-phrase)
			     weights))))

;;;-------  $HAS-VARIABLE  -------

;;;Edited by Bob Schulman   5 May 87  21:31
(defun $HAS-VARIABLE (ksar variable-name)
  "Returns a non-NIL value of the given KSAR has a local variable
  (bound or context) of the given name."

  ;; Mostly used by internal functions

  (if (and (valid-object? ksar)
	   (eql (level-of-object ksar)
		(make-extended-name 'CONTROL-DATA 'KSAR)))
      (member variable-name (car ($value ksar 'USER::boundvars)))

    ;;ELSE - is not a KSAR
    (bb1error '$HAS-VARIABLE
	      ksar " is not a KSAR.")
    )
  )


;;;-------  $VARIABLE  -------

(defun $Variable (ksar variable-name)
    "Returns the value of the local variable of a KSAR."

  ;; Mostly used by internal functions

   (if (and (valid-object? ksar)
	   (eql (level-of-object ksar)
		(make-extended-name 'CONTROL-DATA 'KSAR)))
       (if (member variable-name (car ($value ksar 'USER::boundvars)))
	   ($Eval variable-name ksar)
	  nil)

    ;;ELSE - is not a KSAR
    (bb1error '$VARIABLE
	      ksar " is not a KSAR.")
    )
   )





;;;-------  $LAST-OBJECT-ADDED  -------

(defun $Last-Object-Added ()
  "Returns the last object added to any blackboard.
Searches through past events, so may take a long time."

  (let ((last-add-event (car (member-if #'(lambda (event)
					    (eql ($Value event 'USER::ChangeType)
						 'USER::ADD))
					(reverse (objects-of-level (make-extended-name
								     'CONTROL-DATA 'EVENTS))))))
	)
    (if last-add-event
	($Value last-add-event 'USER::ObjectName)    ;Normal termination
      ;;ELSE
      nil)   ;No ADD events yet.
    )
  )



;;;-------  $NEWEST-OBJECT  -------

(defun $Newest-Object (level)
  "Returns the object most recently added to the given level."

  (if (valid-level? level)
      (car (last (objects-of-level level)))
    ;;ELSE - error
    (bb1error '$NEWEST-OBJECT
	      level " is not an existing blackboard level.")
    )
  )



;;;-------  $NAME  -------

(defun $Name (object new-name)
  "Renames an existing object to a new name."

  (rename-object object new-name)       ;Call low-level blackboard function.
  )


;;;-------  $INPUT  -------

(defun $Input (default-value)
  "Allows the user to enter an expression in an editor window."

  (bb1-input-expression default-value)
  )



;;;-------  $INPUT-ATOM  -------

(defun $Input-Atom (query &OPTIONAL (default-atom nil))
  "Allows the user to enter an atom in a pop-up window."

  (bb1-input-atom default-atom query)
  )


;;;-------  $INPUT-Y-OR-N  -------

(defun $Input-y-or-n (query)
  "Allows the user to answer yes or no."

  (bb1-y-or-n-p query)
  )


;;;-------  $OUTPUT  -------

;;;Edited by Mike Hewett   1 May 87  10:38
(defun $Output (&REST msg-elements)
  "MSG-ELEMENTS are strings, atoms, numbers, T or NIL.
They are printed on a temporary window by PRINC
The element 'T' will cause a carriage return to be printed."

  (bb1-with-help-stream (msg-stream :label "Message from user"
				    :superior (or *bb1-bb-window*
						  *standard-output*))

       (dolist (item msg-elements)
	 (if (eql item t)
	     (terpri msg-stream)
	   (princ item msg-stream)
	   )
	 )
       )
  )


;;;-------  $MESSAGE  -------

(defun $Message (&REST msg-elements)
  "MSG-ELEMENTS are strings, atoms, numbers, T or NIL.
They are printed on the small input window by PRINC
The element 'T' will cause a carriage return to be printed."

  (apply #'display-bb1-message msg-elements)
  )

;;;-------  $CAUSES  -------

(defun $CAUSES (object concept-type &OPTIONAL (not-cachable? nil))
  "Determines whether OBJECT is linked to CONCEPT-TYPE via an IS-A link."

  ($link object concept-type '(USER::causes USER::is-a
			       USER::instantiates USER::exemplifies) '$CAUSES- not-cachable?)
  )


;;;-------  $COMPRISED-BY  -------

;;;Edited by Mike Hewett                27 May 87  19:32
(defun $COMPRISED-BY (object target-compriser  &OPTIONAL (cachable? nil))
  "Predicate for whether OBJECT is COMPRISED-BY TARGET-COMPRISER; 
    return long name of TARGET-COMPRISER if true.  Note that $OBJECT is used for PARENT
    instead of $OBJECTS, because the links followed do not fan out, and that an object can have only one of 
    (INSTANTIATES EXEMPLIFIES IS-A) links."

  ;; Currently doesn't implement CACHABLE? arg.

  (let ((compriser ($object object 'USER::comprised-by))
	parent)
    (cond 
      ((eql (short-name-of-object compriser) target-compriser)
       compriser)
      ((and compriser
	    (setq parent ($comprised-by compriser target-compriser cachable?)))
       parent)
      ((setq parent (or ($object object 'USER::instantiates)
			($object object 'USER::exemplifies)
			($object object 'USER::is-a)))
       ($comprised-by parent target-compriser cachable?)))))


;;;-------  $COMPRISES  -------

;;;Edited by Mike Hewett                27 May 87  19:32
(defun $COMPRISES (compriser target-constituent &OPTIONAL (cachable? nil))
  "Predicate for whether COMPRISER COMPRISES TARGET-CONSTITUENT.  Return long name
    of TARGET-CONSTITUENT if true."

  ;; Currently doesn't implement CACHABLE? arg.

  (some #'(lambda (constituent)
	    ($comprises-aux constituent target-constituent cachable?))
	($objects compriser 'USER::comprises)))


;;;Edited by Mike Hewett                27 May 87  19:32
(defun $COMPRISES-AUX (constituent target-constituent  &OPTIONAL (cachable? nil))
  (cond
    ((or (eql (short-name-of-object constituent)
	      target-constituent)
	 (member target-constituent (short-names ($objects constituent 'USER::plays))))
     constituent)
    (($comprises constituent target-constituent cachable?))
    ((some #'(lambda (child)
	       ($comprises-aux child target-constituent cachable?))
	   (or ($objects constituent 'USER::instantiated-by)
	       ($objects constituent 'USER::exemplified-by)
	       ($objects constituent 'USER::can-be-a))))))


;;;-------  $ENABLES  -------

(defun $ENABLES (object concept-type &OPTIONAL (not-cachable? nil))
  "Determines whether OBJECT is linked to CONCEPT-TYPE via an IS-A link."

  ($link object concept-type '(USER::enables USER::is-a USER::instantiates USER::exemplifies)
	 '$ENABLES- not-cachable?)
  )


;;;-------  $ENTAILS  -------

;;;Edited by Bob Schulman   5 May 87  21:31
(defun $ENTAILS (object concept-type &OPTIONAL (not-cachable? nil))
  "Determines whether OBJECT is linked to CONCEPT-TYPE via an IS-A link."

  ($link object concept-type '(USER::entails USER::is-a USER::instantiates USER::exemplifies)
	 '$ENTAILS- not-cachable?)
  )


;;;-------  $EXAMPLES-OF  -------

;;;Edited by Bob Schulman   5 May 87  21:31
(defun $EXAMPLES-OF (type)
  "Follow CAN-BE-A links down to final EXEMPLIFIED-BY links to find examples of the TYPE object."

  (append ($objects type 'USER::exemplified-by)
	  (mapcan #'$examples-of ($objects type 'USER::can-be-a))))

;;;-------  $INSTANCES-OF  -------

;;;Edited by Bob Schulman   5 May 87  21:31
(defun $INSTANCES-OF (type)
  "Follow CAN-BE-A links down to final INSTANTIATED-BY links to find examples of the TYPE object."

  (append ($objects type 'USER::instantiated-by)
	  (mapcan #'$instances-of
		  (append ($objects type 'USER::can-be-a)
			  ($objects type 'USER::exemplified-by)))))

;;;-------  $INCLUDES  -------

;;;Edited by Vaughan Johnson       23 Jul 87  17:02
(defun $INCLUDES (object concept-type &OPTIONAL (not-cachable? nil))
  "Determines whether OBJECT is linked to CONCEPT-TYPE via an IS-A link."

  ($link object concept-type '(USER::includes USER::is-a USER::instantiates USER::exemplifies)
	 '$INCLUDES- not-cachable?)
  )


;;;-------  $INPUT-TO  -------

;;;Edited by Vaughan Johnson       23 Jul 87  17:02
(defun $input-to (input-object target-short-name &OPTIONAL (not-cachable? nil))
  "Return the long name of target-short-name if input-object or an object it $ISA 
   is input-to target-short-name."

  (cond
    ((not input-object)    nil)		   ; to stop the recursion
    ((find-if #'(lambda (pblm) (eql ($short-name pblm) target-short-name))
	      ($objects input-object 'user::input-to)))
    (($input-to (or ($object input-object 'user::is-a)
		    ($object input-object 'user::exemplifies)
		    ($object input-object 'user::instantiates))
		target-short-name
		not-cachable?))))


;;;-------  $ISA  -------

;;;Edited by Vaughan Johnson       23 Jul 87  17:02
(defun $ISA (object concept-type &OPTIONAL (not-cachable? nil))
  "Determines whether OBJECT is linked to CONCEPT-TYPE via an IS-A link."

  ($link object concept-type '(USER::is-a USER::instantiates USER::exemplifies) '$ISA- not-cachable?)
  )




;;;-------  $LINK  -------

;;;Edited by Mike Hewett   28 Apr 87  10:36
(defun $link (object concept-type search-links cache-name &OPTIONAL (not-cachable? nil))
  "Searches for concept-type, linked to object via any of the search-links.  Optionally caches the
result in the OBJECT array under the $LINKS field."

  (cond ((or (eql object concept-type)
	     (and (valid-object? object)
		  (eql (short-name-of-object object) concept-type)))              t)
	
	(t    (let* ((cached-$links  (cached-$links-of-object object))
		     (cached-result  (and cached-$links (assoc (pack* cache-name concept-type)
							       cached-$links)))
		     result)
		
		(if cached-result
		    (cdr cached-result)
		    ;;ELSE - have to do a lookup
		    (setq result ($TYPE-OF object concept-type search-links))
		    (unless not-cachable?
		      (add-to-object-array object nil nil nil nil nil
					   (cons (pack* cache-name concept-type) result)
					   'update)
		      )
		    result)
		)
	      )
	)
  )



;;;-------  $MODIFIED-BY  -------

;;;Edited by Bob Schulman   5 May 87  21:31
(defun $MODIFIED-BY (object modifier-short-name &OPTIONAL (cachable? nil))
  "Determines whether OBJECT is modified by MODIFIER-SHORT-NAME.   Returns
long name of the object on which the name of the function defining the modifier is stored."

  (let ((modifiers ($Objects object 'USER::modified-by))
	superordinates modifier-long-name)

    (cond ((find modifier-short-name
		 modifiers
		 :key #'(lambda (mod) (short-name-of-object mod))
		 ))

	  ((and (setq superordinates (nconc ($Objects object 'USER::instantiates)
					    ($Objects object 'USER::exemplifies)
					    ($Objects object 'USER::is-a)))
		(some #'(lambda (super) (setq modifier-long-name
					      ($modified-by super modifier-short-name cachable?)))
		      superordinates)
		)

	     modifier-long-name
	     )
	  )
    )
  )

;;;-------  $OBJECTS-OF-TYPE  -------

;;;Edited by Bob Schulman   5 May 87  21:31
;;;Edited by Mike Hewett   8 May 87  11:26
;;;Edited by Vaughan Johnson       23 Jul 87  17:02
(defun $objects-of-type
       (type sort) 
  "Finds all objects that match with the specified type and the specified sort.
An object x matches with type iff ($isa x type))

SORT can be INSTANCE, individual, TYPE, or ALL.

The allowable sorts are 'INSTANCE, 'individual, 'TYPE and 'ALL.
INSTANCE  collects instances, by following can-be-a, exemplified-by, and instantiated-by links.
INDIVIDUAL collects individuals, by following can-be-a and exemplified-by links.
TYPE     collects objects by following can-be-a links only.
ALL      is the union of these three sorts, following can-be-a, exemplfied-by, instantiated-by, and played-by links.
"

  (case sort
	(USER::INSTANCE ($instances-of type))
	(USER::INDIVIDUAL ($examples-of type))
	(USER::TYPE ($types-of type))
	(USER::ALL (append ($instances-of type)
			   ($examples-of type)
			   ($types-of type)))
	(t ($objects-of-type type 'USER::ALL)))
  )

;;;-------  $OUTPUT-FROM  -------

;;;Edited by Vaughan Johnson       23 Jul 87  17:02
(defun $output-from (output-object target-short-name &OPTIONAL (not-cachable? nil))
  "Return the long name of target-short-name if output-object or an object it $ISA 
   is output-from target-short-name."
  
  (cond
    ((not output-object)    nil)	   ; to stop the recursion
    ((find-if #'(lambda (pblm) (eql ($short-name pblm) target-short-name))
	      ($objects output-object 'user::output-from)))
    (($output-from (or ($object output-object 'user::is-a)
		       ($object output-object 'user::exemplifies)
		       ($object output-object 'user::instantiates))
		   target-short-name
		   not-cachable?))))


;;;-------  $PLAYS  -------

(defun $PLAYS (object type &OPTIONAL (cachable? NIL))
  "Determines whether 'object' has a 'PLAYS' link to another
object whose (short) name is 'type'.  Default is to not cache."

  ($link object type '(USER::plays USER::can-be-a USER::instantiated-by USER::exemplified-by)
	 '$PLAYS- (not cachable?))
  )


;;;-------  $PROMOTES  -------

(defun $PROMOTES (object concept-type &OPTIONAL (not-cachable? nil))
  "Determines whether OBJECT is linked to CONCEPT-TYPE via an IS-A link."

  ($link object concept-type '(USER::promotes USER::causes USER::is-a USER::instantiates USER::exemplifies)
	 '$PROMOTES- not-cachable?)
  )


;;;-------  $TYPE-OF  -------

(defun $TYPE-OF (object type links)
  "This uses $IS-LINKED-TO to determine if 'object' is of 'type'.
Only the specified 'links' are followed."

  (if ($Is-Linked-To object type nil links)
      T
    NIL)
  )

;;;-------  $TYPES-OF  -------

(defun $TYPES-OF (type)
  "Follow CAN-BE-A links to find objects matching TYPE via is-a links"

  (let ((sub-types ($objects type 'USER::can-be-a)))
    (append sub-types
	    (mapcan #'$types-of sub-types)))
  )

