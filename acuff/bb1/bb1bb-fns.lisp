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
;;;  Micheal Hewett, Alan Garvey, M. Vaughan Johnson, Jr., Robert Schulman, and Reed Hastings                      +
;;;  have worked on this implementation.                                                                              +
;;;                                                                                                                    +
;;;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;; 
;;;                                  FILE INFO
;;; 
;;; 
;;;
;;;  BB1BB-FNS.LISP      -     Functions for the BB1 Blackboard Manager
;;;
;;;  Mike Hewett      02/05/86
;;;                     03/25/86  Added DELETE-PROCEDURAL-ATTACHMENT-OF-ATTRIBUTE     -mh
;;;                                 Modified MODIFY-VALUE-OF-ATTRIBUTE                        -mh
;;;                     03/26/86  Added functions to handle KNOWLEDGEBASE-INFO              -mh
;;;                     04/04/86  Modified DELETE routines to use DELETE-BB-x-DATA           -mh
;;;                     04/07/86  Took CONTEXT argument from BBADD and BBMODIFY           -mh
;;;                     04/19/86  Added SAVE-KSES-TO-FILE and SAVE-KS-TO-STREAM          -mh
;;;                     07/09/86  Modified SEARCHFOR.  Was returning converse of true result   -mh
;;;                     07/09/86  Fixed FIND-SEARCH-SPACE.                                     -mh
;;;                     07/22/86  Changed MAKE-EXTENDED-NAME to call new utility function MAKE-PACKED    -mh
;;;                     07/24/86  Changed SAVE-SYSTEM-TO-FILE to use code like
;;;                                 COMPILER:FASD-SYMBOL-VALUE to save hash tables.  Not Common Lisp compatible!    -mh
;;;                     09/22/86  Made changes as necessary to support new hash table format  -mh
;;;                     09/23/86  Added COPY-OBJECT, COPY-LEVEL from InterLisp version        -mh
;;;                     09/24/86  Fixed bug in SAVE-KSES-TO-FILE                                -mh
;;;                     09/26/86  Added COPY-BB-DATA-INTO-TEMP                              -mh
;;;                     09/26/86  Modified SET-UP-BB-DATA-ARRAYS to handle temp arrays      -mh
;;;                     09/29/86  Added ALL-LINKS-OF-SYSTEM and ALL-ATTRIBUTES-OF-SYSTEM, formerly in BB1BB-MACS.
;;;                     10/28/86  Added SHORT-NAMES (SHORT-NAME added earlier)             -mh
;;;                     10/29/86  Added ALL-KSES-OF-SYSTEM                                   -mh
;;;                     11/06/86  Added optimized STORE-ATTRIBUTE from BB1BB-MACS          -mh
;;;                     11/29/86  Added BB1-FIX-UP-LINKS-FOR-RUN                             -mh
;;;                     11/29/86  Added BB1-FIX-UP-LINKS-FOR-SAVE                            -mh
;;;                     12/04/86  Added LONG-NAME                                             -mh
;;;                     03/18/87  Added SAVE-BB1-STATE and LOAD-SAVED-STATE               -mh
;;;                                 Added GET-SYSTEM-NAME-FROM-USER                         -mh
;;;                     03/23/87  Modified  ADD-TO-x-ARRAY fns to reduce consing              -mh
;;;                     03/31/87  Added ADD-LINK-FROM-ALL-OBJECTS-AT-LEVEL,
;;;                                        DELETE-ALL-INHERITANCE-OF-ATTRIBUTE (AND -LEVEL),
;;;                                        DELETE-ALL-OBJECTS-OF-BLACKBOARD (AND -LEVEL),
;;;                                        RENAME-ATTRIBUTE                                      -mh
;;;                     04/07/87  Added Restore-Initial-State and Save-Initial-State               -mh
;;;                     04/19/87  Added CHECK-BB-INTEGRITY and other INTEGRITY fns           -mh
;;;                     04/23/87  Modified MAKE-TEMP-SYSTEM - faster and saves links to other bbs    -mh
;;;                     

;;;  This file contains functions which comprise the BB1 Blackboard Manager.
;;;  Note that many of the original blackboard functions are now macros
;;;  which are stored on "BB1BB-MACS.LISP".
;;;
;;;  All of these functions are used internally by BB1 (a lot!).  User
;;;  blackboard manipulations should be done through higher-level
;;;  functions.
;;;
;;;  IN PARTICULAR:  Beware that many of the low-level retrieval functions
;;;                  return the actual data element; i.e. it is possible
;;;                  to accidentally change the actual data on the blackboard
;;;                  when manipulating values retrieved by functions in this file.
;;;
;;;                  Therefore, user-level functions should call (COPY-TREE ...)  or (COPY-ANYTHING ...) on
;;;                  all data returned by these functions.


;;;  Contents:

;;     GENERAL STORAGE:
;;                        add-to-attribute-array
;;                        add-to-blackboard-array
;;                        add-to-data-array
;;                        add-to-knowledgebase-array
;;                        add-to-level-array
;;                        add-to-link-array
;;                        add-to-object-array
;;                        add-to-shortlink-array
;;                        add-to-shortattribute-array
;;                        add-to-system-array
;;
;;
;;  HIGH-LEVEL STORAGE:
;;                        add-attribute-to-object
;;                        add-blackboard-to-knowledgebase
;;                        add-blackboard-to-system
;;                        add-data-to-system
;;                        add-knowledgebase-to-system
;;                        add-level-to-blackboard
;;                        add-link-from-all-objects-at-level
;;                        add-link-from-object-to-object
;;                        add-link-pair-to-system
;;                        add-object-to-level
;;                        add-possible-attribute-to-level
;;                        set-up-bb-data-arrays
;;                        set-up-blackboard
;;                        parse-inheritance-info
;;
;;
;;     DELETE ELEMENTS:
;;                        delete-all-inheritance-of-attribute
;;                        delete-all-inheritance-of-link
;;                        delete-all-objects-of-blackboard
;;                        delete-all-objects-of-level
;;                        delete-attribute-of-object
;;                        delete-blackboard-of-system
;;                        delete-data
;;                        delete-knowledgebase-of-system
;;                        delete-level-of-blackboard
;;                        delete-link-between-objects
;;                        delete-link-of-object
;;                        delete-link-of-system
;;                        delete-object
;;                        delete-possible-attribute-of-system
;;                        delete-procedural-attachment-of-attribute
;;
;;        COPY ELEMENTS:
;;                        copy-object
;;                        copy-level
;;
;;     RENAME ELEMENTS:
;;                        rename-system
;;                        rename-knowledgebase
;;                        rename-blackboard-of-system
;;                        rename-level-of-blackboard
;;                        rename-object
;;                        rename-attribute
;;
;;            RUN-TIME:
;;                        all-attributes-of-system
;;                        all-kses-of-system
;;                        all-links-of-system
;;                        bb1-fix-up-links-for-run
;;                        bb1-fix-up-links-for-save
;;                        bb1-knowledge-base-check-and-integrate
;;                        bbadd
;;                        bbmodify
;;                        check-bb-integrity
;;                        integrity-error
;;                        check-blackboard-integrity
;;                        check-level-integrity
;;                        check-object-integrity
;;                        check-kb-compatibility
;;                        clear-hash-arrays
;;                        current-knowledgebase-name
;;                        current-system-name
;;                        generate-object-name
;;                        load-saved-state
;;                        make-extended-name
;;                        make-temp-system
;;                        modify-value-of-attribute
;;                        restore-initial-state
;;                        save-initial-state
;;                        save-bb1-state
;;                        save-system-to-file
;;                        save-kses-to-file
;;                        save-ks-to-stream
;;                        searchfor
;;                        long-name
;;                        short-name
;;                        short-names
;;                        store-attribute
;;
;;;------------------------------------------------------------------


;;;----- Package information ------

(in-package 'BB1)

;;;----------------------------




;;;  Source:


;;-------  ADD-TO-ATTRIBUTE-ARRAY  -------


(defun add-to-attribute-array
       (attribute new-short-name new-object new-value new-procedural-attachment addmethod)

  "Adds new information into the ATTRIBUTE hash array."

;; arguments of NIL are ignored except when ADDMETHOD = 'override.

  (let (old-data)

	(if (setq old-data (get-bb-attribute-data attribute))
	    (cond ((eql addmethod 'update) 
					   (if *bb1-save-memory-flag*
					       (setf (bb-attributeinforec-values old-data)
						     (list new-value))
					       ;;ELSE
					       (push new-value (bb-attributeinforec-values old-data)))
					   (when new-procedural-attachment
					     (setf (bb-attributeinforec-procedural-attachment old-data)
						   new-procedural-attachment)
					     (setf (bb-attributeinforec-values old-data) nil))
					   attribute)

		  ((eql addmethod 'override) (setf (bb-attributeinforec-short-name old-data)
						   new-short-name)
					     (setf (bb-attributeinforec-object old-data)
						   new-object)
					     (setf (bb-attributeinforec-procedural-attachment old-data)
						   new-procedural-attachment)
					     (setf (bb-attributeinforec-values old-data)
						   new-value)
					     attribute)

		  (t          (bb1error 'ADD-TO-ATTRIBUTE-ARRAY
					"Your ADDMETHOD, " addmethod " must be OVERRIDE or UPDATE."))
		)
      ;;else
	     (progn (put-bb-attribute-data attribute
					   (make-bb-attributeinforec :short-name new-short-name
								     :object     new-object
								     :values     new-value
								     :procedural-attachment
								                 new-procedural-attachment))
		    attribute)
	     )
	)
  )
		


(defun add-to-blackboard-array

       (blackboard new-system new-type new-level new-knowledgebase addmethod)

  "Adds new information into the BLACKBOARD hash array."

;; arguments of NIL are ignored except when ADDMETHOD = 'override.
;; OLD-LEVEL should always be NIL or a list.
;; NEW-LEVEL will always be NIL or an atom.


  (let (old-data)

	(if (setq old-data (get-bb-blackboard-data blackboard))
	    (cond ((eql addmethod 'update)
					   (when new-system
					     (setf (bb-blackboardinforec-system old-data)
						   new-system))

					   (when new-level
					     (cond ((null (bb-blackboardinforec-levels old-data))
						                      (setf (bb-blackboardinforec-levels
									      old-data)
									    (list new-level)))
						   (t                 (nconc (bb-blackboardinforec-levels
										      old-data)
									      (list new-level)))
						   )
					     )

					   (when new-knowledgebase
					     (setf (bb-blackboardinforec-knowledgebase old-data)
						   new-knowledgebase))

;;;---> not necessary	   (put-bb-blackboard-data blackboard old-data)
					   blackboard)

		  ((eql addmethod 'override) (setf (bb-blackboardinforec-system old-data)
						   new-system)
					     (setf (bb-blackboardinforec-type old-data)
						   new-type)
					     (setf (bb-blackboardinforec-levels old-data)
						   new-level)
					     (setf (bb-blackboardinforec-knowledgebase old-data)
						   new-knowledgebase)

;-----> not necessary       (put-bb-blackboard-data blackboard old-data)
					     blackboard)

		  (t          (bb1error 'ADD-TO-BLACKBOARD-ARRAY
					"Your ADDMETHOD, " addmethod " must be OVERRIDE or UPDATE."))
		)
      ;;else
	     (progn (put-bb-blackboard-data blackboard
					   (make-bb-blackboardinforec :system   new-system
								      :type     new-type
								      :levels   new-level
								      :knowledgebase new-knowledgebase))
		    blackboard)
	     )
	)
  )

;;-------  ADD-TO-DATA-ARRAY  -------

(defun add-to-data-array (data new-value)

  (let (old-data)

	(cond ((setq old-data (get-bb-data-data data))  (setf (bb-datainforec-value old-data) new-value)
;;----> not necessary			(put-bb-data-data data old-data)
							data)
	      (t      (put-bb-data-data data
					(make-bb-datainforec :value new-value))
		      data)
	     )
	)
  )


;;-------  ADD-TO-KNOWLEDGEBASE-ARRAY  -------

(defun add-to-knowledgebase-array (knowledgebase new-system new-blackboard addmethod)

  (let (old-data)

	(if (setq old-data (get-bb-knowledgebase-data knowledgebase))
	    (cond ((eql addmethod 'update) (when new-system
					     (setf (bb-knowledgebaseinforec-system old-data)
						   new-system))

					   (when new-blackboard
					     (cond ((null (bb-knowledgebaseinforec-blackboards old-data))
						               (setf (bb-knowledgebaseinforec-blackboards
								       old-data)
								     (list new-blackboard)))
						   (t          (nconc (bb-knowledgebaseinforec-blackboards
									   old-data)
								       (list new-blackboard)))
						   )
					     )

					   knowledgebase)

		  ((eql addmethod 'override) (setf (bb-knowledgebaseinforec-system old-data)
						   new-system)
					     (setf (bb-knowledgebaseinforec-blackboards old-data)
						   new-blackboard)
					
					     knowledgebase)

		  (t          (bb1error 'ADD-TO-KNOWLEDGEBASE-ARRAY
					"Your ADDMETHOD, " addmethod " must be OVERRIDE or UPDATE."))
		)
      ;;else
	     (progn (put-bb-knowledgebase-data knowledgebase
					   (make-bb-knowledgebaseinforec
					                         :system        new-system
								 :blackboards   new-blackboard
								 ))
		    knowledgebase)
	     )
	)
  )



;;-------  ADD-TO-LEVEL-ARRAY  -------


(defun add-to-level-array (level new-short-name new-blackboard new-object addmethod)

  ;Modified 09/22/86 - mh

  (let (old-data)

	(if (setq old-data (get-bb-level-data level))
	    (cond ((eql addmethod 'update) (when new-blackboard
					     (setf (bb-levelinforec-blackboard old-data) new-blackboard)
					     )

					   (when new-object
					     (cond ((null (bb-levelinforec-objects old-data))
						               (setf (bb-levelinforec-objects old-data)
								     (list new-object))
							       )
						   (  t   (nconc (bb-levelinforec-objects old-data)
								 (list new-object))))
					     )
					   level)

		  ((eql addmethod 'override) (setf (bb-levelinforec-short-name old-data)
						   new-short-name)
					     (setf (bb-levelinforec-blackboard old-data)
						   new-blackboard)
					     (setf (bb-levelinforec-objects old-data)
						   new-object)
					
					     level)

		  (t          (bb1error 'ADD-TO-LEVEL-ARRAY
					"Your ADDMETHOD, " addmethod " must be OVERRIDE or UPDATE."))
		)
      ;;else
	     (progn (put-bb-level-data level
					   (make-bb-levelinforec :short-name          new-short-name
								 :blackboard          new-blackboard
								 :objects             new-object))

		    level)
	     )
	)
  )


;;-------  ADD-TO-LINK-ARRAY  -------

(defun add-to-link-array (link new-short-name new-linked-object addmethod)

  (let (old-data)

	(if (setq old-data (get-bb-link-data link))
	    (cond ((eql addmethod 'update) (when new-linked-object
					     (cond ((null (bb-linkinforec-linked-objects old-data))
						            (setf (bb-linkinforec-linked-objects old-data)
								  (list new-linked-object))
							    )
						   (  t     (nconc (bb-linkinforec-linked-objects old-data)
								   (list new-linked-object))))
					     )

					   link)

		  ((eql addmethod 'override) (setf (bb-linkinforec-short-name old-data)
						   new-short-name)
					     (setf (bb-linkinforec-linked-objects old-data)
						   new-linked-object)

					     link)

		  (t          (bb1error 'ADD-TO-LINK-ARRAY
					"Your ADDMETHOD, " addmethod " must be OVERRIDE or UPDATE."))
		)
      ;;else
	     (progn (put-bb-link-data link
				      (make-bb-linkinforec :short-name          new-short-name
							   :linked-objects      new-linked-object))

		    link)
	     )
	)
  )



;;-------  ADD-TO-OBJECT-ARRAY  -------

(defun add-to-object-array (object new-short-name new-level new-blackboard
			    new-attribute new-link new-$link addmethod)

  ;Modified 09/22/86 - mh

  (let (old-data)

	(if (setq old-data (get-bb-object-data object))
	    (cond ((eql addmethod 'update) (when new-attribute
					     (cond ((null (bb-objectinforec-attributes old-data))
					                     (setf (bb-objectinforec-attributes old-data)
								   (list new-attribute)))
						   (  t     (nconc (bb-objectinforec-attributes old-data)
								   (list new-attribute)))))

					   (when new-link
					     (cond ((null (bb-objectinforec-links old-data))
					                     (setf (bb-objectinforec-links old-data)
								   (list new-link)))
						   (  t     (nconc (bb-objectinforec-links old-data)
								   (list new-link)))))

					   (when new-$link
					     (cond ((null (bb-objectinforec-$links old-data))
					                     (setf (bb-objectinforec-$links old-data)
								   (list new-$link)))
						   (  t     (nconc (bb-objectinforec-$links old-data)
								   (list new-$link)))))
					   object)
		  
		  ((eql addmethod 'override) (setf (bb-objectinforec-short-name old-data)
						   new-short-name)
					     (setf (bb-objectinforec-level old-data)
						   new-level)
					     (setf (bb-objectinforec-blackboard old-data)
						   new-blackboard)
					     (setf (bb-objectinforec-attributes old-data)
						   new-attribute)
					     (setf (bb-objectinforec-links old-data)
						   new-link)
					     (setf (bb-objectinforec-$links old-data)
						   new-$link)

					     object)

		  (t          (bb1error 'ADD-TO-OBJECT-ARRAY
					"Your ADDMETHOD, " addmethod " must be OVERRIDE or UPDATE."))
		)
      ;;else
	     (progn (put-bb-object-data object
					(make-bb-objectinforec :short-name    new-short-name
							       :level         new-level
							       :blackboard    new-blackboard
							       :attributes    new-attribute
							       :links         new-link
							       :$links        new-$link))

		    object)
	     )
	)
  )



;;;-------  ADD-TO-SHORTATTRIBUTE-ARRAY  -------

(defun add-to-shortattribute-array (attribute new-inheritance addmethod)
  "Adds elements to the *BB-SHORTATTRIBUTEINFO* array.  ADDMETHOD can be
UPDATE or OVERRIDE."

  ;Added 09/22/86

  (let ((old-data (get-bb-shortattribute-data attribute))
	old-inheritance
	)
    
    (if old-data
	(case addmethod
	      (OVERRIDE   (setf (bb-shortattributeinforec-inheritance-opportunities old-data)
				new-inheritance)
			  attribute)

	      (UPDATE     (when new-inheritance
			    (cond ((null (setq old-inheritance
					       (bb-shortattributeinforec-inheritance-opportunities old-data)))
				                    (setf (bb-shortattributeinforec-inheritance-opportunities
							    old-data)
							  (list new-inheritance))
						    )
				  (t           (setf (bb-shortattributeinforec-inheritance-opportunities
						       old-data)
						     (cons new-inheritance
							   (remove-if #'(lambda (inherit-info)
									  (eql (car inherit-info)
									       (car new-inheritance)))
								      old-inheritance)
							   )
						     )
					       )
				  )
			      )
			  attribute)

	      (otherwise  (BB1ERROR 'ADD-TO-SHORTATTRIBUTE-ARRAY
				    "Illegal ADDMETHOD - should be OVERRIDE or UPDATE."))
	      )
	;;Else - add a new record to the array
	(put-bb-shortattribute-data attribute (make-bb-shortattributeinforec :inheritance-opportunities
									      (and new-inheritance
										   (list new-inheritance))))
	)
    )
  )



;;;-------  ADD-TO-SHORTLINK-ARRAY  -------

(defun add-to-shortlink-array (link new-opplink new-inheritance addmethod)
  "Adds elements to the *BB-SHORTATTRIBUTEINFO* array.  ADDMETHOD can be
UPDATE or OVERRIDE."

  ;Added 09/22/86

  (let ((old-data (get-bb-shortlink-data link))
	old-inheritance)

    (if old-data
	(case addmethod
	      (OVERRIDE   (setf (bb-shortlinkinforec-opplink old-data)
				new-opplink)
			  (setf (bb-shortlinkinforec-inheritance-opportunities old-data)
				new-inheritance)
			  link)

	      (UPDATE     (when new-inheritance
			    (cond ((null (setq old-inheritance
					       (bb-shortlinkinforec-inheritance-opportunities old-data)))
				                    (setf (bb-shortlinkinforec-inheritance-opportunities
							    old-data)
							  (list new-inheritance))
						    )
				  (t           (setf (bb-shortlinkinforec-inheritance-opportunities
						       old-data)
						     (cons new-inheritance
							   (remove-if #'(lambda (inherit-info)
									  (eql (car inherit-info)
									       (car new-inheritance)))
								      old-inheritance)
							   )
						     )
					       )
				  )
			    )

			  (when new-opplink
			    (setf (bb-shortlinkinforec-opplink old-data) new-opplink)
			    )
			  link)
	      (otherwise  (BB1ERROR 'ADD-TO-SHORTLINK-ARRAY
				    "Illegal ADDMETHOD - should be OVERRIDE or UPDATE."))
	      )
	;;Else - add a new record to the array
	(put-bb-shortlink-data link (make-bb-shortlinkinforec :opplink new-opplink
							      :inheritance-opportunities
							      (and new-inheritance
								   (list new-inheritance))))
	)
    )
  )



;;;-------  ADD-TO-SYSTEM-ARRAY  -------


(defun add-to-system-array (system new-domain-bboard new-control-bboard 
			    new-knowledgebase new-data-element addmethod)

  ;; HAIRY!

;; With an ADDMETHOD = UPDATE, expect the "new-x" parameters to *always* be atoms.
;; If ADDMETHOD = OVERRIDE, the caller should build the correct lists.


  (let ((old-data (get-bb-system-data system)))

    (if old-data
	(case addmethod

	    (UPDATE  (when new-domain-bboard
		       (cond ((null (bb-systeminforec-domain-blackboards old-data))
			                 (setf (bb-systeminforec-domain-blackboards old-data)
					       (list new-domain-bboard)))
			     (t          (nconc (bb-systeminforec-domain-blackboards old-data)
						(list new-domain-bboard))))
		       )

		     (when new-control-bboard	
		       (cond ((null (bb-systeminforec-control-blackboards old-data))
			                 (setf (bb-systeminforec-control-blackboards old-data)
					       (list new-control-bboard)))
			     (t          (nconc (bb-systeminforec-control-blackboards old-data)
						(list new-control-bboard))))
		       )
		   
		   (when new-knowledgebase
		       (cond ((null (bb-systeminforec-knowledgebases old-data))
			                 (setf (bb-systeminforec-knowledgebases old-data)
					       (list new-knowledgebase)))
			     (t          (nconc (bb-systeminforec-knowledgebases old-data)
						(list new-knowledgebase))))
		       )
		
		   (when new-data-element
		       (cond ((null (bb-systeminforec-data-elements old-data))
			                 (setf (bb-systeminforec-data-elements old-data)
					       (list new-data-element)))
			     (t          (pushnew new-data-element
						  (bb-systeminforec-data-elements old-data)))
			     )
		       )

		   system)

	    (OVERRIDE	(setf (bb-systeminforec-domain-blackboards old-data)  new-domain-bboard)
			(setf (bb-systeminforec-control-blackboards old-data) new-control-bboard)
			(setf (bb-systeminforec-knowledgebases old-data)      new-knowledgebase)
			(setf (bb-systeminforec-data-elements old-data)       new-data-element)
			system)

	    (otherwise  (bb1error 'ADD-TO-SYSTEM-ARRAY
				  "Your ADDMETHOD, " addmethod " must be OVERRIDE or UPDATE."))
	    )

      ;;else - new data
	(progn (put-bb-system-data system
				   (make-bb-systeminforec :domain-blackboards new-domain-bboard
							  :control-blackboards new-control-bboard
							  :knowledgebases  new-knowledgebase
							  :data-elements   new-data-element))
	       system)
      )
    )
  )



;;;-----------------------------------------------------------------------------------

;;  HIGH-LEVEL STORAGE ROUTINES

;;------- ADD-ATTRIBUTE-TO-OBJECT  -------

;;;Edited by HASTINGS                   15 May 87  15:06
;;;Edited by Mike Hewett                20 May 87  15:17
(defun add-attribute-to-object (attribute value object)
  
  ;; Modifed 09/22/86  -mh
  
  (cond ((not (valid-object? object))
	 (let ((error (make-condition 'object-not-defined
				      "the object, ~a, isn't in the ~a system."
				      object  *bb1-system-name*)))
	   (multiple-value-bind (proceed-type proceed-value)
	       (signal error :proceed-types (list :new-value :retry-operation :use-other-object))
	     (ccase proceed-type
	       (:new-value proceed-value)
	       (:retry-operation (add-attribute-to-object attribute value object))
	       (:use-other-object (add-attribute-to-object attribute value proceed-value))))))
	((not (valid-attribute-for-system? attribute *BB1-SYSTEM-NAME*))
	 (let ((error (make-condition 'attribute-not-in-system
				      "the attribute, ~a, isn't in the ~a system."
				      attribute *bb1-system-name*)))
	   (multiple-value-bind (proceed-type proceed-value)
	       (signal error :proceed-types (list :new-value :retry-operation :use-other-attribute))
	     (ccase proceed-type
	       (:new-value proceed-value)
	       (:retry-operation (add-attribute-to-object attribute value object))
	       (:use-other-attribute (add-attribute-to-object proceed-value value object))))))
	((valid-attribute-for-object? attribute object)
	 (bb1error 'ADD-ATTRIBUTE-TO-OBJECT
		   attribute " is already an attribute of " object "."))
	(t
	 ;;ELSE
	 (let ((long-attr (make-extended-name object attribute)))
	   
	   (add-to-object-array object nil nil nil (cons attribute long-attr) nil nil 'update)
	   (if (listp value)
	       (if (member (car value) '(USER::$procedure  bb1::$PROCEDURE))
		   (add-to-attribute-array long-attr attribute object nil (cadr value) 'override)
		   ;;ELSE
		   (add-to-attribute-array long-attr attribute object (list value) nil 'override))
	       ;;ELSE
	       (add-to-attribute-array long-attr attribute object (list value) nil 'override))
	   )
	 )))


;;-------  ADD-BLACKBOARD-TO-KNOWLEDGEBASE  -------

(defun add-blackboard-to-knowledgebase (blackboard type knowledgebase)

  (if (valid-knowledgebase? knowledgebase)
      (if (valid-blackboard? blackboard)
	  (bb1error 'ADD-BLACKBOARD-TO-KNOWLEDGEBASE
		    blackboard " is already a blackboard in the "
		    (system-of-blackboard blackboard) " system.")
	
	;;ELSE
	  (case type
	    ((domain control)  (add-to-knowledgebase-array knowledgebase nil blackboard 'update)
			       (add-to-blackboard-array blackboard (system-of-knowledgebase knowledgebase)
							type nil knowledgebase 'override))
	    (otherwise
	             (bb1error 'ADD-BLACKBOARD-TO-KNOWLEDGEBASE
			       type " is an undefined blackboard type.")))
	)
	
    ;;ELSE
      (bb1error 'ADD-BLACKBOARD-TO-KNOWLEDGEBASE
		knowledgebase " is not an existing knowledge base.")
    )
  )




;;-------  ADD-BLACKBOARD-TO-SYSTEM  -------

(defun add-blackboard-to-system (blackboard type system)

  ;;Modified 09/22/86  -mh

  (if (valid-system? system)
      (if (valid-blackboard? blackboard)
	  (bb1error 'ADD-BLACKBOARD-TO-SYSTEM
		    blackboard " is already a blackboard in the " system " system.")
	
	;;ELSE
	  (case type
	    (domain  (add-to-system-array system blackboard nil nil nil 'update)
		     (add-to-blackboard-array blackboard system type nil nil 'override))
	    (control (add-to-system-array system nil blackboard nil nil 'update)
		     (add-to-blackboard-array blackboard system type nil nil 'override))
	    (otherwise
	             (bb1error 'ADD-BLACKBOARD-TO-SYSTEM
			       type " is an undefined blackboard type.")))
	)
	
    ;;ELSE
      (bb1error 'ADD-BLACKBOARD-TO-SYSTEM
		system " is not an existing system.")
    )
  )


;;-------  ADD-DATA-TO-SYSTEM  -------

(defun add-data-to-system (data-name data-value system)

  (if (valid-system? system)
      (if (valid-data? data-name)
	  (add-to-data-array data-name data-value)
	;;ELSE
	  (progn (add-to-system-array system nil nil nil data-name 'update)
		 (add-to-data-array data-name data-value))
	  )
    ;;ELSE
      (bb1error 'ADD-DATA-TO-SYSTEM
		system " is not an existing system.")
      )
  )


;;-------  ADD-KNOWLEDGEBASE-TO-SYSTEM  -------

(defun add-knowledgebase-to-system (kb-name system)

  (if (valid-system? system)
      (if (valid-knowledgebase? kb-name)
	  (bb1error 'ADD-KNOWLEDGEBASE-TO-SYSTEM
		    kb-name " is already a knowledge base in the " system " system.")
          ;;ELSE
	  (progn (add-to-system-array system nil nil kb-name nil 'update)
		 (add-to-knowledgebase-array kb-name system nil 'override))
	  )
    ;;ELSE
      (bb1error 'ADD-KNOWLEDGEBASE-TO-SYSTEM
		system " is not an existing system.")
      )
  )


;;-------  ADD-LEVEL-TO-BLACKBOARD  -------

(defun add-level-to-blackboard (level blackboard)

  (if (valid-blackboard? blackboard)
      (let ((long-level (make-extended-name blackboard level)))
	(if (valid-level? long-level)
	    (bb1error 'ADD-LEVEL-TO-BLACKBOARD
		      level " already exists on the " blackboard " blackboard.")
	  ;;ELSE
	    (progn (add-to-blackboard-array blackboard nil nil long-level nil 'update)
		   (add-to-level-array long-level level blackboard nil 'override))
	    )
	long-level)
		
    ;;ELSE
      (bb1error 'ADD-LEVEL-TO-BLACKBOARD
		blackboard " is not an existing blackboard.")
    )
  )


;;-------  ADD-LINK-FROM-ALL-OBJECTS-AT-LEVEL  -------

(defun add-link-from-all-objects-at-level (link level to-object)
  "Adds the same link from all of the objects at LEVEL to TO-OBJECT."

  (if (valid-link-for-system? link *bb1-system-name*)
      (if (valid-level? level)
	  (if (valid-object? to-object)
	      (dolist (from-object (objects-of-level level))
		(add-link-from-object-to-object link from-object to-object)
		(add-link-from-object-to-object (opposite-link-of-link link)
						to-object from-object)
		)
	      ;;ELSE - invalid to-object
	      (bb1error 'ADD-LINK-FROM-ALL-OBJECTS-AT-LEVEL
			to-object " is not an existing object.")
	      )
	  ;;ELSE - invalid level
	  (bb1error 'ADD-LINK-FROM-ALL-OBJECTS-AT-LEVEL
		    level " is not an existing level.")
	  )
      ;;ELSE - invalid link
      (bb1error 'ADD-LINK-FROM-ALL-OBJECTS-AT-LEVEL
		link " is not an existing link.")
      )
  )



;;-------  ADD-LINK-FROM-OBJECT-TO-OBJECT  -------

;;;Edited by Mike Hewett   27 Apr 87  14:05
(defun add-link-from-object-to-object (link from-object to-object &KEY (verbose nil))

;;Optimized  11/06/86  -mh  

  (let ((from-object-info (get-bb-object-data from-object))
	(to-object-info   (get-bb-object-data to-object))
	(link-info        (get-bb-shortlink-data link))
	)

    (if from-object-info
	(if to-object-info
	    (if link-info
		(let* ((long-link      (make-extended-name from-object link))
		       (long-link-info (get-bb-link-data long-link))
		       )
		  (if long-link-info     ; - adding another object to an existing link
		      (unless (member to-object (bb-linkinforec-linked-objects long-link-info))
			(add-to-link-array long-link nil to-object 'update)
			(when verbose
			  (bb1message t "Adding link " link " from " from-object " to " to-object)
			  )
			)
		        ;;ELSE - Adding a new link to this object.
			  (add-to-object-array from-object nil nil nil nil (cons link long-link) nil 'update)
			  (add-to-link-array   long-link link (list to-object) 'override)
			  (when verbose
			    (bb1message t "Adding link " link " from " from-object " to " to-object ".")
			    )
		      )
		  long-link)
		
		;;ELSE  -------------------
		(bb1error 'ADD-LINK-FROM-OBJECT-TO-OBJECT
			  link " is not a valid link for the system of " from-object))
	      ;;ELSE
	    (bb1error 'ADD-LINK-FROM-OBJECT-TO-OBJECT
		      to-object " is not an existing object."))
	;;ELSE
	(bb1error 'ADD-LINK-FROM-OBJECT-TO-OBJECT
		  from-object " is not an existing object."))
    )
  )


;(defun add-link-from-object-to-object (link from-object to-object)

;; old unoptimized version

;  (if (valid-object? from-object)
;      (if (valid-object? to-object)
;	  (if (valid-link-for-system? link (system-of-blackboard (blackboard-of-object from-object)))
;	      (let ((long-link (make-extended-name from-object link)))
;		(if (valid-link? long-link)
;		    (if (not (member to-object (linked-objects-of-link long-link)))
;			(add-to-link-array long-link nil to-object 'update))
;		  ;;ELSE
;		    (progn (add-to-object-array from-object nil nil nil nil (cons link long-link) nil 'update)
;			   (add-to-link-array   long-link link (list to-object) 'override))
;		    )
;		long-link)

;	    ;;ELSE  -------------------
;	      (bb1error 'ADD-LINK-FROM-OBJECT-TO-OBJECT
;			link " is not a valid link for the system of " from-object))
;	;;ELSE
;	  (bb1error 'ADD-LINK-FROM-OBJECT-TO-OBJECT
;		    to-object " is not an existing object."))
;    ;;ELSE
;      (bb1error 'ADD-LINK-FROM-OBJECT-TO-OBJECT
;		from-object " is not an existing object."))
;  )


;;-------  ADD-LINK-PAIR-TO-SYSTEM  -------

(defun add-link-pair-to-system (link opposite-link system)

  (if (valid-system? system)
      (if (not (valid-link-for-system? link system))
	  (if (not (valid-link-for-system? opposite-link system))
	      (progn (add-to-shortlink-array link opposite-link nil 'override)
		     (add-to-shortlink-array opposite-link link nil 'override)
		     )
	    ;;ELSE -----------------------
	    (bb1error 'ADD-LINK-PAIR-TO-SYSTEM
		      opposite-link " is already defined in the " system " system."))
	;;ELSE
	  (bb1error 'ADD-LINK-PAIR-TO-SYSTEM
		    link " is already defined in the " system " system."))
    ;;ELSE
      (bb1error 'ADD-LINK-PAIR-TO-SYSTEM
		system " is not an existing system.")
      )
  )


;;-------  ADD-OBJECT-TO-LEVEL  -------

(defun add-object-to-level (object level)

  (if (valid-level? level)
      (let ((long-object (make-extended-name level object)))
	(if (valid-object? long-object)
	    (bb1error 'ADD-OBJECT-TO-LEVEL
		      object " already exists on the " level " level.")
	  ;;ELSE
	    (progn (add-to-level-array level nil nil long-object 'update)
		   (add-to-object-array long-object object level (blackboard-of-level level) nil
					nil nil 'override)
		   long-object)
	    ))
		
    ;;ELSE
      (bb1error 'ADD-OBJECT-TO-LEVEL
		level " is not an existing level.")
    )
  )

;;;Edited by Reed Hastings   2 May 87  23:19
(defun add-object (long-name)
  "adds the object to the current system"
  (add-object-to-level (third (make-unpacked #\. long-name))
		       (make-extended-name (first (make-unpacked #\. long-name))
					   (second (make-unpacked #\. long-name)))))
;;-------  ADD-ATTRIBUTE-TO-SYSTEM  -------

(defun add-attribute-to-system (attribute system)

  (if (valid-system? system)
      (add-to-shortattribute-array attribute nil 'update)
      ;;ELSE
      (bb1error 'ADD-ATTRIBUTE-TO-SYSTEM
		system " is not a defined system."))
  )



;;-------  SET-UP-BB-DATA-ARRAYS  -------

(defun set-up-bb-data-arrays (&optional (use-temp nil))

  (do ((new-array-name            (copy-list *bb-data-array-names*)           (cdr new-array-name))
       (new-array-initial-length  *bb-data-array-initial-lengths*             (cdr new-array-initial-length)))
      ((null new-array-name) nil)

    (if use-temp (setf (car new-array-name) (pack* *bb-file-prefix* (car new-array-name))))
    (eval `(setf ,(car new-array-name)
		 (create-data-array ',(car new-array-name) ,(car new-array-initial-length))))
    )
  )



;;;-------  COPY-BB-DATA-INTO-TEMP  -------

(defun copy-bb-data-into-temp nil
  "Copies the regular BB1 data hash tables into temporary hash tables."

  (dolist (table *bb-data-array-names*)
    (set (pack* *bb-file-prefix* table) (copy-hash-table (eval table)))
    )
  )



;;-------  SET-UP-BLACKBOARD  -------

;;;Edited by Mike Hewett   24 Apr 87  0:47
(defun set-up-blackboard (system)



  (if (valid-system? system)    ;-- They may have loaded knowledge bases first
      nil  ;;--> Annoying message! (bb1message "Merging system with previously-loaded system.")
    ;;ELSE
    (add-system system))

;; DATA

  (dolist (datum *input-data-names*)
    (add-data-to-system datum (second (assoc datum *input-data*)) system))


  )




;;-------  PARSE-INHERITANCE-INFO  -------

(defun parse-inheritance-info (inherit-sentence)

  ;; This routine parses the user's inheritance command, which should be something like:
  ;;
  ;;     (INHERIT [CACHABLE] {LINK | ATTRIBUTE} attr/link-name FROM link-name)
  ;;
  ;; MH: 02/09/86

  (if (eql (car inherit-sentence) 'inherit)

  ;; JUST MAKING SURE THEY GOT STARTED RIGHT!

      (let* ((cachable? (if (member (getf inherit-sentence 'inherit) '(cachable cacheable))
			    t
			  nil))
	     (dummy     (if (null cachable?)
			    (setf (cdr inherit-sentence)
				  (cons 'dummy (cdr inherit-sentence)))))
	     (attr/link (if (member 'attribute inherit-sentence)
			    (getf inherit-sentence 'attribute)
			    (getf inherit-sentence 'link)))
	     (link      (getf inherit-sentence 'from))
	     (type      (if (member 'attribute inherit-sentence)
			    'attribute
			    'link)))
          (ignore dummy) ;; -reed.  18 dec 86

	(make-bb-parsed-inheritancerec  :attribute-or-link    type
					:name                 attr/link
					:inheritance-record   (cons link cachable?))
	)
    )
  )

;;     DELETE ELEMENT ROUTINES


;;;-------  DELETE-ALL-INHERITANCE-OF-ATTRIBUTE  -------

(defun delete-all-inheritance-of-attribute (attribute)
  "Deletes all inheritance paths for this attribute."

  (if (valid-attribute-for-system? attribute *bb1-system-name*)
      (add-to-shortattribute-array attribute nil 'override)
      ;;ELSE
      (bb1error 'DELETE-ALL-INHERITANCE-OF-ATTRIBUTE
	        attribute " is not an existing attribute."
		)
      )
  )



;;;-------  DELETE-ALL-INHERITANCE-OF-LINK  -------

(defun delete-all-inheritance-of-link (link)
  "Deletes all inheritance paths for this link."

  (if (valid-link-for-system? link *bb1-system-name*)
      (add-to-shortlink-array link (opposite-link-of-link link) nil 'override)
      ;;ELSE
      (bb1error 'DELETE-ALL-INHERITANCE-OF-LINK
	        link " is not an existing link."
		)
      )
  )



;;;-------  DELETE-ALL-OBJECTS-OF-BLACKBOARD  -------

(defun delete-all-objects-of-blackboard (blackboard)
  "Does what it says!"

  (if (valid-blackboard? blackboard)
      (mapc #'delete-object
	    (objects-of-blackboard blackboard))
      ;;ELSE
      (bb1error 'DELETE-ALL-OBJECTS-OF-BLACKBOARD
	        blackboard " is not an existing blackboard."
		)
      )
  )



;;;-------  DELETE-ALL-OBJECTS-OF-LEVEL  -------

(defun delete-all-objects-of-level (level)
  "Does what it says!"

  (if (valid-level? level)
      (mapc #'delete-object
	    (objects-of-level level))
      ;;ELSE
      (bb1error 'DELETE-ALL-OBJECTS-OF-LEVEL
	        level " is not an existing level."
		)
      )
  )



;;-------  DELETE-ATTRIBUTE-OF-OBJECT  -------

(defun delete-attribute-of-object (attr object)

  ;;Modified 09/22/86  -mh

  (let ((long-attr (valid-attribute-for-object? attr object)))

    (cond (long-attr   (add-to-object-array object (short-name-of-object object)
					           (level-of-object object)
						   (blackboard-of-object object)
						   (remove-if #'(lambda (pair) (eql (car pair) attr))
							      (attributes-of-object object))
						   (links-of-object object)
						   ($links-of-object object)
						   'override)
		       (delete-bb-attribute-data long-attr))

	  (t           (bb1error 'DELETE-ATTRIBUTE-OF-OBJECT
				 attr " is not an attribute of " object ".")))
    )
  )



;;-------  DELETE-BLACKBOARD-OF-SYSTEM  -------

(defun delete-blackboard-of-system (blackboard system)
  "Deletes the given blackboard, plus its levels from the system."

  (let ((levels  (copy-list (levels-of-blackboard blackboard)))
	(bb-type (type-of-blackboard blackboard))
	(kb-name (knowledgebase-of-blackboard blackboard)))

    (dolist (lev levels)
      (delete-level-of-blackboard lev blackboard))

    (if kb-name              ; Need to remove it from the knowledgebase
	(add-to-knowledgebase-array   kb-name
				      (system-of-knowledgebase kb-name)
				      (remove blackboard
					      (blackboards-of-knowledgebase kb-name))
				      'override)
      ;;ELSE remove it from the system
      (case bb-type
	((DOMAIN USER::DOMAIN)    (add-to-system-array system
					(remove blackboard
						(domain-blackboards-of-system system))
					(control-blackboards-of-system system)
					(knowledgebases-of-system system)
					(data-elements-of-system system)
					'override))
	
	((CONTROL USER::CONTROL)  (add-to-system-array system
					(domain-blackboards-of-system system)
					(remove blackboard
						(control-blackboards-of-system system))
					(knowledgebases-of-system system)
					(data-elements-of-system system)
					'override))
	
	(otherwise (bb1error 'DELETE-BLACKBOARD-OF-SYSTEM
			     blackboard " has a funny type: " bb-type)))
      )

    (delete-bb-blackboard-data blackboard)
    )
  )



;;-------  DELETE-DATA  -------


(defun delete-data (data-name)

    (cond ((valid-data? data-name)
	
	               (add-to-system-array *bb1-system-name*
					    (domain-blackboards-of-system *bb1-system-name*)
					    (control-blackboards-of-system *bb1-system-name*)
					    (knowledgebases-of-system *bb1-system-name*)
					    (remove data-name (data-elements-of-system *bb1-system-name*))
					    'override)
		       (delete-bb-data-data data-name))

	  (t           (bb1error 'DELETE-DATA
				 data-name " is not a data element in the " *bb1-system-name* " system.")))
  )


;;-------  DELETE-KNOWN-LINK-FROM-OBJECT-TO-OBJECT  -------

(defun delete-known-link-from-object-to-object (long-link from-object to-object)

;;NOTE:  This is used by DELETE-LINK-OF-OBJECT.  It should not be called
;;       at any higher level because it only deletes a link in one direction.
;;       Links must *always* come in pairs, so this is a slightly dangerous
;;       function.

;; MH  02/07/86

  (add-to-link-array long-link
		     (short-name-of-link long-link)
		     (remove to-object (linked-objects-of-link long-link))
		     'override)

  (if (and (null (linked-objects-of-link long-link))
	   (valid-object? from-object))
      (let ((link (short-name-of-link long-link)))
	 (add-to-object-array from-object
			      (short-name-of-object from-object)
			      (level-of-object      from-object)
			      (blackboard-of-object from-object)
			      (attributes-of-object from-object)
			      (remove-if #'(lambda (link-pair) (eql (car link-pair) link))
					 (links-of-object from-object))
			      ($links-of-object     from-object)
			      'override)
	 (delete-bb-link-data long-link)))
					
  )



;;-------  DELETE-KNOWLEDGEBASE-OF-SYSTEM  -------

;;;Edited by Mike Hewett   20 Apr 87  12:40
(defun delete-knowledgebase-of-system (knowledgebase system)
  "Deletes the given knowledge base, plus its blackboards from the system."

  (if (valid-knowledgebase? knowledgebase)
      (if (valid-system? system)

	  (progn (dolist (bb (blackboards-of-knowledgebase knowledgebase))
		   (delete-blackboard-of-system bb system))
		 (add-to-system-array system
				      (domain-blackboards-of-system system)
				      (control-blackboards-of-system system)
				      (remove knowledgebase
					      (knowledgebases-of-system system))
				      (data-elements-of-system system)
				      'override)
		 (delete-bb-knowledgebase-data knowledgebase)
		 )

	;;ELSE - errors
	(bb1error 'DELETE-KNOWLEDGEBASE-OF-SYSTEM
		  "The " knowledgebase " knowledgebase does not belong to the " system " system.")
	)
    ;;ELSE - more errors
    (bb1error 'DELETE-KNOWLEDGEBASE-OF-SYSTEM
	      "The " knowledgebase " knowledgebase does not exist.")
    )
  )



;;-------  DELETE-LEVEL-OF-BLACKBOARD  -------

(defun delete-level-of-blackboard (level blackboard)
  "Deletes the given level, plus its objects from the system."

  (if (valid-level? level)

      (let ((objects  (copy-list (objects-of-level level))))
	
	(dolist (obj objects)
	  (delete-object obj))
	
	(add-to-blackboard-array blackboard
				 (system-of-blackboard               blackboard)
				 (type-of-blackboard                 blackboard)
				 (remove level (levels-of-blackboard blackboard))
				 (knowledgebase-of-blackboard        blackboard)
				 'override)

	(delete-bb-level-data level)
	)
    ;;;else
    (bb1error 'DELETE-LEVEL-OF-BLACKBOARD
	      level " is not an existing level.")
    )
  )




;;-------  DELETE-LINK-BETWEEN-OBJECTS  -------

(defun delete-link-between-objects (link object1 object2)

   "Deletes link (short name) running from object1 to object2, plus its opposite link."

;; This function is centered more around the link than the function
;; DELETE-LINK-OF-OBJECT.  That's why it is a separate function.

;; Link belongs to object1

;; MH  02/07/86

  (if (valid-object? object1)
      (if (valid-object? object2)

	  (let ((long-link-1 (valid-link-for-object? link object1))
		(long-link-2 (valid-link-for-object? (opposite-link-of-link link) object2)))

	    (if long-link-1
		(if long-link-2
		    (progn (delete-known-link-from-object-to-object long-link-1 object1 object2)
			   (delete-known-link-from-object-to-object long-link-2 object2 object1))
		    (bb1error 'DELETE-LINK-BETWEEN-OBJECTS
			      object2 " does not have a " (opposite-link-of-link link) " link."))
	        (bb1error 'DELETE-LINK-BETWEEN-OBJECTS
			  object1 " does not have a " link " link."))
	    )
       ;;else
	  (bb1error 'DELETE-LINK-BETWEEN-OBJECTS
		    object2 " is not a blackboard object."))
   ;;else
      (bb1error 'DELETE-LINK-BETWEEN-OBJECTS
		object1 " is not a blackboard object."))
  )



;;-------  DELETE-LINK-OF-OBJECT  -------

(defun delete-link-of-object (link object)

;; LINK should be the short link name.

  (let ((long-link (valid-link-for-object? link object)))

    (if long-link
	(let ((opplink (opposite-link-of-link link)))

	  (dolist (linked-object (linked-objects-of-link long-link))
	          (delete-known-link-from-object-to-object (make-extended-name linked-object opplink)
							   linked-object object))
	  (delete-bb-link-data long-link)
	  (add-to-object-array object (short-name-of-object object)
			              (level-of-object      object)
				      (blackboard-of-object object)
				      (attributes-of-object object)
				      (remove-if #'(lambda (pair) (eql (car pair) link))
						 (links-of-object object))
				      ($links-of-object     object)
				      'override)
	  )

  ;;else
	(bb1error 'DELETE-LINK-OF-OBJECT
		  link " is not a link of " object ".")
      )
    long-link)
  )




;;;-------  DELETE-LINK-OF-SYSTEM  -------

(defun delete-link-of-system (link system)
  "Deletes the link from the system, and removes it from all objects."

  ;; Stolen from Alan Garvey's CLEANUP-LINKS function

  (if (valid-system? system)
      (if (valid-link-for-system? link system)
	  (progn (dolist (obj (objects-of-system system))
		   (if (valid-link-for-object? link obj)
		       (delete-link-of-object link obj)))
		 (delete-bb-shortlink-data link))
	  ;;Else - invalid link
	  (BB1ERROR 'DELETE-LINK-OF-SYSTEM
		    "The link " link " is not defined in the " system " system.")
	  )
      ;;ELSE - invalid system
      (BB1ERROR 'DELETE-POSSIBLE-LINK-OF-SYSTEM
		"The system " system " does not exist.")
      )
  )



;;-------  DELETE-OBJECT  -------

(defun delete-object (object)

  (if (valid-object? object)

      (let ((level (level-of-object object)))

	(add-to-level-array level
			    (short-name-of-level level)
			    (blackboard-of-level level)
			    (remove object (objects-of-level level))
			    'override)
	(dolist (attribute (mapcar #'car (attributes-of-object object)))
	        (delete-attribute-of-object attribute object))
	(dolist (link (mapcar #'car (links-of-object object)))
	        (delete-link-of-object link object))
	(delete-bb-object-data object)
	)

  ;;else
      (bb1error 'DELETE-OBJECT
		object " is not a blackboard object.")
    )
  )



;;-------  DELETE-POSSIBLE-ATTRIBUTE-OF-SYSTEM  -------

(defun delete-possible-attribute-of-system (attribute system)
  "Deletes the attribute from the system and removes it from any
objects which have it."

  ;; First written by Alan Garvey

  (if (valid-system? system)
      (if (valid-attribute-for-system? attribute system)
	  (progn (dolist (obj (objects-of-system system))
		   (if (valid-attribute-for-object? attribute obj)
		       (delete-attribute-of-object attribute obj)))
		 (delete-bb-shortattribute-data attribute))
	  ;;Else - invalid attribute
	  (BB1ERROR 'DELETE-POSSIBLE-ATTRIBUTE-OF-SYSTEM
		    "The attribute " attribute " is not defined in the " system " system.")
	  )
      ;;ELSE - invalid system
      (BB1ERROR 'DELETE-POSSIBLE-ATTRIBUTE-OF-SYSTEM
		"The system " system " does not exist.")
      )
  )



;;-------  DELETE-PROCEDURAL-ATTACHMENT-OF-ATTRIBUTE  -------

(defun delete-procedural-attachment-of-attribute (attribute)
  "Removes the procedural attachment of the attribute and
   clears any VALUES."

  (if (valid-attribute? attribute)
      (add-to-attribute-array attribute
			      (short-name-of-attribute attribute)
			      (object-of-attribute     attribute)
			      nil                                    ;Erase VALUES
			      nil                                    ;Erase Procedural Attachment
			      'OVERRIDE)
    ;;ELSE
    (bb1error 'DELETE-PROCEDURAL-ATTACHMENT-OF-ATTRIBUTE
	      attribute " is not an existing attribute.")
    )
  )


;;; - - - - - - -  COPY  - - - - - - -  COPY  - - - - - - -  COPY  - - - - - - -


(defun copy-object (old-object new-level new-object-name)
  "Makes a new object with all of the same attributes as old-object.
Links are not copied.  OLD-OBJECT and NEW-LEVEL must be long names.  If the
new object already exists, it is deleted and recreated as a copy of the old-object."

  (if (valid-object? old-object)
      (let ((new-long-name (make-extended-name new-level new-object-name)))
	(if (valid-object? new-long-name) (delete-object new-long-name))
	(if (add-object-to-level new-object-name new-level)
	    (dolist (attr-info (attributes-of-object old-object))
	      (add-attribute-to-object (car attr-info)
				       (car (values-of-attribute (cdr attr-info))) new-long-name)
	      (if (procedural-attachment-of-attribute (cdr attr-info))
		  (add-procedural-attachment-to-attribute (procedural-attachment-of-attribute (cdr attr-info))
							  (cdr attr-info)))
	      )
	    ;;Else - some problem with add-object-to-level
	    (BB1ERROR 'COPY-OBJECT
		      "Unable to perform ADD-OBJECT-TO-LEVEL, object= " new-object-name
		      ", level= " new-level)
	    )
	)
      ;;ELSE - nothing to copy!
      (BB1ERROR 'COPY-OBJECT
		"The object " old-object " doesn't exist.")
      )
  )



(defun copy-level (old-level new-blackboard new-level-name)
  "Makes a new level with copies of all of the same objects as old-object.
Links are not copied.  OLD-LEVEL must be a long name.  If the
new level already exists, it is deleted and recreated as a copy of the old level."

  (if (valid-level? old-level)
      (let ((new-long-name (make-extended-name new-blackboard new-level-name)))
	(if (valid-level? new-long-name) (delete-level-of-blackboard new-long-name new-blackboard))
	(if (add-level-to-blackboard new-level-name new-blackboard)
	    (dolist (object (objects-of-level old-level))
	      (copy-object object new-long-name (short-name-of-object object))
	      )
	    ;;Else - some problem with add-level-to-blackboard
	    (BB1ERROR 'COPY-LEVEL
		      "Unable to perform ADD-LEVEL-TO-BLACKBOARD, level= " new-level-name
		      ", blackboard= " new-blackboard)
	    )
	)
      ;;ELSE - nothing to copy!
      (BB1ERROR 'COPY-LEVEL
		"The level " old-level " doesn't exist.")
      )
  )


  

;;; - - - - - - -  RENAME  - - - - - -  RENAME  - - - - - - -  RENAME  - - - -


(defun rename-system (system new-name)
  "Renames the given system."

  (if (valid-system? system)
      (if (valid-system? new-name)
	  (bb1error 'RENAME-SYSTEM
		    new-name " is already a defined system.")
	;;;ELSE
	(progn (put-bb-system-data new-name (get-bb-system-data system))
	       (dolist (bb (blackboards-of-system new-name))
		 (add-to-blackboard-array bb new-name nil nil nil 'update)
		 )
	       (dolist (kb (knowledgebases-of-system new-name))
		 (add-to-knowledgebase-array kb new-name nil 'update)
		 )
	       (delete-bb-system-data system)
	       (setf *BB1-SYSTEM-NAME* new-name)
	       )
	)
    (bb1error 'RENAME-SYSTEM
	      system " is not an existing system.")
    )
  )


(defun rename-knowledgebase (knowledgebase new-name)
  "Renames the given knowledgebase."

  (if (valid-knowledgebase? knowledgebase)
      (if (valid-knowledgebase? new-name)
	  (bb1error 'RENAME-KNOWLEDGEBASE
		    new-name " is already a defined knowledge base.")

	;;;ELSE - all is okay
	(progn (put-bb-knowledgebase-data new-name (get-bb-knowledgebase-data knowledgebase))
	       (dolist (bb (blackboards-of-knowledgebase new-name))
		 (add-to-blackboard-array bb nil nil nil new-name 'update)
		 )
	       (add-to-system-array *bb1-system-name*
				    (domain-blackboards-of-system *bb1-system-name*)
				    (control-blackboards-of-system *bb1-system-name*)
				    (remove knowledgebase
					    (knowledgebases-of-system *bb1-system-name*))
				    (data-elements-of-system *bb1-system-name*)
				    'override)

	       (add-to-system-array *bb1-system-name* nil nil new-name nil 'update)
	       (delete-bb-knowledgebase-data knowledgebase)
	       new-name
	       )
	)
    (bb1error 'RENAME-KNOWLEDGEBASE
	      knowledgebase " is not an existing knowledgebase.")
    )
  )


(defun rename-blackboard (blackboard new-name)
  "Renames the given blackboard."

  ;; Added 09/22/86

  (if (valid-blackboard? blackboard)
      (if (valid-blackboard? new-name)
	  (bb1error 'RENAME-BLACKBOARD
		    new-name " is already an existing blackboard."
		    t "Can't rename " blackboard ".")

	   ;ELSE - okay to rename it
	  (add-to-blackboard-array new-name *BB1-SYSTEM-NAME* (type-of-blackboard blackboard)
				   nil (knowledgebase-of-blackboard blackboard) 'override)
	  (if (knowledgebase-of-blackboard blackboard)
	      (add-to-knowledgebase-array (knowledgebase-of-blackboard blackboard)
					  nil
					  new-name
					  'update)
	      ;;ELSE - doesn't belong to a knowledgebase
	      (case (type-of-blackboard blackboard)
		    (CONTROL   (add-to-system-array *BB1-SYSTEM-NAME* nil new-name nil nil 'update))
		    (DOMAIN    (add-to-system-array *BB1-SYSTEM-NAME* new-name nil nil nil 'update))
		    (otherwise (bb1error 'RENAME-BLACKBOARD
					 "Strange TYPE-OF-BLACKBOARD - internal error: "
					 (type-of-blackboard blackboard)))
		    )
	      )
	  (dolist (level (levels-of-blackboard blackboard))
	    (rename-level level (short-name-of-level level) new-name))
	  (delete-blackboard-of-system blackboard *BB1-SYSTEM-NAME*)
	  )
      ;;ELSE - not a valid blackboard
      (bb1error 'RENAME-BLACKBOARD
		blackboard " is not an existing blackboard.")
      )
  )



(defun rename-level (level new-name &OPTIONAL (blackboard nil))
  "Renames the given level.  Also used by RENAME-BLACKBOARD to move a level
from one blackboard to another."


  (if (valid-level? level)
      (let* ((bboard (or blackboard (blackboard-of-level level)))
	     (long-new-name (make-extended-name bboard new-name)))

	(if (valid-level? long-new-name)
	    (bb1error 'RENAME-LEVEL
		      long-new-name " is already the name of a level." T
		      level " not renamed.")
	    ;;ELSE - okay to rename it
	    (add-to-blackboard-array bboard nil nil long-new-name nil 'update)
	    (add-to-level-array long-new-name new-name bboard nil 'override)
	    (dolist (obj (objects-of-level level))
	      (rename-object obj (short-name-of-object obj) long-new-name bboard))
	    (delete-level-of-blackboard level (blackboard-of-level level))
	    )
	)
      ;ELSE
      (bb1error 'RENAME-LEVEL level " is not an existing level.")
      )
  )



(defun rename-object (object new-name &OPTIONAL (lev nil) (blackboard nil))

  (let* ((level         (or lev (level-of-object object)))
	 (bboard        (or blackboard (blackboard-of-object object)))
	 (long-new-name (make-extended-name bboard (short-name-of-level level) new-name))
	 )

    (if (valid-object? object)
	(if (valid-object? long-new-name)
	    (bb1error 'RENAME-OBJECT
		      long-new-name " already exists on the blackboard.  Can't rename.")

	  ;;Else - okay to rename
	  (add-to-level-array level nil nil long-new-name 'update)
	  (add-to-object-array long-new-name new-name level bboard nil nil nil 'override)
	  (dolist (attr (attributes-of-object object))
	    (add-to-attribute-array (add-attribute-to-object (car attr) nil long-new-name)
				    (car attr)
				    long-new-name
				    (values-of-attribute (cdr attr))
				    (procedural-attachment-of-attribute (cdr attr))
				    'override)
		   )
	  (dolist (link (links-of-object object))
	    (dolist (to-obj (linked-objects-of-link (cdr link)))
	      (add-link-from-object-to-object (car link) long-new-name to-obj)
	      (add-link-from-object-to-object (opposite-link-of-link (car link))
					      to-obj long-new-name)
	      )
	    )
	  
	  (delete-object object)
	  )
	;Else - Object to be renamed does not exist.
	(bb1error 'RENAME-OBJECT
		  object " is not an existing blackboard object.")
	)
    )
  )



;;;;-------  RENAME-ATTRIBUTE  -------

(defun rename-attribute (attr new-name &OPTIONAL (system *bb1-system-name*) &AUX attr-info)
  "Renames an attribute, pervasive through all objects."

  (if (valid-attribute-for-system? attr system)
      (if (not (valid-attribute-for-system? new-name system))
	  (progn (put-bb-shortattribute-data new-name (get-bb-shortattribute-data attr))    ;Make  new attribute
		 (delete-bb-shortattribute-data attr)                                       ;Delete old attribute 
		 (mapc #'(lambda (object)
			   (when (setq attr-info (assoc attr (attributes-of-object object)))  ;if obj has attr
			     (let ((long-attr-name  (make-extended-name object new-name))
				   (old-attr-data   (get-bb-attribute-data (cdr attr-info)))
				   )
			       (setf (car attr-info) new-name)                               ;Change obj record
			       (setf (cdr attr-info) long-attr-name)                         ;"   "   "    "
			       (setf (bb-attributeinforec-short-name old-attr-data) new-name)
			       (put-bb-attribute-data long-attr-name old-attr-data)
			       )			     
			     )
			   )
		       (objects-of-system system)
		       )
		 )
			       
	  ;;ELSE - can't rename to one that already exists
	  (bb1error 'RENAME-ATTRIBUTE
		    new-name " is already an attribute of " system ".")
	  )
      ;;ELSE - not a valid attribute
      (bb1error 'RENAME-ATTRIBUTE
		attr " is not an attribute of " system ".")
      )
  )




;;;-------------  RUN-TIME ROUTINES



;;-------  ALL-ATTRIBUTES-OF-SYSTEM  -------

(defun all-attributes-of-system (system)
   (ignore system) ;; -reed.  18 dec 86
  (let ((temp nil))
    (maphash #'(lambda (key ignore) (push key temp)) *bb-shortattributeinfo*)
    (reverse temp)
    )
  )


;;-------  ALL-LINKS-OF-SYSTEM  -------

(defun all-links-of-system (system)
   (ignore system) ;; -reed.  18 dec 86
  (let ((temp nil))
    (maphash #'(lambda (key ignore) (push key temp)) *bb-shortlinkinfo*)
    (reverse temp)
    )
  )


;;--------  ALL-KSES-OF-SYSTEM  -------

(defun all-kses-of-system (&OPTIONAL (system *BB1-SYSTEM-NAME*)
			   &KEY      (check-for-errors nil))
  "Returns a list of the long names of all of the known kses in the system."

  ;;modified 03/23/87  - mike

  (declare (ignore check-for-errors))

  (let ((skill-kses (if (and (boundp '*current-skill*) (valid-object? *current-skill*))
			($objects *current-skill* 'user::implementedby)    ;  KS objects are in the Concept hierarchy
			;;ELSE
			NIL))
	)

    (or skill-kses
	(let (levels-of-bb)            ; Check for KS blackboards
	  (mapcan #'(lambda (bb)
		      (setq levels-of-bb
			    (mapcar #'(lambda (lev) (make-extended-name bb lev))
				    '(DOMAIN CONTROL LEARNING)))
		      (if (every #'(lambda (lev) (valid-level? lev))
				 levels-of-bb)
			  (mapcan #'$objects-at-level levels-of-bb)
			  ;;ELSE - not a KS blackboard
			  nil)
		      )
		  (blackboards-of-system system)
		  )
	  ))
    )
  )

    

;;-------  VERIFY-KSES  -------

(defun verify-kses (&OPTIONAL (system *BB1-SYSTEM-NAME*))
  "Verifies that all kses are linked to the *CURRENT-SKILL* object."

  (cond ((or (not (boundp '*CURRENT-SKILL*))
	     (null *CURRENT-SKILL*))               (bb1message "You are not using *CURRENT-SKILL*."))

	((not (valid-object? *CURRENT-SKILL*))     (bb1message "*CURRENT-SKILL* is bound to "
							       *CURRENT-SKILL* " which is not a valid object."))
	)

  (let ((skill-kses (if (and (boundp '*current-skill*) (valid-object? *current-skill*))
			($objects *current-skill* 'user::implementedby)    ;  KS objects are in the Concept hierarchy
			;;ELSE
			NIL))

	(bb-kses    (let (levels-of-bb)            ; Check for KS blackboards
		      (mapcan #'(lambda (bb)
				  (setq levels-of-bb
					(mapcar #'(lambda (lev) (make-extended-name bb lev))
						'(DOMAIN CONTROL LEARNING)))
				  (if (every #'(lambda (lev) (valid-level? lev))
					     levels-of-bb)
				      (mapcan #'$objects-at-level levels-of-bb)
				        ;;ELSE - not a KS blackboard
				      nil)
				  )
			      (blackboards-of-system system)
			      )
		      ))
	)

    (cond ((and (null skill-kses)
		(null bb-kses))          (bb1warning 'ALL-KSES-OF-SYSTEM
						     "I can't find any knowledge sources for the "
						     *BB1-SYSTEM-NAME* " system."))

	  ((and skill-kses
		(not (equal skill-kses bb-kses))
		(subsetp skill-kses bb-kses))    (bb1message "You have some knowledge sources which are not"
							     " linked to the " T *CURRENT-SKILL*
							     " skill.  Select any of these KSes "
							     "you want to link.")
	                                         (let ((other-kses (bb1-select-some-of
								     "Select KSes to use"
								     (sort-simple-list
								       (set-difference bb-kses
										       skill-kses))))
						       )
						   (mapc #'(lambda (ks)
							     (add-link-from-object-to-object
							       'user::implements ks *CURRENT-SKILL*)
							     (add-link-from-object-to-object
							       'user::implementedby *CURRENT-SKILL* ks))
							 other-kses)
						   ($output "Here are the knowledge sources which will be used:"
							    T T (sort-simple-list
								  (union skill-kses other-kses)))
						   ))
	  (t           ($Output "Here are the knowledge sources which will be used:"
				T T (sort-simple-list (or skill-kses bb-kses)))
		       )
	  )
    )
  )


;------------------------------------------------------------------------------------
; OLD ALL-KSES-OF-SYSTEM   "Life was so much simpler then,
;                                time's rewritten every line.
;                                If we had the chance to do it all again,
;                                tell me would we?  ...could we?"                  -B. Streisand
;
;  (copy-tree (objects-of-level (make-extended-name 'control-data.knowledge-source)))
;
;------------------------------------------------------------------------------------



;;-------  BB1-FIX-UP-LINKS-FOR-RUN  -------

;;;Edited by Mike Hewett   27 Apr 87  14:05
(defun bb1-fix-up-links-for-run (&KEY (verbose nil))
  "Goes through all the links in the system.  If the object linked doesn't exist, it
deletes the link.  If the object exists, but the opposite link doesn't, it creates the
opposite link."

  (let (opplink
	)

       ;; Loops through all objects, examining all of each object's links

    (maphash #'(lambda (long-object-name object-info)
		 
		    (declare (ignore object-info))

		 (dolist (long-link (copy-list (actual-links-of-object long-object-name)))
		   (setq opplink (opposite-link-of-link (short-name-of-link long-link)))
		   (dolist (obj (linked-objects-of-link long-link))
		     (if (valid-object? obj)  ; Let ADD-LINK... do all the work

			 (add-link-from-object-to-object opplink obj long-object-name :verbose verbose)

		       ;;ELSE - link to non-existent object.  Delete this link
			 (delete-known-link-from-object-to-object long-link long-object-name obj)
			 (when verbose
			   (bb1message t "Deleting link " (short-name-of-link long-link)
				       " from " long-object-name " to " obj)
			   )
			 )
		     )
		   )
		 )

	     *bb-objectinfo*
	     )
    )
  )




;;;-------  BB1-FIX-UP-LINKS-FOR-SAVE  -------

;;;Edited by Mike Hewett   24 Apr 87  0:47
(defun bb1-fix-up-links-for-save ()
  "Similar to BB1-FIX-UP-LINKS-FOR-RUN except that it won't add links
in opposite directions.  It will only delete one-way links."

  ;; Loops through all objects, examining all of each object's links
  
  (maphash #'(lambda (long-object-name object-info)
	       
	       (declare (ignore object-info))
	       
	       (dolist (long-link (copy-list (actual-links-of-object long-object-name)))
		 (dolist (obj (linked-objects-of-link long-link))
		   (if (not (valid-object? obj))   ;; link to non-existent object.  Delete this link	            
		       (delete-known-link-from-object-to-object long-link long-object-name obj)
		       )
		   )
		 )
	       )
	   
	   *bb-objectinfo*
	   )
  )




;;-------  BB1-KNOWLEDGE-BASE-CHECK-AND-INTEGRATE  -------

(defun bb1-knowledge-base-check-and-integrate (kb-name &optional (override nil) (query nil) (in-ksedit? nil))
  "Integrates the data from hash arrays on a file into the current hash arrays.
   OVERRIDE causes the old hash array information to be lost.  Otherwise does a MERGE.
   QUERY will ask before deleting the old system."

;;; If OVERRIDE is non-nil, no checks will be done and the file arrays will replace
;;; the current arrays.  

  (if (or override (null (current-system-name)))
      (if (or (null (current-system-name))
	      (not query)
	      (bb1-y-or-n-p "Okay to replace the current system?"))
	  (progn (mapc #'(lambda (array-name)
			   (set array-name (eval (pack* *bb-file-prefix* array-name))))
		       *bb-data-array-names*)
		 (setf *BB1-SYSTEM-NAME* (current-system-name))
		 (unless in-ksedit?
		   (bb1-load-display-information *BB1-SYSTEM-NAME* *bb1-bb-window*
						 kb-name)
		   )
		 )
	;;;else
	(bb1message "New system not loaded."))

    ;;;ELSE perform a MERGE operation
    (if (check-kb-compatibility)
	(let* ((current-sys-name               (current-system-name))
	       (file-system-data-name         (pack* *bb-file-prefix* '*bb-systeminfo*))
	       (file-systeminfo               (gethash (current-system-name 'file)
						       (eval file-system-data-name)))
	       (file-shortattribute-data-name (pack* *bb-file-prefix* '*bb-shortattributeinfo*))
	       (file-shortlink-data-name      (pack* *bb-file-prefix* '*bb-shortlinkinfo*))
	       (file-knowledgebase-data-name  (pack* *bb-file-prefix* '*bb-knowledgebaseinfo*))
	       (file-blackboard-data-name     (pack* *bb-file-prefix* '*bb-blackboardinfo*))
	       (file-level-data-name          (pack* *bb-file-prefix* '*bb-levelinfo*))
	       (file-object-data-name         (pack* *bb-file-prefix* '*bb-objectinfo*))
	       (file-attribute-data-name      (pack* *bb-file-prefix* '*bb-attributeinfo*))
	       (file-link-data-name           (pack* *bb-file-prefix* '*bb-linkinfo*))
	       (file-data-data-name           (pack* *bb-file-prefix* '*bb-datainfo*))
	       )

	  (setf *BB1-SYSTEM-NAME* current-sys-name)    ; Necessary because loaded KB overwrites the variable

;	  (if (eql (current-system-name) *bb1-system-name*)
;	      (bb1message "Integrating the new knowledge base into the system."))
	
	  ;; FIRST THE SYSTEM INFORMATION
	
	  (mapc #'(lambda (domain-bb)
		    (add-to-system-array *bb1-system-name* domain-bb nil nil nil 'update))
		(bb-systeminforec-domain-blackboards file-systeminfo))
	
	  (mapc #'(lambda (control-bb)
		    (add-to-system-array *bb1-system-name* nil control-bb nil nil 'update))
		(bb-systeminforec-control-blackboards file-systeminfo))
	
	  (mapc #'(lambda (kb)
		    (add-to-system-array *bb1-system-name* nil nil kb nil 'update))
		(bb-systeminforec-knowledgebases file-systeminfo))
	
	  (mapc #'(lambda (data)
		    (add-to-system-array *bb1-system-name* nil nil nil data 'update))
		(bb-systeminforec-data-elements file-systeminfo))
	
	  ;; THE REST OF THE RECORDS CAN JUST BE COPIED DIRECTLY INTO THE HASH ARRAYS
	
	  (maphash #'(lambda (key val)                       ;be sure and save inheritance info
		       (if (get-bb-shortattribute-data key)
			   (mapcar #'(lambda (inh-opp)
				       (add-to-shortattribute-array key inh-opp 'update)
				       )
				   (bb-shortattributeinforec-inheritance-opportunities val)
				   )
			   ;;ELSE - new record
			   (put-bb-shortattribute-data key val))
		       )
		   (eval file-shortattribute-data-name))

	  (maphash #'(lambda (key val)                       ;be sure and save the inheritance info
		       (if (get-bb-shortlink-data key)
			   (mapcar #'(lambda (inh-opp)
				       (add-to-shortlink-array key nil inh-opp 'update)
				       )
				   (bb-shortlinkinforec-inheritance-opportunities val)
				   )
			   ;;ELSE - new record
			   (put-bb-shortlink-data key val))
		       )
		   (eval file-shortlink-data-name))

	  (maphash #'(lambda (key val)
		       (put-bb-blackboard-data key val))
		   (eval file-blackboard-data-name))

	    ;;Make sure all blackboards point to the current system name

	  (maphash #'(lambda (key val)
		       (declare (ignore val))
		       (add-to-blackboard-array key *BB1-SYSTEM-NAME* nil nil nil 'UPDATE)
		       )
		   *BB-BLACKBOARDINFO*)
	

	  (maphash #'(lambda (key val)
		       (put-bb-knowledgebase-data key val))
		   (eval file-knowledgebase-data-name))
	
	    ;;Make sure all knowledge bases point to the current system name

	  (maphash #'(lambda (key val)
		       (declare (ignore val))
		       (add-to-knowledgebase-array key *BB1-SYSTEM-NAME* nil 'UPDATE)
		       )
		   *BB-KNOWLEDGEBASEINFO*)


	  (maphash #'(lambda (key val)
		       (put-bb-level-data key val))
		   (eval file-level-data-name))
	
	  (maphash #'(lambda (key val)
		       (put-bb-object-data key val))
		   (eval file-object-data-name))
	
	  (maphash #'(lambda (key val)
		       (put-bb-attribute-data key val))
		   (eval file-attribute-data-name))
	
	  (maphash #'(lambda (key val)
		       (put-bb-link-data key val))
		   (eval file-link-data-name))
	
	  (maphash #'(lambda (key val)
		       (let ((old-data (valid-data? key)))
			 (if old-data
			     (if (atom old-data)
				 (put-bb-data-data key (list old-data val))
			       (put-bb-data-data key (append old-data (list val))))
			   (put-bb-data-data key val)))
		       )			
		   (eval file-data-data-name))
	  (unless in-ksedit?
	    (bb1-load-display-information (current-system-name) *bb1-bb-window*
					  kb-name)
	    )
	  )
      ;;else
      (bb1message "Sorry, that knowledge base is incompatible with this system.")
      )
    )
  )




;;-------  BBADD  -------

(defun bbadd (level attrs links friendlyname ksar ksname &optional (suppress nil))

  ;; This is the main high-level routine which adds objects to the blackboard.
  ;;
  ;; LEVEL          - An existing level on any blackboard.
  ;; ATTRS          - An already-evaluated attribute-value list.
  ;; LINKS          - An already-evaluated attribute-value list.
  ;; FRIENDLYNAME   - The name of the new object.  If none is given, a default name will be used.
  ;; KSAR           - Which ksar is making this object?
  ;; KSNAME         - What is the name of the KS of the KSAR.
  ;; SUPPRESS       - If non-NIL, suppresses the call to NEWEVENT. Used internally.
  ;;
  ;; MH:  02/09/86


  (let ((objectname (add-object-to-level (or friendlyname
					     (generate-object-name level))
					 level)))
    (if (valid-object? objectname)
	(progn  (dolist (atval attrs)
		  (if (valid-attribute-for-object? (car atval) objectname)
		      (store-attribute objectname (car atval) (cadr atval))
		    (add-attribute-to-object (car atval) (cadr atval) objectname))
		  )

		(dolist (ln links)
		  (dolist (to-object (if (listp (cadr ln))
					 (cadr ln)
				       (cdr ln)))
		    (add-link-from-object-to-object (car ln) objectname to-object)
		    (add-link-from-object-to-object (opposite-link-of-link (car ln)) to-object objectname)))
		
		(if (or suppress (eql level (make-extended-name 'control-data 'events)))
		    objectname
		  ;;ELSE
		  (progn   (new-event 'USER::add objectname (append attrs links) ksar ksname level)
			   ($eval *bb1-event-display-fn*)
			   objectname)
		  ))
      ;;ELSE NOTHING
	)
    )
  )


;;-------  BBMODIFY  -------

(defun bbmodify (object level attrs links friendlyname ksar ksname &optional (suppress nil))

  ;; This is the main high-level routine which modifies objects on the blackboard.
  ;;
  ;; LEVEL          - An existing level on any blackboard.
  ;; ATTRS          - An already-evaluated attribute-value list.
  ;; LINKS          - An already-evaluated attribute-value list.
  ;; FRIENDLYNAME   - The name of the new object.  If none is given, a default name will be used.
  ;; KSAR           - Which ksar is making this object?
  ;; KSNAME         - What is the name of the KS of the KSAR.
  ;; SUPPRESS       - If non-NIL, suppresses the call to NEWEVENT. Used internally.
  ;;
  ;; MH:  02/09/86


  (if (not (valid-object? object))
      (bb1error 'BBMODIFY
		"Object to modify: " object " is not an existing object.")

    ;;ELSE
      (let* ((objectname    object)
	     (objectlevel   (or level (level-of-object objectname))))

	(dolist (atval attrs)
	  (if (valid-attribute-for-object? (car atval) objectname)
	      (store-attribute objectname (car atval) (cadr atval))
	    (add-attribute-to-object (car atval) (cadr atval) objectname)))
	
	(dolist (ln links)
	  (dolist (to-object (if (listp (cadr ln))
				 (cadr ln)
			       (list (cadr ln))))
	    (add-link-from-object-to-object (car ln) objectname to-object)
	    (add-link-from-object-to-object (opposite-link-of-link (car ln)) to-object objectname)))
	
	(if friendlyname
	    ($name objectname friendlyname))

	
	(if (or suppress (eql objectlevel (make-extended-name 'control-data 'events)))
	    objectname
	  ;;ELSE
	  (progn   (new-event 'USER::modify (or friendlyname objectname) (append attrs links) ksar ksname
			      objectlevel)
		   ($eval *bb1-event-display-fn*)
		   objectname)
	  )
	)
      )
  )



;;;Edited by Mike Hewett   19 Apr 87  21:29
(defvar *INTEGRITY-ERROR-P* nil
  "Flag to tell whether integrity check turned up any errors."
  )



;;-------  CHECK-BB-INTEGRITY  -------

;;;Edited by Mike Hewett   19 Apr 87  21:29
;;;Edited by Mike Hewett   19 Apr 87  21:41
(defun check-bb-integrity (&OPTIONAL (break-on-error t))
  "*&!@&*@ TI Explorer seems to lose hash table entries during a long (24-hour) run.  May
have something to do with the GC.  This routine checks all known BB entries to make
sure all the hash table entries are there."

  (setf *INTEGRITY-ERROR-P* nil)

  (if *BB1-SYSTEM-NAME*
      (let ((kbs  (knowledgebases-of-system *BB1-SYSTEM-NAME*))
	    (bbs  (blackboards-of-system    *BB1-SYSTEM-NAME*))
	    (data (data-elements-of-system  *BB1-SYSTEM-NAME*))
	    )

	   ;;Check DATA of system

	(dolist (datum data)
	  (unless (valid-data? datum)
	    (integrity-error "DATA" datum)
	    )
	  )
	
	   ;;Check KBs of system

	(dolist (kb kbs)
	  (unless (valid-knowledgebase? kb)
	    (integrity-error "KNOWLEDGE BASE" kb)
	    )
	  )

	   ;;Check BBs of system

	(dolist (bb bbs)
	  (if (valid-blackboard? bb)
	      (check-blackboard-integrity bb)
	      ;;Else - record is missing!
	      (integrity-error "BLACKBOARD" bb)
	      )
	  )

	(format *trace-output* "~%Integrity check finished.~%")

	   ;;ANY ERRORS??
	(when (and *INTEGRITY-ERROR-P*
		   break-on-error)
	  (cerror :YES nil nil
		  "There were errors during the BB1 self-test.  See BB1 frame background stream."
		  )
	  )
	)

      ;;ELSE - no system yet
      (format *trace-output* "~%INTEGRITY CHECK: There is no existing system.~%")
      )

  )



;;-------  INTEGRITY-ERROR  -------

;;;Edited by Mike Hewett   19 Apr 87  21:29
(defun integrity-error (type data)
  "Prints an error message to *TRACE-OUTPUT*."

  (format *trace-output* "~%INTEGRITY ERROR:  There is no ~A entry for ~A." type data)
  (unless *INTEGRITY-ERROR-P*
    (setf *INTEGRITY-ERROR-P* t)
    )
  )


;;-------  CHECK-BLACKBOARD-INTEGRITY  -------

;;;Edited by Mike Hewett   19 Apr 87  21:29
(defun check-blackboard-integrity (bb-name)
  "Know BB record exists...check it's levels and objects."

  (dolist (level (levels-of-blackboard bb-name))
    (if (valid-level? level)
	(check-level-integrity level)
	;;ELSE - doesn't exist!!
	(integrity-error "LEVEL" level)
	)
    )
  )



;;-------  CHECK-LEVEL-INTEGRITY  -------

;;;Edited by Mike Hewett   19 Apr 87  21:29
(defun check-level-integrity (level-name)
  "Know LEVEL record exists...check it's objects."

  (dolist (object (objects-of-level level-name))
    (if (valid-object? object)
	(check-object-integrity object)
	;;ELSE - doesn't exist!!
	(integrity-error "OBJECT" object)
	)
    )
  )



;;-------  CHECK-OBJECT-INTEGRITY  -------

;;;Edited by Mike Hewett   19 Apr 87  21:29
(defun check-object-integrity (object-name)
  "Know OBJECT record exists...check it's attributes and links."

    ;;Check attributes first

  (dolist (attr (actual-attributes-of-object object-name))
    (unless (valid-attribute? attr)
      (integrity-error "ATTRIBUTE" attr)
      )
    )

    ;;Then Check LINKS

  (dolist (link (actual-links-of-object object-name))
    (unless (valid-link? link)
      (integrity-error "LINK" link)
      )
    )
  )






;;-------  CHECK-KB-COMPATIBILITY  -------

(defun check-kb-compatibility ()
  "Makes sure a knowledge-base being loaded in is compatible with the current one."

;;; This is necessarily an incomplete check.  There may be many rare incompatibilities
;;; which will be hard to find.  This will find most of them.

    (let* ((file-system-data-name (pack* *bb-file-prefix* '*bb-systeminfo*))
	   (file-system-name (current-system-name 'file))
	   (file-link-name   (pack* *bb-file-prefix* '*bb-shortlinkinfo*))
	   (file-system-data (gethash file-system-name (eval file-system-data-name)))
	   (file-blackboards (append (bb-systeminforec-domain-blackboards  file-system-data)
				     (bb-systeminforec-control-blackboards file-system-data)))
	   (file-links       (let ((temp nil))
			       (maphash #'(lambda (key ignore) (push key temp)) (eval file-link-name))
			       temp))
	   )
;No longer an error to have a different name
;      (if (not (eql file-system-name *bb1-system-name*))
;	  (bb1error 'CHECK-KB-COMPATIBILITY
;		    "The system on file is " file-system-name
;		    ", while the current system is " *bb1-system-name*
;		    ".  Unable to integrate them.")
	(if (intersection file-blackboards (blackboards-of-system *bb1-system-name*))
	    (bb1error 'CHECK-KB-COMPATIBILITY
		      "The blackboards "
		      (intersection file-blackboards (blackboards-of-system *bb1-system-name*))
		      " exist in both systems.  Unable to integrate them.")
	  (catch 'check-link-exit
	    (progn (mapc #'(lambda (link)
			     (if (member link file-links)
				 (if (not (eql (opposite-link-of-link link)
					       (bb-shortlinkinforec-opplink
						 (gethash link (eval file-link-name)))))
				     (progn (bb1error 'check-kb-compatibility
						      link " exists in both systems and has different"
						      " opposite links in each.  "
						      "Unable to integrate them.")
					    (throw 'check-link-exit nil)))))
			 (all-links-of-system *bb1-system-name*))
		   (throw 'check-link-exit t)
		   )
	    )
	  )
;--->	)
      )
    )




;;-------  CLEAR-HASH-ARRAYS  -------

(defun clear-hash-arrays (&optional (confirm nil))
  "Clears out the BB1 hash arrays (or, if they don't exist, makes new ones)."

  (if (current-system-name)      ;;; Is there a previous system?

      ;;NEED TO CLEAR OUT OLD ARRAYS

      (if (or (not confirm)
	      (bb1-y-or-n-p (format nil "Delete the current system (~A)?" (current-system-name))))
	  (progn (mapc #'(lambda (array-name) (clrhash (eval array-name)))
		       *bb-data-array-names*)
		 t)
	nil)

    ;;ELSE MAKE NEW HASH ARRAYS

    (progn (set-up-bb-data-arrays)
	   t)
    )
  )



;;-------  CURRENT-SYSTEM-NAME  ----

(defun current-system-name (&optional (file nil))
  "Searches *bb-systeminfo* for the last system added."

  (let ((current-system  nil)
	(hash-table-name (if file
			     (pack* *bb-file-prefix* '*bb-systeminfo*)
			   '*bb-systeminfo*)))

    (if (and (boundp hash-table-name)
	     (hash-table-p (eval hash-table-name)))
	(maphash #'(lambda (key ignore)
		     (setq current-system key))
		 (eval hash-table-name))
      )
    current-system)
  )


(defun current-knowledgebase-name (&optional (file nil))
  "Searches *bb-knowledgebaseinfo* for the last knowledgebase added."

  (let ((current-kb  nil)
	(hash-table-name (if file
			     (pack* *bb-file-prefix* '*bb-knowledgebaseinfo*)
			   '*bb-knowledgebaseinfo*)))

    (if (and (boundp hash-table-name)
	     (hash-table-p (eval hash-table-name)))
	(maphash #'(lambda (key ignore)
		     (setq current-kb key))
		 (eval hash-table-name))
      )
    current-kb)
  )



;;;-------  FIND-SEARCH-SPACE  -------

(defun find-search-space (a-v-list)
  "Used by $FIND to obtain a list of blackboard objects
to look at which might match a-v-list."

  ;; Optimized  11/06/86 -mh

  ;;This function is only called if the user did not specify a level at which
  ;;$FIND should be looking.  Needless to say, this kind of blind search is
  ;;inefficient.

  ;;Strategy is to first get all objects from all blackboards, then prune them
  ;;according to whether or not they have the appropriate attributes and/or links.

  ;;Need to remove those attributes and links which have inheritance
  ;;because they might be inherited (obviously).

  (let  ((attrs-and-links (mapcar #'car a-v-list))
	 (all-objects     ;;Collect all objects of system the fast way: not (objects-of-system *bb1-system-name*))
	                  (maphash #'(lambda (key val) (declare (ignore val)) key)
				   *bb-objectinfo*))
	 (attrs           nil)
	 (links           nil)
	 (attr-info       nil)
	 )

    ;;Collect attributes and values

    (dolist (attr-or-link attrs-and-links) 
      (cond ((setq attr-info (get-bb-shortattribute-data (car attr-or-link)))
	          (if (not (bb-shortattributeinforec-inheritance-opportunities attr-info))
		      (push (car attr-or-link) attrs)))
	    (t  ;Must be a link
  	          (if (not (bb-shortlinkinforec-inheritance-opportunities
			     (get-bb-shortlink-data (car attr-or-link))))
		      (push (car attr-or-link) links)
		      ))
	    )
      )


    ;;RETURN all objects who have all of ATTRS and LINKS

    (remove-if-not
      #'(lambda (obj)
		(and (subsetp attrs
			      (mapcar #'car (attributes-of-object obj)))
		     (subsetp links
			      (mapcar #'car (links-of-object obj))))
		     )
      all-objects)
    )
  )


;(defun find-search-space (a-v-list)
;  "Used by $FIND to obtain a list of blackboard objects
;to look at which might match a-v-list."

;  ;;This function is only called if the user did not specify a level at which
;  ;;$FIND should be looking.  Needless to say, this kind of blind search is
;  ;;inefficient.

;  ;;Strategy is to first get all objects from all blackboards, then prune them
;  ;;according to whether or not they have the appropriate attributes and/or links.

;  (let* ((attrs-and-links (mapcar #'car a-v-list))
;	 (attrs           (remove-if #'(lambda (var)
;					 (or (valid-link-for-system? var *bb1-system-name*)
;					     (inheritance-opportunity-of-attribute var)))
;				     attrs-and-links))
;	 (links           (remove-if-not #'(lambda (var)
;					     (and (valid-link-for-system? var *bb1-system-name*)
;						  (not (inheritance-opportunity-of-link var))))
;					 attrs-and-links))
;	 (all-objects     (objects-of-system *bb1-system-name*))
;	 )

;    ;;RETURN all objects who have all of ATTRS and LINKS

;    (remove-if-not
;      #'(lambda (obj)
;		(and (subsetp attrs
;			      (mapcar #'car (attributes-of-object obj)))
;		     (subsetp links
;			      (mapcar #'car (links-of-object obj))))
;		     )
;      all-objects)
;    )
;  )



;;-------  GENERATE-OBJECT-NAME  -------

(defun generate-object-name (level)

;; Given the appropriate level, constructs a name by concatenating the
;; level name with the number of objects which will be on the blackboard.

  (if (valid-level? level)
      (user-pack* (short-name-of-level level)
		  (write-to-string (1+ (length (objects-of-level level)))))
    ;;ELSE
      (bb1error 'GENERATE-OBJECT-NAME
		level " is not an existing level."))
  )



;;-------  GET-SYSTEM-NAME-FROM-USER  -------

;;;Edited by Mike Hewett   26 Apr 87  23:39
(defun get-system-name-from-user ()
  "Prompts the user for a system name."

  (let ((new-sys-name (bb1-input-atom nil "System name" "What system are you working on?"))
	)

    (when new-sys-name
      (setf *BB1-SYSTEM-NAME* new-sys-name)
      )
    new-sys-name
    )
  )



;;-------  LOAD-SAVED-STATE  -------

(defun load-saved-state ()
  "Loads a file containing a saved BB1 state, so the user can continue from
that state."

  (when (null *BB1-SYSTEM-NAME*)
    (get-system-name-from-user)
    )

  (setq *BB1-CURRENT-FILE-NAME* (format nil "~A-STATE-SAVE" *BB1-SYSTEM-NAME*))
  (setq *BB1-CURRENT-FILE-TYPE* "LISP")
  (display-bb1-message "Load state from what file?")
  (bb1-set-filename-defaults)
  (display-bb1-message "Loading saved state")
  
  (if (bb1-load-file)
      (prog1 t
	     (display-bb1-message "Saved state loaded.")
	     )
        ;;ELSE
      (display-bb1-message "Sorry, can't find that file.")
      nil
      )
  )



;;-------  MAKE-EXTENDED-NAME  -------

;;;Edited by Mike Hewett   24 Apr 87  0:47
(defun make-extended-name (&rest name-elements)
  "Concatenates name-elements into one long atom, interspersed with '.' characters."

  ;; Now calls MAKE-PACKED, a utility function

  (apply #'make-packed "." name-elements)
  )



;;-------  MAKE-TEMP-SYSTEM  -------

;;;Edited by Mike Hewett   7 Apr 87  12:47
;;;Edited by Mike Hewett   24 Apr 87  0:47
(defun make-temp-system (blackboards attributes-to-save links-to-save
			 &OPTIONAL (kb-name nil) (system (current-system-name)))
  "Makes a system in the *FILE- arrays consisting only of those blackboards specified.
Copies the data from the *BB- arrays into the *FILE- arrays.

If ATTRIBUTES-TO-SAVE is ALL, then all attributes (possible and actual) will be saved.

If ATTRIBUTES-TO-SAVE is USED, then only those attributes actually on objects
will be saved.  Same for LINKS-TO-SAVE.

If ATTRIBUTES-TO-SAVE is USED+SOME, then those attributes actually on objects
will be saved, plus any others the user selects.


INDEX: :topic (bb1-internal)  :keyword (save file hash-table)
"

  (let ((info       nil)
	)

    (set-up-bb-data-arrays 'temp)

      ;;Copy system information

    (setq info (copy-bb-systeminforec (get-bb-system-data system)))
    (setf (bb-systeminforec-domain-blackboards info) (intersection (bb-systeminforec-domain-blackboards info)
								   blackboards))
    (setf (bb-systeminforec-control-blackboards info) (intersection (bb-systeminforec-control-blackboards info)
								    blackboards))
    (setf (bb-systeminforec-knowledgebases info) `(,kb-name))
    
    (setf (gethash system (eval (pack* *bb-file-prefix* '*bb-systeminfo*))) info)
    
    
      ;;Copy knowledgebase information

    (when kb-name
      (setf (gethash kb-name (eval (pack* *bb-file-prefix* '*bb-knowledgebaseinfo*)))
	    (make-bb-knowledgebaseinforec :system      system
					  :blackboards blackboards))
      )
    
      ;;Copy blackboard, level, object, attribute, and link information
    
      ;;BLACKBOARD
    (dolist (bb blackboards)                 ;;Need to copy these structures because we might change them
      (setf (gethash bb (eval (pack* *bb-file-prefix* '*bb-blackboardinfo*)))
	    (copy-bb-blackboardinforec (get-bb-blackboard-data bb)))
      
        ;;LEVEL
      (dolist (level (levels-of-blackboard bb))
	(setf (gethash level (eval (pack* *bb-file-prefix* '*bb-levelinfo*)))
	      (get-bb-level-data level))
	
	;;OBJECT - Need to copy this because we may change it.
	(dolist (object (objects-of-level level))
	  (setf (gethash object (eval (pack* *bb-file-prefix* '*bb-objectinfo*)))
		(copy-bb-objectinforec (get-bb-object-data object)))
	  
	   ;;ATTRIBUTE
	  (dolist (attr (actual-attributes-of-object object))
	    (setf (gethash attr (eval (pack* *bb-file-prefix* '*bb-attributeinfo*)))
		  (get-bb-attribute-data attr))
	    )
	  
	   ;;LINK - Need to copy this because we may change it.
	  (dolist (link (actual-links-of-object object))
	    (setf (gethash link (eval (pack* *bb-file-prefix* '*bb-linkinfo*)))
		  (copy-bb-linkinforec (get-bb-link-data link)))
	    )
	  )
	)
      )
    
    ;; Save entire DATA array
    
    (set (pack* *bb-file-prefix* '*bb-datainfo*) *bb-datainfo*)  ;;Okay to set, because we won't alter it.
    
    ;; Copy the SHORTLINK array
    
    (set (pack* *bb-file-prefix* '*bb-shortlinkinfo*) (copy-hash-table *bb-shortlinkinfo*))
    
    ;; Copy the SHORTATTRIBUTE array
    
    (set (pack* *bb-file-prefix* '*bb-shortattributeinfo*) (copy-hash-table *bb-shortattributeinfo*))
    
    
    ;; They may not want to save links to objects not saved.  Ask them

    (when (bb1-y-or-n-p "There may be links from objects being saved to objects not being saved.
Should I delete those links?")

      (psetq *bb-objectinfo*         *file-*bb-objectinfo*
	     *file-*bb-objectinfo*   *bb-objectinfo*

	     *bb-linkinfo*            *file-*bb-linkinfo*
	     *file-*bb-linkinfo*      *bb-linkinfo*
	     )

      (bb1-fix-up-links-for-save)

      (psetq *bb-objectinfo*         *file-*bb-objectinfo*
	     *file-*bb-objectinfo*   *bb-objectinfo*

	     *bb-linkinfo*            *file-*bb-linkinfo*
	     *file-*bb-linkinfo*      *bb-linkinfo*
	     )
      )



    ;; Delete the unwanted attributes
    
    (unless (eql attributes-to-save 'ALL)    ; have to only copy specified attributes
      (let ((harray (eval (pack* *bb-file-prefix* '*bb-shortattributeinfo*)))
	    (saved-attributes   (let ((attrs nil))
				  (maphash #'(lambda (key val)
					       (declare (ignore val))
					       (push (short-name-of-attribute key) attrs)
					       )
					   (eval (pack* *bb-file-prefix* '*bb-attributeinfo*))
					   )
				  attrs)
				)
	    (other-attributes   nil)
	    )
	
	
	(when (eql attributes-to-save 'USED+SOME)
	  (setq other-attributes (set-difference (all-attributes-of-system system)
						 saved-attributes))
	  (when other-attributes
	    (display-bbedit-message "Select other attributes to save")
	    (setq saved-attributes (nconc (bb1-select-some-of "Pick attributes to save"
							      (sort-simple-list other-attributes))
					  saved-attributes))
	    )
	  )
	
	(maphash #'(lambda (key val)
		     (declare (ignore val))
		     (unless (member key saved-attributes)
		       (remhash key harray)
		       )
		     )
		 harray)
	)
      )
    
    
      ;; Delete the unwanted LINKS 
   
    (unless (eql links-to-save 'ALL)    ; have to only copy specified links
      (let ((harray  (eval (pack* *bb-file-prefix* '*bb-shortlinkinfo*)))
	    (saved-links   (let ((links nil))
			     (maphash #'(lambda (key val)
					  (declare (ignore val))
					  (push (short-name-of-link key) links)
					  )
				      (eval (pack* *bb-file-prefix* '*bb-linkinfo*))
				      )
			     links)
			   )
	  (other-links       nil)
	  )
	
	
	(when (eql links-to-save 'USED+SOME)
	  (setq other-links (set-difference (all-links-of-system system)
					    saved-links))
	  (when other-links
	    (display-bbedit-message "Select other links to save")
	    (setq saved-links (nconc (bb1-select-some-of "Pick links to save" (sort-simple-list other-links))
				     saved-links))
	    )
	  )
	
	(maphash #'(lambda (key val)
		     (declare (ignore val))
		     (unless (member key saved-links)
		       (remhash key harray)
		       )
		     )
		 harray)
	)
      )
    
    
    ;;RENAME the KB
    
    (when kb-name
      (psetq *file-*bb-blackboardinfo* *bb-blackboardinfo*
	     *bb-blackboardinfo*       *file-*bb-blackboardinfo*)
      
      (dolist (bb blackboards)           ;Rename kb to the given name
	(setf (bb-blackboardinforec-knowledgebase (get-bb-blackboard-data bb)) kb-name)
	)
      
      (psetq *bb-blackboardinfo*        *file-*bb-blackboardinfo*
	     *file-*bb-blackboardinfo*  *bb-blackboardinfo*)
      )
    )
  )



;;-------  MODIFY-VALUE-OF-ATTRIBUTE  -------

;;;Edited by Mike Hewett   24 Apr 87  0:47
(defun modify-value-of-attribute (attribute new-value)

;;; If the attribute has a procedural attachment, remove it.  This function
;;; is only used when storing an actual value on an attribute (as opposed
;;; to the value of a procedural attachment invocation).

  (if (valid-attribute? attribute)
      (progn (if (procedural-attachment-of-attribute attribute)
		 (delete-procedural-attachment-of-attribute attribute))
	     ;; Might be adding a procedural attachment
	     (if (and (listp new-value)
		      (eql (car new-value) '$PROCEDURE))
		 (add-to-attribute-array attribute nil nil nil (cadr new-value) 'update)
	         ;;ELSE - place value on attribute
	       (add-to-attribute-array attribute nil nil new-value nil 'update))
	     )
    ;;ELSE
    (bb1error 'MODIFY-VALUE-OF-ATTRIBUTE
	      attribute " is not an existing attribute."))
  )




;;-------  RESTORE-INITIAL-STATE  -------

;;;Edited by Mike Hewett   26 Apr 87  21:02
;;;Edited by Mike Hewett   26 Apr 87  21:10
;;;Edited by Mike Hewett                21 May 87  13:41
(defun restore-initial-state (&OPTIONAL (restore-method *BB1-RESTART-RUN-BEHAVIOR*))
  "RESTORE-METHOD can be :NORMAL, (:CLEAR ...) or NIL.

:NORMAL  - Copies the SAVE hash arrays to the BB hash arrays.

:CLEAR    - Clears the blackboards or levels given by the arguments.
              Args can be a symbol (assumed to be a blackboard name) or
              a list (:BLACKBOARD bb-name ...) or (:LEVEL level-name ...)

NIL        - Does nothing."


  (let ((command    restore-method)
	(args       (when (consp restore-method)
		      (cdr restore-method)))
	)

    (when (consp restore-method)
      (setq command (car restore-method)))

    (ccase command
      (:NORMAL     (bb1message T "Restoring initial blackboard states.")
		   (dolist (arr *bb-data-array-names*)
		     (set arr (copy-hash-table (eval (pack* *bb-save-prefix* arr))))
		     ))
      (:CLEAR      (dolist (arg args)
		     (cond ((atom arg)   (bb1message T "Clearing the " arg " blackboard.")
					 (delete-all-objects-of-blackboard arg))
			   ((consp arg)  (ccase (car arg)
					   (:BLACKBOARD  (dolist (bb (cdr arg))
							   (bb1message T "Clearing the " bb " blackboard.")
							   (delete-all-objects-of-blackboard bb)))
					   (:LEVEL       (dolist (lev (cdr arg))
							   (bb1message T "Clearing the " lev " level.")
							   (delete-all-objects-of-level lev)))
					   ))
			   (t            (bb1message T "Unknown command: " arg " in *BB1-RESTART-RUN-BEHAVIOR*")
					 ))
		     ))
		     
      (nil         (bb1message T "Final program state is preserved for next run."))
      )
    )
  )



;;-------  SAVE-INITIAL-STATE  -------

;;;Edited by Mike Hewett   7 Apr 87  12:47
;;;Edited by Mike Hewett   26 Apr 87  21:02
;;;Edited by Mike Hewett   26 Apr 87  21:10
(defun save-initial-state ()
  "Copies the BB hash arrays to the SAVE hash arrays"

  (bb1message T "Saving current blackboard states.")
  (dolist (arr *bb-data-array-names*)
    (set (pack* *bb-save-prefix* arr) (copy-hash-table (eval arr)))
    )
  )



;;-------  SAVE-BB1-STATE  -------

(defun save-bb1-state ()
  "Saves the entire state of the BB1 system (presumably during the middle of a run)
so that the system can be restarted from that state at a later time."

  (display-bb1-message "Save state to what file?")
  (setq *BB1-CURRENT-FILE-NAME* (concatenate 'string (string *BB1-SYSTEM-NAME*)
					             "-STATE-SAVE"))
  (setq *BB1-CURRENT-FILE-TYPE* "LISP")
  (bb1-set-filename-defaults)
  (if (string-equal *BB1-CURRENT-FILE-NAME* "nil")
      (display-bb1-message "Nothing saved.")

      ;;ELSE

    (let ((fname (bb1-find-output-file))
	  )
      (if fname

	  (with-open-file (save-f fname  :direction :OUTPUT)
	    
	     ;;Write a header
	    (format save-f
		    ";;;  -*-  Mode:LISP; Syntax:COMMON-LISP; Base:10; Package:USER  -*-~%")
	    
	    (format save-f ";;;;   BB1 SAVE STATE:   ~A system saved on ~A~%~%~%"
		    *BB1-SYSTEM-NAME*
		    (get-time-string))
	    
	    
	      ;;First, save variables
	    
	    (dolist (v (nconc (mapcar #'car *USER-ACCESSIBLE-VARIABLES*)
			      *STATE-VARIABLES-TO-SAVE*))
	      (cond ((not (boundp v))          (format save-f "(setf bb1::~A nil)~%" v))
		    ((hash-table-p (eval v))   (dump-hash-table-to-stream (format nil "BB1::~A" v)
									  (eval v) save-f))
		    (t                         (format save-f "(setf bb1::~A (quote " v)
					       (write (eval v) :stream save-f :escape t :pretty t)
					       (format save-f "))~%" ))
		    )
	      )
	    
	      ;;Next, save the hash arrays
	    
	    (dolist (harray *BB-DATA-ARRAY-NAMES*)
	      (dump-hash-table-to-stream (format nil "BB1::~A" harray) (eval harray) save-f)
	      )

	    (display-bb1-message "Save completed.")
	    )
	  ;;ELSE - no such file
	  (display-bb1-message "Sorry, can't open that file.")
	  )
      )
    )
  )



;;-------  SAVE-SYSTEM-TO-FILE  -------

;;;Edited by Mike Hewett   24 Apr 87  0:47
(defun save-system-to-file (system-name file-name &optional (kb-name nil) (use-temp nil))
  "File should be okay to open and write to.

INDEX:  :topic (bb1-internal)   :keyword  (save file)
"

  (if (current-system-name)

      (with-open-file (file-stream file-name :direction :output)
	
	
	(format file-stream
		";;;  -*-  Mode:LISP; Syntax:COMMON-LISP; Base:10; Package:USER  -*-~%")
	
	(format file-stream ";;;;   BB1 SYSTEM SAVE:   ~A system saved on ~A~%~%~%"
		system-name
		(get-time-string))
	(format file-stream "(setf BB1::*BB1-SYSTEM-NAME* (quote ~A))~%~%~%" system-name)
	(format file-stream "(setf BB1::*BBEDIT-CURRENT-KB* (quote ~A))~%~%~%" kb-name)
	
	(mapc #'(lambda (array-name)
		  (dump-hash-table-to-stream (concatenate 'string "BB1::"
							  (string *bb-file-prefix*)
							  (string array-name))
					     (cond (use-temp  (eval (pack* *bb-file-prefix*
									   array-name)))
						   (t         (eval array-name)))
					     file-stream))
	      *BB-DATA-ARRAY-NAMES*)
	)
      )
  
  )




;;;-------  SAVE-KSES-TO-FILE  -------

;;;Edited by Mike Hewett   24 Apr 87  0:47
(defun save-kses-to-file (bb-list)
  "Saves the given BBs to a file.  Called by KSEDIT to
save KSes."

  (let* ((system (or *BB1-SYSTEM-NAME* (current-system-name)))
	 (file-name     (format nil "~A-KS-KB" system))
	 (file-pointer  nil)
	)

    (setf *BB1-CURRENT-FILE-TYPE* "LISP")
    (setf *BB1-CURRENT-FILE-NAME* file-name)
    (bb1-set-filename-defaults)

    (if (setq file-pointer (bb1-find-output-file))

	(progn (display-ksedit-message "Saving to " file-pointer)
	       (make-temp-system bb-list 'USED 'USED)
	       (save-system-to-file system file-pointer nil 'temp)
	       (display-ksedit-message "Save complete."))
	;;;else
	(display-ksedit-message "Unable to open the file.  System not saved.")
	)
    )
  )







;;-------  SEARCHFOR  -------

(defun searchfor (objects avlst)

;; Optimized  11/06/86   -mh     could use some more optimization
;; Fixed bug  11/11/86   -mh     Oops!

;; Returns the list of OBJECTS which have the
;; attributes/links whose values match the ones in avlst.
;;  The avlst is already evaluated.
;;
;; MH: 02/10/86

  (mapcan   #'(lambda (obj)
		(if (every #'(lambda (av)
			       (cond ((or (inheritance-opportunity-of-attribute (car av))
					  (valid-attribute-for-object? (car av) obj))
				      
				          (equal ($Value obj (car av) 'suppress-error)
						 (cadr av)))
				     ((or (inheritance-opportunity-of-link (car av))
					  (valid-link-for-object? (car av) obj))
				      
				          (member (cadr av) ($objects obj (car av))))
				     (t   nil)
				     )
			       )
			   avlst)
		    (list obj))
		)
	    objects)
  )




;(defun searchfor (objects avlst)

;;; Returns the list of OBJECTS which have the
;;; attributes/links whose values match the ones in avlst.
;;;  The avlst is already evaluated.
;;;
;;; MH: 02/10/86

;  (remove nil
;	  (mapcar #'(lambda (obj)
;		      (if (member nil (mapcar #'(lambda (av)
;						  (if (or (valid-attribute-for-object? (car av) obj)
;							  (inheritance-opportunity-of-attribute (car av)))
;						      (equal ($value obj (car av) 'suppress-error)
;							     (cadr av))
;						        ;;ELSEIF
;						      (if (or (valid-link-for-object? (car av) obj)
;							      (inheritance-opportunity-of-link (car av)))
;							  (member (cadr av)
;								  ($objects obj (car av)))
;							   ;;ELSE
;							  nil))
;						  )
;					      avlst)
;				  )
;                          nil
;		        ;;ELSE
;                          obj))
;		  objects))
;  )



;;;------- LONG-NAME -------

(defun long-name (name &OPTIONAL (special-type nil))
  "Given the short name of an object,  level, attribute,
or link, returns the long name.  

If SPECIAL-TYPE? is one of {ACTION EVENT STATE}
uses the *BB1-LANGUAGE-BLACKBOARDS* variable to
look for the object on one of the language blackboards.

Else finds the first object, level, attribute, or link whose
short name is OBJ."


  ;;This is used often in the language code

  (if (atom name)

  (let ((search-name (user-pack* name))
	(search-type (user-pack* special-type))
	(search-list nil)
	(search-val  nil)
	(found-thing nil)
	)
	
    (if (member search-type '(USER::ACTION USER::EVENT USER::STATE)) 
	
	
        ;; Search for a language object with this name
	
	(dolist (bb *BB1-LANGUAGE-BLACKBOARDS*)
	  (setq search-val (make-extended-name bb search-type search-name))
	  (if (valid-object? search-val)
	      (return search-val)
	      )
	  )
	
	
        ;; ELSE - look for an object, level, attribute, or link
	
	
	(or    
	  
	  ;OBJECT
	  
	  (dolist (bb (blackboards-of-system *BB1-SYSTEM-NAME*) nil)   ;;Objects
	    (setq search-list (mapcar #'(lambda (o) (cons (short-name-of-object o) o))
				      (objects-of-blackboard bb)))
	    (if (setq found-thing
		      (cdr (assoc search-name search-list)))
		(return found-thing))
	    )
	  
	  ;LEVEL
	  
	  (dolist (bb (blackboards-of-system *BB1-SYSTEM-NAME*) nil)   ;;Levels
	    (setq search-list (mapcar #'(lambda (l) (cons (short-name-of-level l) l))
				      (levels-of-blackboard bb)))
	    (if (setq found-thing
		      (cdr (assoc search-name search-list)))
		(return found-thing))
	    )
	  
	  ;ATTRIBUTE
	  
	  (dolist (bb (blackboards-of-system *BB1-SYSTEM-NAME*) nil)   ;;Attributes
	    (if (setq search-val
		      (dolist (obj (objects-of-blackboard bb) nil)
			(setq search-list (mapcar #'(lambda (a) (cons (short-name-of-attribute a) a))
						  (actual-attributes-of-object obj)))
			(if (setq found-thing
				  (cdr (assoc search-name search-list)))
			    (return found-thing))
			))
		(return search-val)
		)
	    )
	  
	  
	  ;LINK
	  
	  (dolist (bb (blackboards-of-system *BB1-SYSTEM-NAME*) nil)   ;;Links
	    (if (setq search-val
		      (dolist (obj (objects-of-blackboard bb) nil)
			(setq search-list (mapcar #'(lambda (l) (cons (short-name-of-link l) l))
						  (actual-links-of-object obj)))
			(if (setq found-thing
				  (cdr (assoc search-name search-list)))
			    (return found-thing))
			))
		(return search-val)
		)
	    )
	  )
	)
    )
  )
  )




;;;------- SHORT-NAME  ------

(defun short-name (obj)
  "Generic version of SHORT-NAME-OF-x.  Doesn't cause an error."

  (cond ((valid-object? obj)    (short-name-of-object obj))
	((valid-level? obj)     (short-name-of-level obj))
	((valid-attribute? obj) (short-name-of-attribute obj))
	((valid-link? obj)      (short-name-of-link obj))
	(t                      obj)
	)
  )


;;;-------  SHORT-NAMES  -------

(defun short-names (objs)
  "MAPCAR of SHORT-NAME over OBJS."

  (mapcar #'short-name objs))




;;;-------  STORE-ATTRIBUTE  -------

(defun store-attribute (object attr new-value)

;;; Optimized  11/06/86   -mh

;;; If the attribute has a procedural attachment, remove it.  This function
;;; is only used when storing an actual value on an attribute (as opposed
;;; to the value of a procedural attachment invocation).

  (let* ((attribute (cdr (assoc attr (attributes-of-object object))))
	 (attr-info (get-bb-attribute-data attribute))
	 proc
	 )

    (if attribute
	(cond ((setq proc (bb-attributeinforec-procedural-attachment attr-info))
	          (delete-procedural-attachment-of-attribute attribute)
	          (add-to-attribute-array attribute nil nil new-value nil 'update))

	      ((and (consp new-value)
		    (member (car new-value) '(USER::$PROCEDURE bb1::$PROCEDURE)))
   		  (add-to-attribute-array attribute nil nil nil (cadr new-value) 'update))

	      (t  (add-to-attribute-array attribute nil nil new-value nil 'update))
	      )
	;;ELSE
	(bb1error 'STORE-ATTRIBUTE
		  attribute " is not an existing attribute."))
    
    )
  )
