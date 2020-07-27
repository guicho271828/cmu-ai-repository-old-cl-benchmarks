;;; -*- MODE: LISP; READTABLE:COMMON-LISP; PACKAGE:BB1; BASE:10  -*-

;;;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;                                                                                                                   +
;;;                                  BB1 Version 1.2                                                                  +
;;;                                                                                                                    +
;;;  Copyright (c) 1986,1987  Stanford University, Department of Computer Science, Knowledge Systems Laboratory  +
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
;;;  BB1RECORD.LISP      -     Record definitions for the BB1 system
;;;
;;;  Mike Hewett     02/04/86
;;;                    03/26/86  ADDED RECORDS FOR BB-KNOWLEDGEBASEINFO DATABASES    -MH
;;;                    04/08/86  ADDED Knowledge-Source record                              -mh
;;;                    09/22/86  New attribute and link records (corresponding to new hash arrays).
;;;                                Also modified some of the old records because of hash array changes.
;;;                    10/28/86  Added WORD structure from PMTEdit         
;;;                    11/23/86  The file BB1RECORD-FNS now contains :print-functions for some of these structures.
;;;                                Any changes to structures must also be reflected there.                                 -mh

;;;  This file contains record declarations (actually Common Lisp structure definitions)
;;;  for the entire BB1 system.  There should be *no* other record declarations in the
;;;  system.  BB1RECORD-FNS contains print-functions for these records.

;;;  Contents:  bb-attributeinforec
;;;             bb-blackboardinforec
;;;             bb-datainforec
;;;             bb-knowledgebaseinforec
;;;             bb-levelinforec
;;;             bb-linkinforec
;;;             bb-objectinforec
;;;             bb-shortattributeinforec
;;;             bb-shortlinkinforec
;;;             bb-systeminforec
;;;
;;;             bb-inheritancerec
;;;             bb-parsed-inheritancerec
;;;
;;;             bb-rulerec
;;;
;;;             knowledge-source
;;;
;;;             trigger-condition-treerec
;;;
;;;             word


;;;----- Package information ------

(in-package 'BB1)

;;;----------------------------




;;; ----------    bb-attributeinforec    -------------

(defstruct (bb-attributeinforec (:print-function bb-attributeinforec-print-function))

           procedural-attachment
	   values
	   object
	   short-name
	   )

;;; ----------    bb-blackboardinforec   -------------

(defstruct (bb-blackboardinforec (:print-function bb-blackboardinforec-print-function))

           levels
	   system
	   knowledgebase
	   type
	   )

;;; ----------    bb-datainforec         -------------

(defstruct (bb-datainforec (:print-function bb-datainforec-print-function))

           value
	   )

;;; ----------    bb-knowledgebaseinforec        -------------

(defstruct (bb-knowledgebaseinforec (:print-function bb-knowledgebaseinforec-print-function))

           blackboards
	   system
	   )

;;; ----------    bb-levelinforec        -------------

(defstruct (bb-levelinforec (:print-function bb-levelinforec-print-function))

           objects
	   blackboard
	   short-name
	   )

;;; ----------    bb-linkinforec         -------------

(defstruct (bb-linkinforec (:print-function bb-linkinforec-print-function))

           linked-objects
	   short-name
	   )

;;; ----------    bb-objectinforec       -------------

(defstruct (bb-objectinforec (:print-function bb-objectinforec-print-function))

           attributes
	   links
	   $links
	   level
	   short-name
	   blackboard
	   )

;;; ----------    bb-systeminforec       -------------

(defstruct (bb-systeminforec (:print-function bb-systeminforec-print-function))

	   domain-blackboards
	   control-blackboards
	   knowledgebases
	   data-elements
	   )

;;; ----------    bb-inheritancerec      -------------

(defstruct (bb-inheritancerec (:type list))

           vialink
	   cachable?
	   )


;;; ----------    bb-parsed-inheritancerec   --------

(defstruct bb-parsed-inheritancerec

	   attribute-or-link
  	   name
	   inheritance-record)


;;; ----------    bb-rulerec   --------

(defstruct (bb-rulerec (:type list))

           label
	   lhs
	   actions)


;;; ----------    bb-shortattributeinforec   ----------

(defstruct (bb-shortattributeinforec (:print-function bb-shortattributeinforec-print-function))
    
           inheritance-opportunities
	   )



;;; ----------    bb-shortlinkinforec   ----------

(defstruct (bb-shortlinkinforec (:print-function bb-shortlinkinforec-print-function))
    
           opplink
	   inheritance-opportunities
	   )



;;; -------  knowledge-source  -------

(defstruct (knowledge-source (:print-function knowledge-source-print-function))

           trigger-conditions
	   context
	   (preconditions '(t))
	   obviation-conditions
	   action-variables
	   actions
	   from-blackboard
	   to-blackboard
	   from-level
	   to-level
	   name
	   (date-created (get-time-string)) 
	   (description  "Knowledge source with no description.")
	   author
	   (cost 0)
	   (reliability 100)
	   )


;;;-------  trigger-condition-treerec  -------

(defstruct (trigger-condition-treerec  (:print-function trigger-tree-print-function))

           sortkey
	   trigcondlist
	   potentialkses
	   )



;;;-------  word  ------- Used in PMT-EDIT

(defstruct (word (:print-function word-print-function))

           print-name
	   position
	   anchor
	   type
	   )

