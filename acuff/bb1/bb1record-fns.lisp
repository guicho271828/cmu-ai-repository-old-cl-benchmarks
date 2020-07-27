;;; -*- MODE:COMMON-LISP; READTABLE:COMMON-LISP; PACKAGE:BB1; BASE:10  -*-

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
;;;     BB1RECORD-FNS.LISP       -      Record-related functions for BB1
;;;
;;;     Mike Hewett    11/23/86
;;;
;;; - - - - - - - - - - - - -
;;;
;;;  This file was started to hold the :PRINT-FUNCTIONS for the various records in the BB1 system.
;;;  These are needed when hash arrays are dumped.  The Common LISP standard guarantees that
;;;  structures will be printed in a readable fashion when *PRINT-PRETTY* is T, but TI seems to
;;;  ignore that lately.  Also, having a print-function should be much faster than using the generic
;;;  print-named-object which TI LISP uses.
;;;
;;;
;;;
;;;
;;;
;;;     Changes:  11/23/86   New file                                                            -mh
;;;                03/19/87   Added TRIGGER-TREE-PRINT-FUNCTION                             -mh
;;;
;;;


;;;----- Package information ------

(in-package 'BB1)

;;;----------------------------


;;;----- Contents
;;;
;;;   BB-ATTRIBUTEINFOREC-PRINT-FUNCTION
;;;   BB-BLACKBOARDINFOREC-PRINT-FUNCTION
;;;   BB-DATAINFOREC-PRINT-FUNCTION
;;;   BB-KNOWLEDGEBASEINFOREC-PRINT-FUNCTION
;;;   BB-LEVELINFOREC-PRINT-FUNCTION
;;;   BB-LINKINFOREC-PRINT-FUNCTION
;;;   BB-OBJECTINFOREC-PRINT-FUNCTION
;;;   BB-SHORTATTRIBUTEINFOREC-PRINT-FUNCTION
;;;   BB-SHORTLINKINFOREC-PRINT-FUNCTION
;;;   BB-SYSTEMINFOREC-PRINT-FUNCTION
;;;   KNOWLEDGE-SOURCE-PRINT-FUNCTION
;;;   WORD-PRINT-FUNCTION
;;;   TRIGGER-TREE-PRINT-FUNCTION


;;;-------  BB-ATTRIBUTEINFOREC-PRINT-FUNCTION

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-attributeinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-ATTRIBUTEINFOREC."

  (declare (ignore print-depth))

  ;; Has fields PROCEDURAL-ATTACHMENT VALUES OBJECT SHORT-NAME

  (format stream " #S(BB1::BB-ATTRIBUTEINFOREC~%")

  (format stream "  :PROCEDURAL-ATTACHMENT ~S~%" (bb-attributeinforec-procedural-attachment instance))
  (format stream "  :VALUES ~S~%"                (bb-attributeinforec-values                instance))
  (format stream "  :OBJECT ~S~%"                (bb-attributeinforec-object                instance))
  (format stream "  :SHORT-NAME ~S~%"            (bb-attributeinforec-short-name            instance))
  (format stream "  )")
  )



;;;-------  BB-BLACKBOARDINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-blackboardinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-BLACKBOARDINFOREC."

  (declare (ignore print-depth))

  ;; Has fields LEVELS SYSTEM KNOWLEDGEBASE TYPE

  (format stream " #S(BB1::BB-BLACKBOARDINFOREC~%")

  (format stream "  :LEVELS ~S~%"          (bb-blackboardinforec-levels         instance))
  (format stream "  :SYSTEM ~S~%"          (bb-blackboardinforec-system         instance))
  (format stream "  :KNOWLEDGEBASE ~S~%"   (bb-blackboardinforec-knowledgebase  instance))
  (format stream "  :TYPE ~S~%"            (bb-blackboardinforec-type           instance))
  (format stream "  )")
  )




;;;-------  BB-DATAINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-datainforec-print-function (instance stream print-depth)
  "Prints an instance of BB-DATAINFOREC."

  (declare (ignore print-depth))

  ;; Has fields VALUE

  (format stream " #S(BB1::BB-DATAINFOREC~% :VALUE ~S)" (bb-datainforec-value instance))
  )




;;;-------  BB-KNOWLEDGEBASEINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-knowledgebaseinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-KNOWLEDGEBASEINFOREC."

  (declare (ignore print-depth))

  ;; Has fields BLACKBOARDS SYSTEM

  (format stream " #S(BB1::BB-KNOWLEDGEBASEINFOREC~%")

  (format stream "  :BLACKBOARDS ~S~%" (bb-knowledgebaseinforec-blackboards instance))
  (format stream "  :SYSTEM ~S~%"      (bb-knowledgebaseinforec-system      instance))
  (format stream "  )")
  )




;;;-------  BB-LEVELINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-levelinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-LEVELINFOREC."

  (declare (ignore print-depth))

  ;; Has fields OBJECTS BLACKBOARD SHORT-NAME

  (format stream " #S(BB1::BB-LEVELINFOREC~%")

  (format stream "  :OBJECTS ~S~%"     (bb-levelinforec-objects    instance))
  (format stream "  :BLACKBOARD ~S~%"  (bb-levelinforec-blackboard instance))
  (format stream "  :SHORT-NAME ~S~%"  (bb-levelinforec-short-name instance))
  (format stream "  )")
  )




;;;-------  BB-LINKINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-linkinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-LINKINFOREC."

  (declare (ignore print-depth))

  ;; Has fields LINKED-OBJECTS SHORT-NAME

  (format stream " #S(BB1::BB-LINKINFOREC~%")

  (format stream "  :LINKED-OBJECTS ~S~%"  (bb-linkinforec-linked-objects instance))
  (format stream "  :SHORT-NAME ~S~%"      (bb-linkinforec-short-name instance))
  (format stream "  )")
  )




;;;-------  BB-OBJECTINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-objectinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-OBJECTINFOREC."

  (declare (ignore print-depth))

  ;; Has fields ATTRIBUTES LINKS $LINKS LEVEL SHORT-NAME BLACKBOARD

  (format stream " #S(BB1::BB-OBJECTINFOREC~%")

  (format stream "  :ATTRIBUTES ~S~%"   (bb-objectinforec-attributes instance))
  (format stream "  :LINKS ~S~%"        (bb-objectinforec-links      instance))
  (format stream "  :$LINKS ~S~%"       (bb-objectinforec-$links     instance))
  (format stream "  :LEVEL ~S~%"        (bb-objectinforec-level      instance))
  (format stream "  :SHORT-NAME ~S~%"   (bb-objectinforec-short-name instance))
  (format stream "  :BLACKBOARD ~S~%"   (bb-objectinforec-blackboard instance))
  (format stream "  )")
  )




;;;-------  BB-SHORTATTRIBUTEINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-shortattributeinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-SHORTATTRIBUTEINFOREC."

  (declare (ignore print-depth))

  ;; Has fields INHERITANCE-OPPORTUNITIES

  (format stream " #S(BB1::BB-SHORTATTRIBUTEINFOREC~%")

  (format stream "  :INHERITANCE-OPPORTUNITIES ~S~%"
	  (bb-shortattributeinforec-inheritance-opportunities instance))
  (format stream "  )")
  )




;;;-------  BB-SHORTLINKINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-shortlinkinforec-print-function (instance stream print-depth)
  "Prints an instance of BB-SHORTLINKINFOREC."

  (declare (ignore print-depth))

  ;; Has fields OPPLINK INHERITANCE-OPPORTUNITIES

  (format stream " #S(BB1::BB-SHORTLINKINFOREC~%")

  (format stream "  :OPPLINK ~S~%"                    (bb-shortlinkinforec-opplink instance))
  (format stream "  :INHERITANCE-OPPORTUNITIES ~S~%"  (bb-shortlinkinforec-inheritance-opportunities instance))
  (format stream "  )")
  )




;;;-------  BB-SYSTEMINFOREC-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun bb-systeminforec-print-function (instance stream print-depth)
  "Prints an instance of BB-SYSTEMINFOREC."

  (declare (ignore print-depth))

  ;; Has fields DOMAIN-BLACKBOARDS CONTROL-BLACKBOARDS KNOWLEDGEBASES DATA-ELEMENTS

  (format stream " #S(BB1::BB-SYSTEMINFOREC~%")

  (format stream "  :DOMAIN-BLACKBOARDS ~S~%"   (bb-systeminforec-domain-blackboards instance))
  (format stream "  :CONTROL-BLACKBOARDS ~S~%"  (bb-systeminforec-control-blackboards instance))
  (format stream "  :KNOWLEDGEBASES ~S~%"       (bb-systeminforec-knowledgebases instance))
  (format stream "  :DATA-ELEMENTS ~S~%"        (bb-systeminforec-data-elements instance))
  (format stream "  )")
  )




;;;-------  KNOWLEDGE-SOURCE-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun knowledge-source-print-function (instance stream print-depth)
  "Prints an instance of KNOWLEDGE-SOURCE."

  (declare (ignore print-depth))

  ;; Has fields  TRIGGER-CONDITIONS
  ;;            CONTEXT
  ;;            PRECONDITIONS
  ;;            OBVIATION-CONDITIONS
  ;;            ACTION-VARIABLES
  ;;            ACTIONS
  ;;            FROM-BLACKBOARD
  ;;            TO-BLACKBOARD
  ;;            FROM-LEVEL
  ;;            TO-LEVEL
  ;;            NAME
  ;;            DATE-CREATED
  ;;            DESCRIPTION
  ;;            AUTHOR
  ;;            COST
  ;;            RELIABILITY 


  (format stream " #S(BB1::KNOWLEDGE-SOURCE~%")

  (format stream "  :TRIGGER-CONDITIONS ~S~%"   (knowledge-source-trigger-conditions   instance))
  (format stream "  :CONTEXT ~S~%"              (knowledge-source-context              instance))
  (format stream "  :PRECONDITIONS ~S~%"        (knowledge-source-preconditions        instance))
  (format stream "  :OBVIATION-CONDITIONS ~S~%" (knowledge-source-obviation-conditions instance))
  (format stream "  :ACTION-VARIABLES ~S~%"     (knowledge-source-action-variables     instance))
  (format stream "  :ACTIONS ~S~%"              (knowledge-source-actions              instance))
  (format stream "  :FROM-BLACKBOARD ~S~%"      (knowledge-source-from-blackboard      instance))
  (format stream "  :TO-BLACKBOARD ~S~%"        (knowledge-source-to-blackboard        instance))
  (format stream "  :FROM-LEVEL ~S~%"           (knowledge-source-from-level           instance))
  (format stream "  :TO-LEVEL ~S~%"             (knowledge-source-to-level             instance))
  (format stream "  :NAME ~S~%"                 (knowledge-source-name                 instance))
  (format stream "  :DATE-CREATED ~S~%"         (knowledge-source-date-created         instance))
  (format stream "  :DESCRIPTION ~S~%"          (knowledge-source-description          instance))
  (format stream "  :AUTHOR ~S~%"               (knowledge-source-author               instance))
  (format stream "  :COST ~S~%"                 (knowledge-source-cost                 instance))
  (format stream "  :RELIABILITY ~S~%"          (knowledge-source-reliability          instance))
  (format stream "  )")
  )



;;;-------  WORD-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun word-print-function (instance stream print-depth)
  "Prints an instance of WORD."

  (declare (ignore print-depth))

  ;; Has fields PRINT-NAME POSITION ANCHOR TYPE

  (format stream " #S(BB1::WORD~%")

  (format stream "  :PRINT-NAME ~A~%" (word-print-name instance))
  (format stream "  :POSITION ~A~%"   (word-position   instance))
  (format stream "  :ANCHOR ~A~%"     (word-anchor     instance))
  (format stream "  :TYPE ~A~%"       (word-type       instance))
  (format stream "  )")
  )




;;;-------  TRIGGER-TREE-PRINT-FUNCTION  -------

;;;Edited by Mike Hewett                1 Jun 87  13:55
;;;Edited by Mike Hewett                2 Jun 87  12:36
(defun trigger-tree-print-function (instance stream print-depth)
  "Prints an instance of TRIGGER-TREE"

  (declare (ignore print-depth))

  ;; Has fields  SORTKEY TRIGCONDLIST POTENTIALKSES

  (format stream " #S(BB1::trigger-condition-treerec~%")

  (format stream "  :SORTKEY ~S~%"    (trigger-condition-treerec-sortkey instance))

  (format stream "  :TRIGCONDLIST ")  (write (trigger-condition-treerec-trigcondlist  instance)
					     :stream stream :escape t :pretty t)
                                      (terpri stream)

  (format stream "  :POTENTIALKSES ~S~%" (trigger-condition-treerec-potentialkses instance))
  (format stream "  )")
  )
