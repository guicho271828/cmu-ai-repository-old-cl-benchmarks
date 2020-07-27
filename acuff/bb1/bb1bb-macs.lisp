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
;;;  BB1BB-MACS.LISP      -     Macros for the BB1 blackboard manager
;;;
;;;  Mike Hewett     02/04/86
;;;                    03/26/86  Added macros to handle the knowledgebase information       -mh
;;;                    04/04/86  Added delete macros for each hash array                     -mh
;;;                    09/22/86  Added low-level functions to support new link and attribute hash arrays   -mh
;;;                    09/29/86  Moved ALL-LINKS-OF-SYSTEM and ALL-ATTRIBUTES-OF-SYSTEM to
;;;                                BB1BB-FNS. They don't seem to work as macros.  Probably a compiler bug.  -mh
;;;                    10/29/86  Modified VALID-KS? for new KS representation
;;;                    11/06/86  Moved STORE-ATTRIBUTE to BB1BB-FNS after being optimized
;;;                   12/16/86  Put in two ignores to get rid of compiler warnings  -reed
;;;                   12/16/86  Added bb1-with-help-stream from bb1output. -reed

;;;  This file contains routines which are simple enough to be implemented as
;;;  macros.  They were originally true functions in the InterLisp version of BB1.

;;;  Contents:
;;;
;;;        LOW-LEVEL:    get-bb-attribute-data
;;;                        get-bb-blackboard-data
;;;                        get-bb-data-data
;;;                        get-bb-knowledgebase-data
;;;                        get-bb-level-data
;;;                        get-bb-link-data
;;;                        get-bb-object-data
;;;                        get-bb-shortattribute-data
;;;                        get-bb-shortlink-data
;;;                        get-bb-system-data
;;;
;;;                        put-bb-attribute-data
;;;                        put-bb-blackboard-data
;;;                        put-bb-data-data
;;;                        put-bb-knowledgebase-data
;;;                        put-bb-level-data
;;;                        put-bb-link-data
;;;                        put-bb-object-data
;;;                        put-bb-shortattribute-data
;;;                        put-bb-shortlink-data
;;;                        put-bb-system-data
;;;
;;;
;;;                        delete-bb-attribute-data
;;;                        delete-bb-blackboard-data
;;;                        delete-bb-data-data
;;;                        delete-bb-knowledgebase-data
;;;                        delete-bb-level-data
;;;                        delete-bb-link-data
;;;                        delete-bb-object-data
;;;                        delete-bb-shortattribute-data
;;;                        delete-bb-shortlink-data
;;;                        delete-bb-system-data
;;;
;;;
;;;        VALIDITY:       valid-attribute?
;;;                        valid-blackboard?
;;;                        valid-data?
;;;                        valid-knowledgebase?
;;;                        valid-ks?
;;;                        valid-level?
;;;                        valid-link?
;;;                        valid-object?
;;;                        valid-system?
;;;                        valid-attribute-for-system?
;;;                        valid-blackboard-for-system?
;;;                        valid-level-for-blackboard?
;;;                        valid-link-for-system?
;;;
;;;
;;;  RECORD RETRIEVAL:     links-of-system
;;;                        attribute-inheritance-opportunities-of-system
;;;                        link-inheritance-opportunities-of-system
;;;                        domain-blackboards-of-system
;;;                        control-blackboards-of-system
;;;                        data-elements-of-system
;;;
;;;                        inheritance-opportunities-of-shortattribute
;;;
;;;                        opplink-of-shortlink
;;;                        inheritance-opportunities-of-shortlink
;;;
;;;                        levels-of-blackboard
;;;                        system-of-blackboard
;;;                        knowledgebase-of-blackboard
;;;                        type-of-blackboard
;;;
;;;                        blackboards-of-knowledgebase
;;;                        system-of-knowledgebase
;;;
;;;                        objects-of-level
;;;                        possible-attributes-of-level
;;;                        blackboard-of-level
;;;                        default-attributes-of-level
;;;                        short-name-of-level
;;;
;;;                        attributes-of-object
;;;                        cached-$links-of-object
;;;                        links-of-object
;;;                        level-of-object
;;;                        short-name-of-object
;;;                        blackboard-of-object
;;;
;;;                        procedural-attachment-of-attribute
;;;                        values-of-attribute
;;;                        object-of-attribute
;;;                        short-name-of-attribute
;;;
;;;                        linked-objects-of-link
;;;                        short-name-of-link
;;;
;;;                        value-of-data
;;;
;;;
;;;  HIGH-LEVEL STORAGE:  
;;;                        add-inheritance-opportunity-to-attribute
;;;                        add-inheritance-opportunity-to-link
;;;                        add-procedural-attachment-to-attribute
;;;                        add-system
;;;                        create-data-array
;;;
;;;
;;;  EXTENDED RETRIEVAL:   actual-attributes-of-object
;;;                        actual-links-of-object
;;;                        blackboards-of-system
;;;                        inheritance-opportunity-of-attribute
;;;                        inheritance-opportunity-of-link
;;;                        levels-of-system
;;;                        objects-of-blackboard
;;;                        objects-of-system
;;;                        opposite-link-of-link
;;;
;;;
;;;  RUN-TIME FUNCTIONS:   store-attribute - moved to BB1BB-FNS
;;; OUTPUT MACROS    bb1-with-help-stream
;;;-------------------------------------------------------------------------------


;;;----- Package information ------

(in-package 'BB1)

;;;----------------------------




;;;--------        LOW-LEVEL ROUTINES        --------

   ;;---------------------------------------------------
   ;; GET routines - all do a simple hash-array retrieve.


(defmacro get-bb-attribute-data (key)

  `(gethash ,key *bb-attributeinfo*))


(defmacro get-bb-blackboard-data (key)

  `(gethash ,key *bb-blackboardinfo*))


(defmacro get-bb-data-data (key)

  `(gethash ,key *bb-datainfo*))


(defmacro get-bb-knowledgebase-data (key)

  `(gethash ,key *bb-knowledgebaseinfo*))


(defmacro get-bb-level-data (key)

  `(gethash ,key *bb-levelinfo*))


(defmacro get-bb-link-data (key)

  `(gethash ,key *bb-linkinfo*))


(defmacro get-bb-object-data (key)

  `(gethash ,key *bb-objectinfo*))


(defmacro get-bb-shortattribute-data (key)

  `(gethash ,key *bb-shortattributeinfo*))


(defmacro get-bb-shortlink-data (key)

  `(gethash ,key *bb-shortlinkinfo*))


(defmacro get-bb-system-data (key)

  `(gethash ,key *bb-systeminfo*))


   ;;------------------------------------------------
   ;; PUT routines - all do a simple hash-array store


(defmacro put-bb-attribute-data (key data)

  `(setf (gethash ,key *bb-attributeinfo*) ,data))


(defmacro put-bb-blackboard-data (key data)

  `(setf (gethash ,key *bb-blackboardinfo*) ,data))


(defmacro put-bb-data-data (key data)

  `(setf (gethash ,key *bb-datainfo*) ,data))


(defmacro put-bb-knowledgebase-data (key data)

  `(setf (gethash ,key *bb-knowledgebaseinfo*) ,data))


(defmacro put-bb-level-data (key data)

  `(setf (gethash ,key *bb-levelinfo*) ,data))


(defmacro put-bb-link-data (key data)

  `(setf (gethash ,key *bb-linkinfo*) ,data))


(defmacro put-bb-object-data (key data)

  `(setf (gethash ,key *bb-objectinfo*) ,data))


(defmacro put-bb-shortattribute-data (key data)

  `(setf (gethash ,key *bb-shortattributeinfo*) ,data))


(defmacro put-bb-shortlink-data (key data)

  `(setf (gethash ,key *bb-shortlinkinfo*) ,data))


(defmacro put-bb-system-data (key data)

  `(setf (gethash ,key *bb-systeminfo*) ,data))



   ;;------------------------------------------------
   ;; DELETE routines - all do a REMHASH operation


(defmacro delete-bb-attribute-data (key)

  `(remhash ,key *bb-attributeinfo*))


(defmacro delete-bb-blackboard-data (key)

  `(remhash ,key *bb-blackboardinfo*))


(defmacro delete-bb-data-data (key)

  `(remhash ,key *bb-datainfo*))


(defmacro delete-bb-knowledgebase-data (key)

  `(remhash ,key *bb-knowledgebaseinfo*))


(defmacro delete-bb-level-data (key)

  `(remhash ,key *bb-levelinfo*))


(defmacro delete-bb-link-data (key)

  `(remhash ,key *bb-linkinfo*))


(defmacro delete-bb-object-data (key)

  `(remhash ,key *bb-objectinfo*))


(defmacro delete-bb-shortattribute-data (key)

  `(remhash ,key *bb-shortattributeinfo*))


(defmacro delete-bb-shortlink-data (key)

  `(remhash ,key *bb-shortlinkinfo*))


(defmacro delete-bb-system-data (key)

  `(remhash ,key *bb-systeminfo*))



;;; ------------  VALIDITY-CHECKING ROUTINES ----------------


(defmacro valid-attribute? (key)

  `(get-bb-attribute-data ,key))


(defmacro valid-blackboard? (key)

  `(get-bb-blackboard-data ,key))


(defmacro valid-data? (key)

  `(get-bb-data-data ,key))


(defmacro valid-knowledgebase? (key)

  `(get-bb-knowledgebase-data ,key))


(defmacro valid-ks? (ks)

  `(member ,ks (mapcar #'(lambda (obj) (short-name-of-object obj))
			 (all-kses-of-system)))
  )

;  `(valid-object? (make-extended-name 'control-data.knowledge-source ,ks)))
;       )



(defmacro valid-level? (key)

  `(get-bb-level-data ,key))


(defmacro valid-link? (key)

  `(get-bb-link-data ,key))


(defmacro valid-object? (key)

  `(get-bb-object-data ,key))


(defmacro valid-system? (key)

  `(get-bb-system-data ,key))


;;;-------   SPECIAL NOTE:  The following macros take advantage of the fact
;;;-------                  that Common Lisp guarantees that
;;;
;;;-------                        (CAR NIL) = (CDR NIL) = NIL.
;;;
;;;-------                  If (God forbid!) this should ever change, additional
;;;-------                  code will be needed in the macros.


(defmacro valid-attribute-for-system? (attribute system)
  (ignore system) ;; reed 16 Dec 86

  `(get-bb-shortattribute-data ,attribute))


(defmacro valid-attribute-for-object? (attribute object)

  `(cdr (assoc ,attribute (attributes-of-object ,object))))


(defmacro valid-blackboard-for-system? (blackboard system)

  `(car (member ,blackboard (blackboards-of-system ,system))))


(defmacro valid-level-for-blackboard? (level blackboard)

  `(car (member ,level (levels-of-blackboard ,blackboard))))


(defmacro valid-link-for-object? (link object)

  `(cdr (assoc ,link (links-of-object ,object))))


(defmacro valid-link-for-system? (link system)
   (ignore system) ;;reed 16 Dec 86
  `(get-bb-shortlink-data ,link))



;;-------  RECORD RETRIEVAL ROUTINES  ------


;;----- SYSTEM INFO

(defmacro domain-blackboards-of-system (system)

  (let ((record (gensym)))
    `(let ((,record (get-bb-system-data ,system)))
       (and ,record (bb-systeminforec-domain-blackboards ,record)))))


(defmacro control-blackboards-of-system (system)

  (let ((record (gensym)))
    `(let ((,record (get-bb-system-data ,system)))
       (and ,record (bb-systeminforec-control-blackboards ,record)))))


(defmacro knowledgebases-of-system (system)

  (let ((record (gensym)))
    `(let ((,record (get-bb-system-data ,system)))
       (and ,record (bb-systeminforec-knowledgebases ,record)))))


(defmacro data-elements-of-system (system)

  (let ((record (gensym)))
    `(let ((,record (get-bb-system-data ,system)))
       (and ,record (bb-systeminforec-data-elements ,record)))))



;;----- SHORTATTRIBUTE INFO

(defmacro inheritance-opportunities-of-shortattribute (attribute)

  (let ((record (gensym)))
    `(let ((,record (get-bb-shortattribute-data ,attribute)))
       (and ,record (bb-shortattributeinforec-inheritance-opportunities ,record)))))
  


;;----- SHORTLINK INFO

(defmacro inheritance-opportunities-of-shortlink (link)

  (let ((record (gensym)))
    `(let ((,record (get-bb-shortlink-data ,link)))
       (and ,record (bb-shortlinkinforec-inheritance-opportunities ,record)))))
  

(defmacro opplink-of-shortlink (link)

  (let ((record (gensym)))
    `(let ((,record (get-bb-shortlink-data ,link)))
       (and ,record (bb-shortlinkinforec-opplink ,record)))))
  



;;----- KNOWLEDGEBASE INFO

(defmacro blackboards-of-knowledgebase (knowledgebase)

  (let ((record (gensym)))
    `(let ((,record (get-bb-knowledgebase-data ,knowledgebase)))
       (and ,record (bb-knowledgebaseinforec-blackboards ,record)))))


(defmacro system-of-knowledgebase (knowledgebase)

  (let ((record (gensym)))
    `(let ((,record (get-bb-knowledgebase-data ,knowledgebase)))
       (and ,record (bb-knowledgebaseinforec-system ,record)))))



;;----- BLACKBOARD INFO

(defmacro levels-of-blackboard (blackboard)

  (let ((record (gensym)))
    `(let ((,record (get-bb-blackboard-data ,blackboard)))
       (and ,record (bb-blackboardinforec-levels ,record)))))


(defmacro system-of-blackboard (blackboard)

  (let ((record (gensym)))
    `(let ((,record (get-bb-blackboard-data ,blackboard)))
       (and ,record (bb-blackboardinforec-system ,record)))))


(defmacro knowledgebase-of-blackboard (blackboard)

  (let ((record (gensym)))
    `(let ((,record (get-bb-blackboard-data ,blackboard)))
       (and ,record (bb-blackboardinforec-knowledgebase ,record)))))


(defmacro type-of-blackboard (blackboard)

  (let ((record (gensym)))
    `(let ((,record (get-bb-blackboard-data ,blackboard)))
       (and ,record (bb-blackboardinforec-type ,record)))))



;;----- LEVEL INFO

(defmacro objects-of-level (level)

  (let ((record (gensym)))
    `(let ((,record (get-bb-level-data ,level)))
       (and ,record (bb-levelinforec-objects ,record)))))


(defmacro blackboard-of-level (level)

  (let ((record (gensym)))
    `(let ((,record (get-bb-level-data ,level)))
       (and ,record (bb-levelinforec-blackboard ,record)))))


(defmacro short-name-of-level (level)

  (let ((record (gensym)))
    `(let ((,record (get-bb-level-data ,level)))
       (and ,record (bb-levelinforec-short-name ,record)))))




;;----- OBJECT INFO

(defmacro attributes-of-object (object)

  (let ((record (gensym)))
    `(let ((,record (get-bb-object-data ,object)))
       (and ,record (bb-objectinforec-attributes ,record)))))


(defmacro cached-$links-of-object (object)

  `($links-of-object ,object))


(defmacro $links-of-object (object)

  (let ((record (gensym)))
    `(let ((,record (get-bb-object-data ,object)))
       (and ,record (bb-objectinforec-$links ,record)))))

  
(defmacro links-of-object (object)

  (let ((record (gensym)))
    `(let ((,record (get-bb-object-data ,object)))
       (and ,record (bb-objectinforec-links ,record)))))


(defmacro level-of-object (object)

  (let ((record (gensym)))
    `(let ((,record (get-bb-object-data ,object)))
       (and ,record (bb-objectinforec-level ,record)))))


(defmacro short-name-of-object (object)

  (let ((record (gensym)))
    `(let ((,record (get-bb-object-data ,object)))
       (and ,record (bb-objectinforec-short-name ,record)))))


(defmacro blackboard-of-object (object)

  (let ((record (gensym)))
    `(let ((,record (get-bb-object-data ,object)))
       (and ,record (bb-objectinforec-blackboard ,record)))))




;;----- ATTRIBUTE INFO

(defmacro procedural-attachment-of-attribute (attribute)

  (let ((record (gensym)))
    `(let ((,record (get-bb-attribute-data ,attribute)))
       (and ,record (bb-attributeinforec-procedural-attachment ,record)))))


(defmacro values-of-attribute (attribute)

  (let ((record (gensym)))
    `(let ((,record (get-bb-attribute-data ,attribute)))
       (and ,record (bb-attributeinforec-values ,record)))))


(defmacro object-of-attribute (attribute)

  (let ((record (gensym)))
    `(let ((,record (get-bb-attribute-data ,attribute)))
       (and ,record (bb-attributeinforec-object ,record)))))


(defmacro short-name-of-attribute (attribute)

  (let ((record (gensym)))
    `(let ((,record (get-bb-attribute-data ,attribute)))
       (and ,record (bb-attributeinforec-short-name ,record)))))



;;----- LINK INFO

(defmacro linked-objects-of-link (link)

  (let ((record (gensym)))
    `(let ((,record (get-bb-link-data ,link)))
       (and ,record (bb-linkinforec-linked-objects ,record)))))


(defmacro short-name-of-link (link)

  (let ((record (gensym)))
    `(let ((,record (get-bb-link-data ,link)))
       (and ,record (bb-linkinforec-short-name ,record)))))



;;----- DATA INFO

(defmacro value-of-data (data)

  (let ((record (gensym)))
    `(let ((,record (get-bb-data-data ,data)))
       (and ,record (bb-datainforec-value ,record)))))




;;;-------------  HIGH-LEVEL STORAGE ROUTINES  -------------


(defmacro add-inheritance-opportunity-to-attribute (attribute inheritance-opportunity system)

  `(if (valid-system? ,system)

      (add-to-shortattribute-array ,attribute ,inheritance-opportunity 'update)

      (bb1error 'ADD-INHERITANCE-OPPORTUNITY-TO-ATTRIBUTE
		,system " is not a valid system name. ")
      )
  )


(defmacro add-inheritance-opportunity-to-link (link inheritance-opportunity system)

  `(if (valid-system? ,system)

      (add-to-shortlink-array ,link nil ,inheritance-opportunity 'update)

      (bb1error 'ADD-INHERITANCE-OPPORTUNITY-TO-LINK
		,system " is not a valid system name. ")
      )
  )


(defmacro add-procedural-attachment-to-attribute (function-name attribute)

  `(if (valid-attribute? ,attribute)

      (add-to-attribute-array ,attribute nil nil nil ,function-name 'update)

    (bb1error 'ADD-PROCEDURAL-ATTACHMENT-TO-ATTRIBUTE
	      ,attribute " is not a valid attribute.")
    )
  )


(defmacro add-system (system)

  `(if (valid-system? ,system)

      (bb1error 'ADD-SYSTEM
		,system " is already defined.")
     ;;ELSE
      (add-to-system-array ,system nil nil nil nil 'override)
    )
  )


(defmacro create-data-array (new-array-name initial-size)

  `(set ,new-array-name (make-hash-table :size ,initial-size)))




;;-------  EXTENDED RETRIEVAL ROUTINES  -------

(defmacro actual-attributes-of-object (object)

  `(if (valid-object? ,object)
      (mapcar #'cdr (attributes-of-object ,object))
      nil))


(defmacro actual-links-of-object (object)

  `(if (valid-object? ,object)
      (mapcar #'cdr (links-of-object ,object))
      nil))


(defmacro blackboards-of-system (system)

  `(if (valid-system? ,system)
      (append (domain-blackboards-of-system ,system)
	      (control-blackboards-of-system ,system)
	      (mapcan #'(lambda (kb)
			  (copy-tree (blackboards-of-knowledgebase kb)))
		      (knowledgebases-of-system ,system))
	      )
     )
  )




(defmacro inheritance-opportunity-of-attribute (attribute)
  "Returns a list of inheritance records.  Expects the SHORT name of the attribute."

  `(inheritance-opportunities-of-shortattribute ,attribute))


(defmacro inheritance-opportunity-of-link (link)

  `(inheritance-opportunities-of-shortlink ,link))


(defmacro levels-of-system (system)

  `(if (valid-system? ,system)
      (mapcan #'(lambda (bb) (copy-list (levels-of-blackboard bb)))
	      (blackboards-of-system ,system))
      nil)
  )


(defmacro objects-of-blackboard (blackboard)

  `(if (valid-blackboard? ,blackboard)
      (mapcan #'(lambda (lev) (copy-list (objects-of-level lev)))
	      (levels-of-blackboard ,blackboard))
      nil)
  )


(defmacro objects-of-system (system)

  `(if (valid-system? ,system)
      (mapcan #'(lambda (bb) (objects-of-blackboard bb))
	      (blackboards-of-system ,system))
      nil)
  )


;;;Edited by Mike Hewett   1 May 87  0:19
(defmacro opposite-link-of-link (link)

  (let ((record (gensym)))
    `(let ((,record (get-bb-shortlink-data ,link)))
       (and ,record (bb-shortlinkinforec-opplink ,record)))))




;;-------  RUN-TIME ROUTINES  -------

;; Optimized  11/06/86  -mh 
;; Moved to BB1BB-FNS

;(defmacro store-attribute (object attribute value)

;  `(modify-value-of-attribute (cdr (assoc ,attribute (attributes-of-object ,object)))
;			     ,value))


;;;-------------------------------------------------------------------------------

;;;---------output macros----------------

;;; BB1-WITH-HELP-STREAM      move this  to bb1-macs so
                          ;;$output in bb1output does the right thing.  reed 17 Dec 86

;;;--- This is necessary because the LMI Lambda and the TI EXPLORER have
;;;--- the macro "WITH-HELP-STREAM" in different packages.  Probably good
;;;--- for generality anyway.

;(defmacro bb1-with-help-stream ((stream . options) &body bod)
;  "Uses SYS:WITH-HELP-STREAM."

;;      (system:with-help-stream (,stream . ,options) ,@bod)      ;  LMI LAMBDA

;;                                                            		; TI EXPLORER
;  `(si:with-help-stream (,stream . ,options) ,@bod)
;  )


;;;Edited by Mike Hewett   1 May 87  0:19
;;;Edited by Mike Hewett   1 May 87  13:32
(defmacro bb1-with-help-stream ((stream . options) &body bod)

  ;;Note special methods :EXPOSE-FOR-TYPEOUT and :DEACTIVATE which are used with typeout windows

  `(let ((temp-window (send (send ,(getf options :superior) :superior) :typeout-window)))
     (unwind-protect
	 (progn (send temp-window :expose-for-typeout)
		(with-open-stream (,stream temp-window)
		  ,@bod)
		(format temp-window "~2%Hit space bar or click mouse to continue:")
		(send temp-window :any-tyi)
		)
       (send temp-window :deactivate)
       )
     )
  )

