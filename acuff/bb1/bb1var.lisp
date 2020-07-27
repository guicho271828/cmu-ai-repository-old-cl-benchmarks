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
;;;  BB1VAR.LISP      -     Variables and Constants for the BB1 system
;;;
;;;  Mike Hewett     02/04/86
;;;                    03/26/86  - ADDED GLOBAL VARIABLES FOR STORING KNOWLEDGEBASE INFO  -MH
;;;                    04/02/86  - ADDED *BB1-PRINT-WARNINGS* and *BB1-PRINT-ERRORS*       -mh
;;;                    04/07/86  - Added *BB1-LANGUAGE-LEVELS*, *BB1-ACTION-VERBS*, AND *BB1-LANGUAGE-NOISE-WORDS*  -mh
;;;                    04/08/86  - Added *PRECONDITION-RECHECK-INTERVAL* *OBVIATION-INTERVAL*    -mh
;;;                    07/09/86  - Added user-graphics option to BB1 run-time menu                    -mh
;;;                    07/24/86  - Added *SAVE-HASH-TABLE-TYPE* for fast or slow saving.            -mh
;;;                    09/02/86  - Added a couple of fonts and a spacing constant for DRAW-CONTROL-PLAN   -mh
;;;                    09/16/86  - Added *BB1-LEFT-ARROW-STRING* and *BB1-RIGHT-ARROW-STRING*         -mh
;;;                    09/22/86  - Added new hash arrays per restructuring.                                     -mh
;;;                    09/23/86  - Modified BBEDIT menu strings                                                  -mh
;;;                    10/07/86  - Added variable for Explain package                                            -mh
;;;                    10/15/86  - FONTS:BIGFONT  -> FONTS:BIGFNT                                             -mh
;;;                    10/15/86  - Added *RATE-TRIGGERED-KSARS?*                                             -mh
;;;                    10/28/86  - Added PMT-EDIT variables                                                      -mh
;;;                    10/28/86  - Added *BB1-FILE-EXTENSIONS*                                                 -mh
;;;                    11/05/86  - Removed variable *ANY-ACTIVE-CRITERIA*                                      -mh
;;;                    11/17/86  - Modified *BB1-USER-FILE-NAME-SUFFIXES* because KSes are on blackboard    -mh
;;;                    11/29/86  - Added *BOUND-VARIABLES* and *CONTEXT-VARIABLES* for new EVAL mechanism -mh
;;;                    12/01/86  - Added *CURRENT-SKILL* and *CURRENT-PROBLEM*                               -mh
;;;                    12/10/86  - Added a new menu item to KSEDIT-MENU-FIELDS                                 -mh
;;;                    1/25/87  - Added *last-root* and *last-link* for hierarchical mode of bbedit                  -reed
;;;                    1/25/87  - Modified bbedit menu vars                                                         -reed
;;;                    02/18/87 - Added *BB1-INFO-RETENTION-TIME*                                               -mh
;;;                    02/18/87 - Added *CHARGE-AHEAD-KSARS*                                                   -mh
;;;                    02/18/87 - Added *save-*bb-...* variables for hash arrays                                    -mh
;;;                    02/21/87 - Added new menu command to main BB1 menu                                     -mh
;;;                    03/11/87 - Modified BBEDIT menu commands                                                   -mh
;;;                    03/15/87 - Modified KSEDIT menu commands                                                   -mh
;;;                    03/18/87 - Added *USER-ACCESSIBLE-VARIABLES*                                              -mh
;;;                    03/19/87 - Added *COMPONENT-PMTS*                                                        -mh
;;;                    03/23/87 - Added some main BB1 menu commands                                             -mh
;;;                    03/25/87 - Added *GDR-THRESHOLD*                                                          -mh
;;;                    03/26/87 - Added *BB1-EDIT-CHANGES*                                                       -mh
;;;                    04/01/87 - Modified some BBEDIT command menu strings                                       -mh
;;;                    04/24/87 - Added *BB1-VERSION*                                                              -mh
;;;                    05/21/87 - Added *BB1-RESTART-RUN-BEHAVIOR*                                              -mh
;;;                    05/26/87 - Fixed *BB1-FILE-EXTENSIONS* for TI Release 3.                                     -mh
;;;                    06/02/87 - Added to *USER-TRIGGER-FUNCTIONS*                                              -mh

;;;  This file contains variable and constant declarations used in
;;;  the BB1 system.  As suggested in "Common Lisp: The Language",
;;;  DEFVAR is used for variables which the system may change.
;;;  DEFPARAMETER is used for variables which the system should
;;;               normally not change, and
;;;  DEFCONSTANT is used for true constants which can be optimized by the compiler.
;;;
;;;  As also suggested in CLtL, all constants have names which start
;;;     and end with an asterisk. (*)


;;;-------------------------------------------------------------------------------


;;;----- Package information ------

(in-package 'BB1)

  ;; Put symbols to export in the BB1PACKAGE file.

;;;----------------------------


;;;VERSION VERSION VERSION VERSION

;;;Edited by Mike Hewett   24 Apr 87  12:24
(defvar *bb1-version* "BB1 (2.0)")

;;-----------  Variables used by the Blackboard Manager  ---   form:  *bb-...   -----------


(defconstant *bb-data-array-names* '(bb1::*bb-attributeinfo*     bb1::*bb-blackboardinfo* bb1::*bb-datainfo*
				     bb1::*bb-knowledgebaseinfo* bb1::*bb-levelinfo*      bb1::*bb-linkinfo*
				     bb1::*bb-objectinfo*        bb1::*bb-shortattributeinfo*
				     bb1::*bb-shortlinkinfo*     bb1::*bb-systeminfo*)
  "Names for hash arrays used by the Blackboard Manager.")

(defconstant *bb-data-array-initial-lengths* '(1000 10 25 10 100 300 500 100 100 10)
  "Initial lengths for the Blackboard hash arrays.")

(defconstant *bb-file-prefix*  '*file-
  "Prefix placed on the bb-data-arrays when saving/loading to/from a file.")

(defconstant *bb-save-prefix*  '*save-
  "Prefix placed on the bb-data-arrays when saving/loading to/from a file.")

(defvar *bb-attributeinfo*      nil "Hash array for ATTRIBUTES.  Used by the Blackboard Manager.")
(defvar *bb-blackboardinfo*     nil "Hash array for BLACKBOARDS.  Used by the Blackboard Manager.")
(defvar *bb-datainfo*           nil "Hash array for DATA.  Used by the Blackboard Manager.")
(defvar *bb-knowledgebaseinfo*  nil "Hash array for KNOWLEDGEBASES.  Used by the Blackboard Manager.")
(defvar *bb-levelinfo*          nil "Hash array for LEVELS.  Used by the Blackboard Manager.")
(defvar *bb-linkinfo*           nil "Hash array for LINKS.  Used by the Blackboard Manager.")
(defvar *bb-objectinfo*         nil "Hash array for OBJECTS.  Used by the Blackboard Manager.")
(defvar *bb-shortattributeinfo* nil "Hash array for 2Attributes*.  Used by the Blackboard Manager.")
(defvar *bb-shortlinkinfo*      nil "Hash array for 2Links*.  Used by the Blackboard Manager.")
(defvar *bb-systeminfo*         nil "Hash array for SYSTEMS.  Used by the blackboard Manager.")

(defvar *save-*bb-attributeinfo*      nil "Hash array for ATTRIBUTES.  Used by the Blackboard Manager.")
(defvar *save-*bb-blackboardinfo*     nil "Hash array for BLACKBOARDS.  Used by the Blackboard Manager.")
(defvar *save-*bb-datainfo*           nil "Hash array for DATA.  Used by the Blackboard Manager.")
(defvar *save-*bb-knowledgebaseinfo*  nil "Hash array for KNOWLEDGEBASES.  Used by the Blackboard Manager.")
(defvar *save-*bb-levelinfo*          nil "Hash array for LEVELS.  Used by the Blackboard Manager.")
(defvar *save-*bb-linkinfo*           nil "Hash array for LINKS.  Used by the Blackboard Manager.")
(defvar *save-*bb-objectinfo*         nil "Hash array for OBJECTS.  Used by the Blackboard Manager.")
(defvar *save-*bb-shortattributeinfo* nil "Hash array for 2Attributes*.  Used by the Blackboard Manager.")
(defvar *save-*bb-shortlinkinfo*      nil "Hash array for 2Links*.  Used by the Blackboard Manager.")
(defvar *save-*bb-systeminfo*         nil "Hash array for SYSTEMS.  Used by the blackboard Manager.")

;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-attributeinfo*      nil "Hash array for ATTRIBUTES.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-blackboardinfo*     nil "Hash array for BLACKBOARDS.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-datainfo*           nil "Hash array for DATA.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-knowledgebaseinfo*  nil "Hash array for KNOWLEDGEBASES.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-levelinfo*          nil "Hash array for LEVELS.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-linkinfo*           nil "Hash array for LINKS.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-objectinfo*         nil "Hash array for OBJECTS.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-shortattributeinfo* nil "Hash array for 2Attributes*.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-shortlinkinfo*      nil "Hash array for 2Links*.  Used by the Blackboard Manager.")
;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *file-*bb-systeminfo*         nil "Hash array for SYSTEMS.  Used by the blackboard Manager.")

;;;Edited by Mike Hewett   23 Apr 87  23:24
(defvar *bb1-save-memory-flag* t "Controls saving of previous values on attributes.")

(defvar *bb1-edit-changes*  nil   "2Used to collect user's changes during an editing session.*")



;;-----------  Variables used by the Display Manager  ---------------------

(defvar *bb1-message-stream*     t  "Standard window for BB1 messages.")
(defvar *bb1-event-display-fn* nil  "User-settable function.")
(defvar *bb1-user-graphics-fn* nil  "User-settable function.")

(defvar *bb1-bb-window*        nil  "Always bound to the window displaying a blackboard.")

(defconstant *bb1-display-bb-top-position*    100  "Default blackboard window vertical position in pixels.")
(defconstant *bb1-display-bb-left-position*    10  "Default blackboard window horizontal position in pixels.")
(defconstant *bb1-display-bb-height*          600  "Default blackboard window height in pixels.")
(defconstant *bb1-display-bb-width*          1000  "Default blackboard window width in pixels.")

(defconstant *bb1-display-max-characters-per-object-name*  20  "Max length of displayed name.")
(defconstant *bb1-display-max-display-blackboards*          1  "Number of simultaneously-displayable bb's.")
(defconstant *bb1-display-max-display-levels*          100000  "Max levels of a blackboard to display.")
(defconstant *bb1-display-max-display-objects*         100000  "Max objects to display at a level.")

;;;Edited by Mike Hewett   27 Apr 87  12:39
(defconstant *bb1-display-after-bb-label-vertical-separation*     30 "# of pixels after bb label.")
;;;Edited by Mike Hewett   27 Apr 87  12:56
(defconstant *bb1-display-after-level-label-vertical-separation*  25 "# of pixels after level label.")
(defconstant *bb1-display-after-object-vertical-separation*       20 "# of pixels vertically between objects.")
(defconstant *bb1-display-after-object-horizontal-separation*      2 "# of blank spaces after object.")
;;;Edited by Mike Hewett   27 Apr 87  12:56
;;;Edited by Mike Hewett   27 Apr 87  13:20
(defconstant *bb1-display-after-level-vertical-separation*        30 "# of pixels vertically between objects.")
(defconstant *bb1-display-after-bb-vertical-separation*            0 "# of pixels vertically between objects.")

(defconstant *bb1-control-plan-unit-spacing*           32  "How many pixels per unit on the time line?")


    ;; FONTS

;(defconstant *bb1-inordinantly-fancy-label-font*  'fonts:40vr         "Super-duper font.")  ; use shadow40 if exists
;(defconstant *bb1-fancy-label-font*               'fonts:40vr         "Fancy font for big labels.")
;(defconstant *bb1-big-label-font*                 'fonts:METS         "Large font for labels.") ;use 25FR3 if exists
;(defconstant *bb1-standard-label-font*            'fonts:hl12bi       "Standard label font.")
;(defconstant *bb1-tiny-label-font*                'fonts:cptfontb     "2Small label font.*")

;(defconstant *bb1-fancy-display-font*      'fonts:METS          "Fancy font for big text.") ; use 25FR3 if exists
;(defconstant *bb1-big-display-font*        'fonts:bigfnt        "Large font for text.")
;(defconstant *bb1-bold-display-font*       'fonts:hl12b         "Bold version of standard font.")
;(defconstant *bb1-italic-display-font*     'fonts:hl12i         "Italic version of standard font.")
;(defconstant *bb1-standard-display-font*   'fonts:cptfont       "Standard font for text.")
;(defconstant *bb1-small-display-font*      'fonts:hl10          "Small font for standard text.")
;(defconstant *bb1-tiny-font*               'fonts:cptfont       "2Tiny font for standard text.*")
;(defconstant *bb1-link-label-font*         'fonts:hl10          "Font for labels in hierarchical mode")

   ;; Constants which may not be machine-independent

(defconstant *bb1-left-arrow-string*    (string (code-char 24))     "2Should print as a left-arrow*")
(defconstant *bb1-right-arrow-string*   (string (code-char 25))     "2Should print as a right-arrow*")


;;-----------  Variables used by BBEDIT  ---------------------

(defvar *bbedit-display* nil    "Frame for all BBEDIT interactions.")

(defvar *bbedit-state*          nil "Current BBEDIT state.")

(defvar *bbedit-current-blackboard* nil "Blackboard currently being edited.")
;;;Edited by Mike Hewett                21 May 87  13:21
(defvar *bbedit-current-kb*         nil "Knowledge base currently being edited.")



;;-----------  Variables used by KSEDIT  ---------------------

(defvar *ksedit-display* nil    "Frame for all KSEDIT interactions.")

(defvar *ksedit-state*          nil "Current KSEDIT state.")

(defvar *ksedit-current-ks*     nil "Knowledge source currently being edited.")

(defvar *ksedit-unsaved-ks*     nil "Set to T if KS needs to be saved.")

(defvar *ksedit-unsaved-file*   nil "Set to T if file of KSes needs to be saved.")

(defconstant *ksedit-menu-strings*
	                      '(("Exit KSEDIT"                         . EXIT-KSEDIT)
				
				("Refresh KS information"              . REFRESH-DISPLAY)
				("Reset display"                       . RESET-DISPLAY)
				("Save KS(es) to a file"               . SAVE-KS)
				("Store edited version of KS"          . STORE-KS)

				("Build a new knowledge source"        . BUILD-NEW-KS)
				("Copy a knowledge source"             . COPY-KS)
				("Initialize KS with templates"        . MAKE-TEMPLATE-KS)
				("Edit the current BB1 KSes"           . EDIT-CURRENT-KSES)
				("Edit another knowledge source"       . EDIT-SOME-KS)
				("Edit knowledge sources on a file"    . EDIT-KSES-ON-FILE)
				("Load more KSes from a file"          . EDIT-KSES-ON-FILE)
				("Display this knowledge source"       . DISPLAY-ALL-KS)
				("Display another knowledge source"    . DISPLAY-SOME-KS)
				("Pretty print kses to a file"         . PRETTY-PRINT-KS-TO-FILE)

				("Rename this knowledge source"        . RENAME-KS)
				("Delete this knowledge source"        . DELETE-KS)
				("Load old style KS file"              . LOAD-OLD-KS-FILE)
				)

  "Menu-to-command mappings used in PROCESS-KSEDIT-MOUSE-INPUT.  Related to UPDATE-KSEDIT-MENU.")

(defconstant *KS-FIELDS*  '((trigger-conditions   .  USER::triggerconditions)
			    (context              .  USER::ksarcontexts)
			    (preconditions        .  USER::preconditions)
			    (obviation-conditions .  USER::obviationconditions)
			    (action-variables     .  USER::ksvars)
			    (actions              .  USER::actions)
			    (from-blackboard      .  USER::frombb)
			    (to-blackboard        .  USER::tobb)
			    (from-level           .  USER::fromlevel)
			    (to-level             .  USER::tolevel)
			    (name                 .  USER::name)
			    (date-created         .  USER::datecreated)
			    (description          .  USER::description)
			    (author               .  USER::author)
			    (cost                 .  USER::cost)
			    (reliability          .  USER::reliability))
  "Mapping from ks record fields to knowledge-source level attribute names.")



;;----------  Variables used by EXPLAIN  -------------

(defvar *EXPLAIN-DISPLAY*  nil   "2Display frame for BB1 explanation.*")

(defvar *KSAR-TO-EXPLAIN*  nil   "2The KSAR being explained by EXPLAIN functions.*")

(defvar *RECOMMENDED-KSAR*  nil   "2The KSAR being recommended by the scheduler.*")

(defvar *FOCUS-TO-COMPARE*  nil   "2The focus used for rating by EXPLAIN functions.*")

(defvar *EXPLAIN-ITEM-ALIST* nil "2Defines menus for mouse-sensitive main window*")

(defconstant *EXPLAIN-MENU-STRINGS*  '(("Help   "           .  MAIN-HELP)
				       ("Quit   "           .  MAIN-QUIT)
				       ("Feasible"          .  FEASIBLE)
				       ("Past KSARs"        .  PAST)
				       ("Left   "           .  MAIN-LEFT)
				       ("Right  "           .  MAIN-RIGHT)
				       ("Up     "           .  MAIN-UP)
				       ("Down   "           .  MAIN-DOWN)
				       ("Reset  "           .  RESET)
				       ("Left  "            .  RATING-LEFT)
				       ("Right "            .  RATING-RIGHT)
				       ("Up    "            .  RATING-UP)
				       ("Down  "            .  RATING-DOWN)
				       ("Left "             .  COMPARE-LEFT)
				       ("Right"             .  COMPARE-RIGHT)
				       ("Up   "             .  COMPARE-UP)
				       ("Down "             .  COMPARE-DOWN)
				       ("Compare"           .  COMPARE)
				       )
  "Menu to command mappings for explanation package.")



;;-----------  Variables used by PMT-EDIT  ---------------------

(defvar *language-name* nil)		   ;these two specials are only used in cvv menus.
(defvar *language-blackboard-name* nil)





;;-----------  Variables used by BB1 in general  ---------------------


(defvar *bb1-system-name* nil   "Set to the name of the current user's system.  Used internally by
the Blackboard Manager, so care must be taken that it is always set correctly.")

(defvar *bb1-display*            nil   "Main display frame for BB1.")
(defvar *bb1-current-blackboard* nil   "Blackboard currently being displayed by BB1.")
(defvar *bb1-current-agenda*     'USER::executableksars  "Agenda currently being displayed by BB1.")

(defvar *other-user-file-or-files* nil "For setup menu.  These files are loaded by bb1 at the start of a run")
(defvar *GDR-THRESHOLD* 101 "2Threshold (percentage) above which Goal-directed reasoning KSes will run.*")

;;;Edited by Mike Hewett                21 May 87  13:21
(defvar *bb1-restart-run-behavior* :NORMAL
  "2Determines how BB1 acts when a run ends and a new one is started.  Can be :NORMAL,* 2(:CLEAR ...) or NIL.

:NORMAL  - Copies the SAVE hash arrays to the BB hash arrays.

:CLEAR    - Clears the blackboards or levels given by the arguments.
              Args can be a symbol (assumed to be a blackboard name) or
              a list (:BLACKBOARD bb-name ...) or (:LEVEL level-name ...)

NIL        - Does nothing.

   Example:  (:CLEAR   (:BLACKBOARD SOLUTION TEMP)  (:LEVEL PROBLEM.MISC))*"
  )

(defvar *bb1-state*         nil   "Current state of BB1.")

(defvar *next-interrupt-cycle* 0  "Used when 'charging ahead' in a BB1 run.")
(defvar *charge-ahead-ksars*  nil "2Used when charging ahead executing several selected KSARs.*")
(defvar *bb1-calculator-menu* nil "2Used by BB1-INPUT-INTEGER-WITH-KEYPAD*")

(defvar *current-ksar*         nil  "Set dynamically during interpretation, triggering, invoking, etc.")
(defvar *bound-variables*     '(nil nil) "2Store variable bindings of current KSAR for easy access.*")
(defvar *context-variables*   '(nil nil) "2Store variable bindings of current KSAR for easy access.*")


(defvar $TRIGGER-EVENT      nil    "Accessible by the user.")
(defvar $TRIGGER-OBJECT     nil    "Accessible by the user.")
(defvar $THIS-CYCLE         0      "The current BB1 cycle number.  Accessible by the user.")

(defvar $KSAR               nil    "The current KSAR of interest  User-accessible.")

(defvar *INITIALIZATION-FORM*    nil
  "User's function which will be EVAL'ed to start BB1.  Must return name of first KS to execute.")

(defvar *TERMINATION-CONDITION*  '(agenda-is-null)
  "User-settable condition for determining whether the BB1 run should end.")

(defvar *AFTER-TERMINATION-FORM* nil
  "User-settable form to be executed after a BB1 run ends.")

(defvar *SYSTEM-KNOWLEDGE-BASES* nil
  "User-settable list of knowledge bases to load in for a given system.")


(defvar *bb1-cycle-event-list* nil "A list of all events occurring on a given cycle.")
(defvar *next-ksar-number*  0      "Used to generate ksar names.")

(defvar *bb1-tempvar* nil       "Used as a temporary variable in menu-select operations.")

(defvar *input-data*        nil   "User-settable variable.  Holds pairs of input data names and values.")
(defvar *input-data-names*  nil   "User-settable variable.  Holds names of input data to be used.")

(defvar *STATE-VARIABLES-TO-SAVE*
	'($THIS-CYCLE $THIS-KSAR $TRIGGER-EVENT $TRIGGER-OBJECT *CURRENT-KSAR* *BOUND-VARIABLES*
		      *CONTEXT-VARIABLES* *CURRENT-AGENDA*
		      *BB1-CURRENT-AGENDA* *NEXT-INTERRUPT-CYCLE* *INPUT-DATA* *INPUT-DATA-NAMES*
		      *BB1-CYCLE-EVENT-LIST* *NEXT-KSAR-NUMBER* *TRIGGER-CONDITION-TREE*
 		      *BB1-STATE* *BBEDIT-STATE* *KSEDIT-STATE*
		       *BBEDIT-CURRENT-BLACKBOARD* *KSEDIT-CURRENT-KS*
		      *RECOMMENDED-KSAR* *BB1-CURRENT-BLACKBOARD* *BB1-CURRENT-AGENDA*
		      *PARTIAL-MATCH-TABLE* *BB1-CURRENT-DIRECTORY* *BB1-CURRENT-FILE-NAME*
		      *BB1-CURRENT-FILE-TYPE* *BB1-CURRENT-PATHNAME* *LAST-ROOT* *LAST-LINK*
		      )
  "2Variables to be saved during a SAVE-BB1-STATE.  Should not include any variables included 
on *USER-ACCESSIBLE-VARIABLES* (below), because they will be saved too.*"
  )

;;;Edited by Mike Hewett                21 May 87  13:26
;;;Edited by Mike Hewett                21 May 87  13:41
(defvar *USER-ACCESSIBLE-VARIABLES*
      '((*BB1-TEMPVAR*             "Write -SETUP after exiting" :boolean)
	(*BB1-SYSTEM-NAME*         "System name" :documentation "The name of your system." :sexp)
	(*CURRENT-SKILL*           "Skill"       :documentation
				   "Object which describes the skill being used to solve the problem."
				   :sexp)
	(*CURRENT-PROBLEM*         "Problem"     :documentation
				   "Object which describes the problem being solved." :sexp)
	
	(*RATE-TRIGGERED-KSARS?*   "Rate triggered ksars?" :boolean)
	(*INITIALIZATION-FORM*     "Initialization form" :documentation
				   "An expression which, when evaluated, must return the name of the first KS to run."
				   :sexp)
	(*TERMINATION-CONDITION*   "Termination condition"
				   :documentation
				   "An expression which, if it evaluates to a non-NIL value, causes the BB1 run to stop."
				   :sexp)
	(*AFTER-TERMINATION-FORM*  "After termination form"
				   :documentation "Evaluated after your BB1 run terminates." :sexp)
	(*BB1-RESTART-RUN-BEHAVIOR* "Restart behavior"
				    :documentation
				    "Controls how BB1 acts after terminating a run.  :NORMAL is the default"
				    :sexp)
	(*SYSTEM-KNOWLEDGE-BASES*  "Knowledge bases" :documentation "A list of knowledge bases in your system."
				   :sexp :constraint #'(lambda (val) (if (listp val) nil "Value must be a list.")))
	(*OTHER-USER-FILE-OR-FILES* "Other file or files to load"
				    :documentation "A string or list of strings" :sexp)
	(*BB1-LANGUAGE-BLACKBOARDs* "Language blackboards" :documentation
				    "A list of blackboards on which BB1 can find the definitions of user-defined actions."
				    :sexp :constraint #'(lambda (val) (if (listp val) nil "Value must be a list.")))
	(*COMPONENT-PMTS*           "Partial match tables"
				    :documentation "List of partial match table variables to use."
				    :sexp :constraint
				    #'(lambda (val) (if (listp val) nil "Value must be a list.")))
	(*PRECONDITION-RECHECK-INTERVAL* "Precondition recheck interval" :fixnum)
	(*OBVIATION-INTERVAL*            "Obviation recheck interval" :fixnum)
	(*BB1-INFO-RETENTION-TIME*       "Control info retention time" :fixnum)
	(*BB1-EVENT-DISPLAY-FN*     "Event display function"
				    :documentation "An expression to evaluate whenever an event occurs." :sexp)
	(*BB1-USER-GRAPHICS-FN*     "Graphics function"
				    :documentation
				    "An expression to evaluate when DISPLAY USER GRAPHICS is selected in BB1."
				    :sexp)
        (*BB1-SAVE-MEMORY-FLAG*     "Make efficient use of memory?"
				    :documentation
				    "T means don't save old attribute values.  NIL means save them."
				    :boolean)
	(*BB1-PRINT-WARNINGS*       "Print warning messages" :boolean)
	(*BB1-PRINT-ERRORS*         "Print error messages"   :boolean)
	)
      "2Variables which the user can access.  In format suitable for TV:CHOOSE-VARIABLE-VALUES.*"
      )


  ;; USED BY THE INTERPRETER AND LANGUAGE TRANSLATOR

(defvar *bb1-language-levels*       nil  "Levels at which BB1 will look for translations of actions.")
(defvar *bb1-language-blackboards*  nil  "Blackboards on which BB1 will look for translations of actions.")
(defvar *current-skill*             nil  "2The skill being run by BB1.*")
(defvar *current-problem*           nil  "2The problem being solved by BB1.*")

(defvar *partial-match-table*       nil  "2Partial match table for action language.*")
(defvar *component-pmts*            nil  "2Partial match tables to use during a run.  User-accessible.*")

;;;Edited by Vaughan Johnson   20 Apr 87  13:29
(defvar *bb1-language-noise-words*  '(USER::about USER::and USER::as USER::at USER::for USER::from USER::in
						  USER::into USER::of USER::ordered USER::to USER::until
						  USER::via USER::weighted USER::with)
  "Words which are ignored by the BB1 action translator.")

;;;Edited by Vaughan Johnson   20 Apr 87  13:29
(defvar *bb1-action-verbs*          '(USER::propose USER::execute USER::loop)
  "The standard bb1 action language.")


  ;; Used by the Agenda Handler

(defvar *current-agenda*                 nil   "The current agenda.  Should be set at start of run.")

(defvar *trigger-condition-tree*         nil
  "Discrimination tree used for efficient triggering of KSes.")

;;;Edited by Mike Hewett                2 Jun 87  9:50
(defvar *USER-TRIGGER-FUNCTIONS* '(($EVENT-TYPE-IS          #'(lambda (event) ($Value event 'USER::changetype)))
				 ($EVENT-LEVEL-IS         #'(lambda (event) ($Value event 'USER::eventlevel)))
				 ($CHANGED-ATTRIBUTE-IS   #'(lambda (event) (mapcar #'car
										    ($Value event
											    'USER::changes))))
				 )
      "2These are the functions recognized by the trigger condition tree builder.  The CAR is the function name
and the CDR is how to retrieve it from an event object (via FUNCALL).  Adding more elements to the list will
cause BB1 to automatically use them when building and retrieving from the tree.*"
      )


(defvar *rate-triggered-ksars?*          nil
  "2Set to T if doing goal-directed reasoning.  BB1 runs faster if NIL.*")

(defvar *precondition-recheck-interval*   3
  "User-settable variable.  Lower values may slow down a system significantly.")

(defvar *precondition-recheck-condition*  '(zerop (mod $THIS-CYCLE *precondition-recheck-interval*))
  "User-settable variable.  This can be any function.  Can not reference any KSAR's local variables.")

(defvar *obviation-interval*  3
  "User-settable variable.  Lower values may slow down a system significantly.")

(defvar *obviation-condition* '(zerop (mod $THIS-CYCLE *obviation-interval*))
  "User-settable variable.  This can be any function.  Can not reference any KSAR's local variables.")

(defvar *bb1-info-retention-time* nil
  "2User-settable variable.  If a number, will remove all but n old scheduling decisions and
old KSARs every n cycles.*")


  ;; USED BY THE SCHEDULER

(defvar *default-scheduling-rule* '(most-recently-invoked-control-ksars)
  "The user can set this too, I guess.")



  ;; FILE NAME VARIABLES - USED BY BB1-SET-FILENAME-DEFAULTS  - - - - - - - - - -


(defvar *bb1-host*              (machine-instance)
  "Will be set to the current system host.  Can be changed by the user.")

(defvar *bb1-current-directory* '(replace this)
  "The current directory being accessed (by the user).")

(defvar *bb1-current-file-name* "Replace this"
  "The current file being accessed.")

(defvar *bb1-current-file-type* "lisp"
  "This will change a lot as files get read in.")

(defvar *bb1-current-pathname*  "no-pathname"
  "The 'pathname' being accessed.")

   ;;- Used when loading a user system

(defconstant *bb1-user-file-name-suffixes* '("-SETUP" "-DOMAINFNS" "-CONTROLFNS")
  "2File suffixes which BB1 will look for when loading a user's 'system'.*")

;(defconstant *bb1-user-file-name-suffixes* '("-SETUP" "-DOMAINKS" "-DOMAINBB" "-DOMAINFNS"
;					     "-CONTROLKS" "-CONTROLFNS"))

;;;Edited by Mike Hewett                3 Jun 87  18:24
(defconstant *bb1-file-extensions*     #+(and ti release-3) '("XLD"   "LISP")
	                               #-(and ti release-3) '("XFASL" "LISP")
     "Used when loading files.")


  ;;; - - - -   I/O  - - - -  I/O  - - - -  I/O  - - - -  I/O  - - - -  I/O  - - - -


(defvar *bb1-print-warnings* t  "If NIL, won't print any BB1WARNING messages.")
(defvar *bb1-print-errors*   t  "If NIL, won't print any BB1ERROR messages.")

(defvar *bb1-default-menu-x*    450
  "Default x position for menus.")

(defvar *bb1-default-menu-y*    160
  "Default y position for menus.")

;;------------------- for bbedit's hierarchical mode ------------------

(defvar *last-root* nil "A history for the users convenience")
(defvar *last-link* nil "A history for the users convenience")
