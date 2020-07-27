(in-package "BB1")
(use-package "USER")
(export 'test-bbedit)

;;;Edited by Byung Suk Lee         3 Aug 87  16:01
;;;Edited by Byung Suk Lee         3 Aug 87  18:50
(DEFUN GET-BB1-SYMBOL (NAME NUMBER)
  (INTERN (FORMAT NIL "~s-~d" NAME NUMBER) "BB1"))


;;;Edited by Byung Suk Lee         3 Aug 87  16:01
;;;Edited by Byung Suk Lee         3 Aug 87  18:50
(DEFMACRO RUN-TIME (FUNCTION-NAME &BODY ARGS)
  `(LET ((START-TIME 0)
	 (RUN-TIME 0))
     (SETQ START-TIME (GET-INTERNAL-REAL-TIME))
     (,FUNCTION-NAME ,@ARGS)
     (SETQ RUN-TIME (- (GET-INTERNAL-REAL-TIME)
		       START-TIME))
     (FORMAT T "Run time = ~f seconds"
	     (/ (FLOAT RUN-TIME)
		(FLOAT INTERNAL-TIME-UNITS-PER-SECOND))
	     )))


;;;Edited by Byung Suk Lee         3 Aug 87  16:01
;;;Edited by Byung Suk Lee         3 Aug 87  18:50
(DEFUN COMPUTE-RUN-TIME (START-TIME END-TIME)
  (/ (- END-TIME
	 START-TIME)
     (FLOAT INTERNAL-TIME-UNITS-PER-SECOND)))


;;;Edited by Byung Suk Lee         3 Aug 87  17:03
;;;Edited by Byung Suk Lee         3 Aug 87  18:50
(DEFUN TEST-BBEDIT (&OPTIONAL (REPEAT-COUNT 3)
		    &KEY
		    (SYSTEM 'TEST-SYSTEM)
		    (BB-TYPE 'DOMAIN)
		    (BB-NAME 'TEST-BB)
		    (NO-OF-LEVELS 10)
		    (NO-OF-OBJECTS-PER-LEVEL 10)
		    (NO-OF-ATTRIBUTES-PER-OBJECT 4)
		    (NO-OF-LINKS-PER-OBJECT 1)
		    (VERBOSE NIL)
		    (TIME-INFO T)
		    (OUT-FILE *TRACE-OUTPUT*  S-OUT-FILE)) 
  
  "Test program for measuring the running time of BBEDIT functions ."
  
  (LET
    ((STREAM *TRACE-OUTPUT*)
     (ENTIRE-TEST-START-TIME 0)
     (ENTIRE-TEST-END-TIME 0)
     (SINGLE-TEST-START-TIME 0)
     (SINGLE-TEST-END-TIME 0)
     (LOCAL-LOOP1-START-TIME 0)
     (LOCAL-LOOP1-END-TIME 0))
    
    (UNWIND-PROTECT
	(PROGN

	  (IF S-OUT-FILE (SETQ STREAM	 (OPEN OUT-FILE :IF-EXISTS :NEW-VERSION
				       :IF-DOES-NOT-EXIST :CREATE
				       :DIRECTION :OUTPUT)))
	  (FORMAT STREAM "~%***** HOST : ~a *****~%" (machine-instance))
	  (IF (>= NO-OF-LINKS-PER-OBJECT NO-OF-OBJECTS-PER-LEVEL)
	      (SETQ NO-OF-LINKS-PER-OBJECT (1- NO-OF-OBJECTS-PER-LEVEL)))
	  (SET-UP-BB-DATA-ARRAYS)
	  (WHEN TIME-INFO (SETQ ENTIRE-TEST-START-TIME (GET-INTERNAL-REAL-TIME)))
	  (DOTIMES
	    (I1 REPEAT-COUNT REPEAT-COUNT)	; Repeat REPEAT-COUNT times.
	    
	    ;; Repeat BBEDIT test calls REPEAT-COUNT times and return REPEAT-COUNT
	    
	    (WHEN VERBOSE (FORMAT STREAM "~%----- Test loop ~d -----~%" I1))
	    (WHEN TIME-INFO (SETQ SINGLE-TEST-START-TIME (GET-INTERNAL-REAL-TIME)))
	    (UNWIND-PROTECT
		(PROGN
		  
		  ;; Make an initial empty system.
		  
		  (ADD-SYSTEM SYSTEM)
		  (WHEN VERBOSE (FORMAT STREAM "System ~s added.~%" SYSTEM))
		  
		  ;; Construct a blackboard and  Add objects, attributes, and links on the blackboard.
		  
		  (ADD-BLACKBOARD-TO-SYSTEM BB-NAME BB-TYPE SYSTEM)	; Add a blackboard.
		  (WHEN VERBOSE
		    (FORMAT STREAM "~s blackboard ~s added to system ~s.~%" BB-TYPE BB-NAME SYSTEM))
		  (WHEN VERBOSE (FORMAT STREAM "Building the blackboard ... ~%"))
		  (DOTIMES (I2 NO-OF-LEVELS T)
		    (WHEN VERBOSE (FORMAT STREAM "~5tLevel ~d ...~%" I2))
		    (WHEN TIME-INFO (SETQ LOCAL-LOOP1-START-TIME (GET-INTERNAL-REAL-TIME)))
		    (LET* ((LEVEL (GET-BB1-SYMBOL 'LEVEL I2))
			   (LONG-LEVEL (MAKE-EXTENDED-NAME BB-NAME LEVEL))
			   (BAG-OF-OBJECTS NIL))	; initially no object.
		      (ADD-LEVEL-TO-BLACKBOARD LEVEL BB-NAME)	; Add level.
		      
		      ;; For  each  level,
		      
		      (DOTIMES (I3 NO-OF-OBJECTS-PER-LEVEL T)
			(LET* ((OBJECT (GET-BB1-SYMBOL 'OBJECT I3))
			       (LONG-OBJECT (MAKE-EXTENDED-NAME LONG-LEVEL OBJECT)))
			  (ADD-OBJECT-TO-LEVEL OBJECT LONG-LEVEL)	;  Add object.
			  (WHEN VERBOSE (FORMAT STREAM "~10tObject ~d added.~%" I3))
			  
			  ;; For  each  object,
			  
			  (DOTIMES (I4 NO-OF-ATTRIBUTES-PER-OBJECT T)
			    (LET ((ATTRIBUTE (GET-BB1-SYMBOL 'ATTRIBUTE I4))
				  (VALUE (GET-BB1-SYMBOL 'VALUE I4)))
			      (ADD-ATTRIBUTE-TO-SYSTEM ATTRIBUTE SYSTEM)	; For system info.
			      (ADD-ATTRIBUTE-TO-OBJECT ATTRIBUTE VALUE LONG-OBJECT)))	; Add attributes.
			  (WHEN VERBOSE (FORMAT STREAM 
						"~10t~d attributes added to object ~d.~%" 
						NO-OF-ATTRIBUTES-PER-OBJECT I3))
			  (WHEN BAG-OF-OBJECTS
			    (LET ((NO-OF-LINKS (MIN NO-OF-LINKS-PER-OBJECT
						    (LENGTH BAG-OF-OBJECTS))))
			      (DOTIMES (I5 NO-OF-LINKS T)
				(LET ((LINK (GET-BB1-SYMBOL 'LINK I5))
				      (REVERSE-LINK (GET-BB1-SYMBOL 'REVERSE-LINK I5))
				      (TO-OBJECT (NTH I5 BAG-OF-OBJECTS)))
				  (UNLESS (VALID-LINK-FOR-SYSTEM? LINK SYSTEM)
				    (ADD-LINK-PAIR-TO-SYSTEM LINK REVERSE-LINK SYSTEM))	; For system info.
				  (ADD-LINK-FROM-OBJECT-TO-OBJECT LINK LONG-OBJECT TO-OBJECT)
				  (ADD-LINK-FROM-OBJECT-TO-OBJECT REVERSE-LINK TO-OBJECT LONG-OBJECT)))	; Add links.
			      (WHEN VERBOSE (FORMAT STREAM "~10t~d links added to object ~d.~%" 
						    NO-OF-LINKS I3))))
			  (PUSH LONG-OBJECT BAG-OF-OBJECTS)	
						; insert the long object name to bag
			  )))
		    (WHEN TIME-INFO (SETQ LOCAL-LOOP1-END-TIME (GET-INTERNAL-REAL-TIME))
			  (FORMAT STREAM "~5t~d objects added in ~d seconds.~%" 
				  NO-OF-OBJECTS-PER-LEVEL (COMPUTE-RUN-TIME LOCAL-LOOP1-START-TIME 
									    LOCAL-LOOP1-END-TIME))))
		  
		  ;; Reference the objects on the blackboard.
		  
		  (WHEN VERBOSE (FORMAT STREAM "Accessing the blackboard ...~%"))
		  (DOTIMES (I2 NO-OF-LEVELS T)
		    (WHEN VERBOSE (FORMAT STREAM "~5tLevel ~d ...~%" I2))
		    (WHEN TIME-INFO (SETQ LOCAL-LOOP1-START-TIME (GET-INTERNAL-REAL-TIME)))
		    (LET* ((LEVEL (GET-BB1-SYMBOL 'LEVEL I2))
			   (LONG-LEVEL (MAKE-EXTENDED-NAME BB-NAME LEVEL)))
		      
		      ;; For each level,
		      
		      (DOTIMES (I3 NO-OF-OBJECTS-PER-LEVEL T)
			(LET* ((OBJECT (GET-BB1-SYMBOL 'OBJECT I3))
			       (LONG-OBJECT (MAKE-EXTENDED-NAME LONG-LEVEL OBJECT)))
			  
			  ;; For each object,
			  
			  (DOTIMES (I4 NO-OF-ATTRIBUTES-PER-OBJECT T)
			    ($VALUE LONG-OBJECT (GET-BB1-SYMBOL 'ATTRIBUTE I4)))	; Get the values of attributes
			  
			  (WHEN VERBOSE (FORMAT STREAM 
						"~10t~d attributes of object ~d retrieved.~%" 
						NO-OF-ATTRIBUTES-PER-OBJECT I3))
			  (DOTIMES (I5 NO-OF-LINKS-PER-OBJECT T)
			    ($OBJECTS LONG-OBJECT (GET-BB1-SYMBOL 'LINK I5))
			    ($OBJECTS LONG-OBJECT (GET-BB1-SYMBOL 'REVERSE-LINK I5)
				      ))
			  (WHEN VERBOSE (FORMAT STREAM 
						"~10t~d link-pairs of object ~d retrieved.~%" 
						NO-OF-LINKS-PER-OBJECT I3))	;Get the linked objects.	
			  
			  )))
		    (WHEN TIME-INFO (SETQ LOCAL-LOOP1-END-TIME (GET-INTERNAL-REAL-TIME))
			  (FORMAT STREAM "~5t~d objects  accessed in ~d seconds.~%" 
				  NO-OF-OBJECTS-PER-LEVEL (COMPUTE-RUN-TIME LOCAL-LOOP1-START-TIME 
									    LOCAL-LOOP1-END-TIME))))
		  
		  ;; Delete the objects from the blackboard.
		  
		  (WHEN VERBOSE (FORMAT STREAM "Deleting the blackboard objects...~%"))
		  (DOTIMES (I2 NO-OF-LEVELS T)
		    (WHEN VERBOSE (FORMAT STREAM "~5tLevel ~d ...~%" I2))
		    (WHEN TIME-INFO (SETQ LOCAL-LOOP1-START-TIME (GET-INTERNAL-REAL-TIME)))
		    (LET* ((LEVEL (GET-BB1-SYMBOL 'LEVEL I2))
			   (LONG-LEVEL (MAKE-EXTENDED-NAME BB-NAME LEVEL)))
		      
		      ;; "For each level,
		      
		      (DOTIMES (I3 NO-OF-OBJECTS-PER-LEVEL T)
			(LET* ((OBJECT (GET-BB1-SYMBOL 'OBJECT I3))
			       (LONG-OBJECT (MAKE-EXTENDED-NAME LONG-LEVEL OBJECT)))
			  
			  ;; For each object,
			  
			  (DELETE-OBJECT LONG-OBJECT)	; Delete the object.
			  
			  )))
		    (WHEN TIME-INFO (SETQ LOCAL-LOOP1-END-TIME (GET-INTERNAL-REAL-TIME))
			  (FORMAT STREAM "~10t~d objects deleted in ~d seconds.~%" 
				  NO-OF-OBJECTS-PER-LEVEL (COMPUTE-RUN-TIME LOCAL-LOOP1-START-TIME 
									    LOCAL-LOOP1-END-TIME)))))
	      
	      ;; Clear up the hash array, effectively the entire system.
	      
	      (SET-UP-BB-DATA-ARRAYS)
	      (WHEN VERBOSE (FORMAT STREAM "System cleared.~%")))
	    (WHEN TIME-INFO (SETQ SINGLE-TEST-END-TIME (GET-INTERNAL-REAL-TIME))
		  (FORMAT STREAM "Test ~d took ~d seconds.~%" I1 (COMPUTE-RUN-TIME SINGLE-TEST-START-TIME 
										   SINGLE-TEST-END-TIME))))
	  (WHEN TIME-INFO (SETQ ENTIRE-TEST-END-TIME (GET-INTERNAL-REAL-TIME))
		(FORMAT STREAM "The entire test took ~d seconds.~%" (COMPUTE-RUN-TIME 
								      ENTIRE-TEST-START-TIME 
								      ENTIRE-TEST-END-TIME))))
      (IF S-OUT-FILE (CLOSE STREAM)))))


;;;Edited by Byung Suk Lee         3 Aug 87  16:01
(DEFUN BB1ERROR (CALLING-ROUTINE &REST MSG-ELEMENTS) (ERROR "~s : ~{~a ~}~%" CALLING-ROUTINE 
                                                            MSG-ELEMENTS))


;;;Edited by Byung Suk Lee         3 Aug 87  16:01
(DEFUN ADD-LINK-FROM-OBJECT-TO-OBJECT (LINK FROM-OBJECT TO-OBJECT &KEY (VERBOSE NIL))
   (LET ((FROM-OBJECT-INFO (GET-BB-OBJECT-DATA FROM-OBJECT))
         (TO-OBJECT-INFO (GET-BB-OBJECT-DATA TO-OBJECT))
         (LINK-INFO (GET-BB-SHORTLINK-DATA LINK)))
        (IF FROM-OBJECT-INFO
            (IF TO-OBJECT-INFO
                (IF LINK-INFO (LET* ((LONG-LINK (MAKE-EXTENDED-NAME FROM-OBJECT LINK))
                                     (LONG-LINK-INFO (GET-BB-LINK-DATA LONG-LINK)))
                                    (COND
                                       (LONG-LINK-INFO (UNLESS (MEMBER TO-OBJECT (
                                                                        BB-LINKINFOREC-LINKED-OBJECTS
                                                                                  LONG-LINK-INFO))
                                                              (ADD-TO-LINK-ARRAY LONG-LINK NIL 
                                                                     TO-OBJECT 'UPDATE)
                                                              (WHEN VERBOSE
                                                                    (BB1MESSAGE T "Adding link " LINK 
                                                                           " from " FROM-OBJECT 
                                                                           " to " TO-OBJECT))))
                                       (T (ADD-TO-OBJECT-ARRAY FROM-OBJECT NIL NIL NIL NIL
                                                 (CONS LINK LONG-LINK)
                                                 NIL
                                                 'UPDATE)
                                          (ADD-TO-LINK-ARRAY LONG-LINK LINK (LIST TO-OBJECT)
                                                 'OVERRIDE)
                                          (WHEN VERBOSE
                                                (BB1MESSAGE T "Adding link " LINK " from " 
                                                       FROM-OBJECT " to " TO-OBJECT "."))))
                                    LONG-LINK)
                    (BB1ERROR 'ADD-LINK-FROM-OBJECT-TO-OBJECT LINK 
                           " is not a valid link for the system of " FROM-OBJECT))
                (BB1ERROR 'ADD-LINK-FROM-OBJECT-TO-OBJECT TO-OBJECT " is not an existing object."))
            (BB1ERROR 'ADD-LINK-FROM-OBJECT-TO-OBJECT FROM-OBJECT " is not an existing object."))))


;;;Edited by Byung Suk Lee         3 Aug 87  16:01
(DEFMACRO VALID-LINK-FOR-SYSTEM? (LINK SYSTEM) (DECLARE (IGNORE SYSTEM))
                                               `(GET-BB-SHORTLINK-DATA ,LINK))

