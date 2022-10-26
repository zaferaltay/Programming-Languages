(setf outputList nil)

;;DETERMINE FUNCTIONS OF DIGIT OR LETTER------------------------------------------------------------------------------------
(defun isDigit (nextChar)
	(setq digitList '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	(if (not (equal(position nextChar digitList) nil))
		(return-from isDigit t)
		(return-from isDigit nil)
	)		
)
(defun isLetter(nextChar)
  (setq letterList '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t  #\u  #\v  #\w  #\x #\y #\z))
	(if (not (equal(position nextChar letterList) nil))
		(return-from isLetter t)
		(return-from isLetter nil)
	)  
)
;;-----------------------------------------------------------------------------------------------------------------------
(defun findkwList(str)
	  (setq kwList '( "and"  "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
	  (setq counter2 0)
	  (setq flag 0)
	  (loop
	  	 (if(string= str (elt kwList counter2)) 
	  	 	(setq flag 1)
	  	 )
	  	(when (or (= flag 1)(= counter2 17))(return str))
	  	(setq counter2 (+ counter2 1))
	  )

	  (if (= flag 1)
	  	(return-from findkwList counter2)
	  	(return-from findkwList 99)
	  )
)
(defun findopList(str)
	(setq opList '(#\+ #\- #\/ #\*  #\( #\) "**" #\" #\, ))
	 (setq counter1 0)
	  (setq flag 0)
	  (loop
	  	 (if(string= str (elt opList counter1)) 
	  	 	(setq flag 1)
	  	 )
	  	(when (or (= flag 1)(= counter1 8))(return str))
	  	(setq counter1 (+ counter1 1))
	  )

	  (if (= flag 1)
	  	(return-from findopList counter1)
	  	(return-from findopList 99)
	  )
)
;;-------------------------------------------COMMENT,IDENT,DIGIT KONTROL--------------------------------------------------------------------------------------
(defun controlCID(listStr)
 (setq errCheck 0)
 (setq dig 0)
 (setq ident 0)
 (setq commentControl 0)
 (setq lstCounter 0)
  (if(isDigit (char listStr 0))
  	 (if (char= #\0 (char listStr 0))
  	 	(if (> (length listStr) 1)
  	 		(setq errCheck 1)
  	 		(setf outputList  (append outputList (list "VALUE") )) 
  	 	)
  	 	(loop
  	 		(when (= lstCounter (length listStr))(return listStr))
  	 		(if (isDigit (char listStr lstCounter))
  	 			()
  	 			(setq errCheck 1)
  	 		)
  	 		(setq lstCounter (+ 1 lstCounter))
  	 		(setq dig 1)
  	 	)
  	 )
  	 (if(isLetter (char listStr 0))
  	 	(loop
           (when (= lstCounter (length listStr))(return listStr))
  	 		(if (or (isLetter (char listStr lstCounter))(isDigit (char listStr lstCounter)))
  	 			()
  	 			(setq errCheck 1)
  	 		)
  	 		(setq lstCounter (+ 1 lstCounter))
  	 		(setq ident 1)
  	 	)
  	 	(if (char= #\; (char listStr lstCounter))
  	 		(setq commentControl 1)
  	 		(setq errCheck 1)
  	 	)
  	 )
  )

  (if(and (= errCheck 0)(= dig 1))
  	(setf outputList  (append outputList (list "VALUE") )) 
  	(if(and (= errCheck 0)(= ident 1))
  		(setf outputList  (append outputList (list "IDENTIFIER") ))
  	)
  )

  (if(= errCheck 1)
  	(setf outputList  (append outputList (list "ERROR") ))
  )

  (if(= 1 commentControl)
  	(setf outputList  (append outputList (list "COMMENT") ))
  )

  (if(= 1 commentControl)
  	(return-from controlCID 110)
  	(return-from controlCID 55)
  )



)
;;----------------------------------------STRING PARCALARININ NE OLDUGUNU ANLAMA------------------------------------------------------------------------------
(defun lexer(mergeList)
  

  (setq tokenOpList '("OP_PLUS" "OP_MINUS"  "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_OC" "OP_COMMA") )
  (setq kwTokenList '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" 
  					  "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
  (setq ocCcFlag 0)
 
  (setq counter3 0)
  (setq control 0)
  (setq controlcıdRes 0)

  ;(print (string= (string "+") (elt opList 0)))
  ;(print (findkwList (string "deffun")))  
  ;(print (elt kwTokenList (findkwList (string "add"))))
  ;(print (elt tokenOpList (findopList (string "+"))))
  ;(setf outputList  (append outputList (list "OP_OC") )) 
  ;(setf outputList  (append outputList (list "OP_CC") ));burada flag değişimleri yap


  (loop
  	(if(not (= 99 (findopList (elt mergeList counter3)) ))
  		(if(string= (elt tokenOpList (findopList (elt mergeList counter3))) "OP_OC")
  			(if (= ocCcFlag 0)
  				(progn  
  					(setf outputList  (append outputList (list "OP_OC") ))
  					(setq ocCcFlag 1)
  				)
  				(progn 
  					(setf outputList  (append outputList (list "OP_CC") ))
  					(setq ocCcFlag 0)
  				)       
  			)
  			(setf outputList  (append outputList (list (elt tokenOPList (findopList (elt mergeList counter3)))) ));(print (elt tokenOPList (findopList (elt mergeList counter3))))
  		)
  		(if(not (= 99 (findkwList (elt mergeList counter3))))
  			(setf outputList  (append outputList (list (elt kwTokenList (findkwList (elt mergeList counter3)))) ));(print (elt kwTokenList (findkwList (elt mergeList counter3))))
  			(setq control 1)
  		)
  	)

  	(if (= control 1)
  		(setq controlcıdRes (controlCID (elt mergeList counter3))))
  	(if (= control 1)
  		(setq control 0))

  	(if(= 110 controlcıdRes)
  		(return-from lexer 999)
  	)

  	(setq counter3 (+ 1 counter3))
  	(when (= counter3 (length mergeList))(return mergeList))
  )



)


;;----------------------------------------KODLARI PARCALAMA--------------------------------------------------------------------------------
(defun readFromUser(inputString)
 (setq counter 0)
 (setq lexeme nil)
 (setq mergeList nil)
 (if (< 0 (length inputString))
 	(setq nextChar (char inputString counter))
 )
 (loop
 	(setq lexeme nil)
    (if (isDigit nextChar)
      (loop     
       (when  (not (or (isLetter nextChar)(isDigit nextChar)))(return inputString))
       (setq lexeme (concatenate 'string lexeme (list nextChar)))
       (setq counter (+ counter 1))
       (if (<= counter (- (length inputString)  1))
       	(setq nextChar (char inputString counter))
       	(setq nextChar #\Space)
       )    
      );ilk if true bitiş	
       (if(isLetter nextChar) 
      	(loop
      		;(not (or (isLetter nextChar)(isDigit nextChar))
      		(when (not (or  (and (and (char/= #\tab nextChar)(char/= #\Space nextChar))(= (findopList (string nextChar)) 99))(isLetter nextChar)(isDigit nextChar))) (return inputString))
      		(setq lexeme (concatenate 'string lexeme (list nextChar)))
            (setq counter (+ counter 1))
            (if (<= counter (- (length inputString) 1 ))
            	(setq nextChar (char inputString counter))
            	(setq nextChar #\Space)
            )

      	);else-if bitiş 
      	(loop
      		;;(when (or (not (and (char= #\* nextChar)(char/= #\* (char inputString (+ 1 counter)))))(isDigit nextChar)(isLetter nextChar)(char= nextChar #\Space))(return inputString))  		
      		(if(or (char= #\Space nextChar)(char= #\tab nextChar))
      			()
      			(setq lexeme (concatenate 'string lexeme (list nextChar)))
      		)
      		
      		(setq counter (+ counter 1))
      		(if (<= counter (- (length inputString) 1) )
        	      	(setq nextChar (char inputString counter))
        	      	(setq nextChar #\Space)
            )
            (if(< counter (length inputString) )
     		  (when (not (and (char= nextChar #\*)(char= (char inputString counter) #\*)))(return  inputString))
     		  (when (= counter (length inputString) )(return inputString))
     		)	
      	);else bitiş
      ) 
    );;tüm if bitiş
    (if  (or (char= nextChar #\Space)(char= nextChar #\tab))
    	(setq counter (+ counter 1))
    )
    (if  (and (or (char= nextChar #\Space)(char= nextChar #\tab))(< counter (length inputString)))
    	(setq nextChar (char inputString counter))
    )
    (if(< 0 (length lexeme))
    	(setq mergeList (append mergeList (list lexeme)))
    )
    
   (when (>= counter  (length inputString)  )(return inputString))
   )
 (lexer mergeList)
);function bitiş

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun start(par)
	(if (= 0 (length *args*))	
	  (loop 
	     (defparameter line (read-line))
	     (setq line(string-downcase line))	   
	     (when (= 0 (length line))(return par))
	     (readFromUser line)
      );;DOSYA YOKSA OKUMA DURUMU 
   )
	(setq counter4 0)
	(loop 
		(setq myString (concatenate 'string (string (elt outputList counter4)) '(#\newline)))
	    (with-open-file (str "parsed_lisp.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
            (format str myString))
	    (setq counter4 (+ counter4 1))
	    (when (= counter4 (length outputList))(return outputList))
	)

)												;(setf totalList  (append totalList (list (parse-integer myString))) ) 

;(setq indexFile (elt *args* 0))
(start 1)

