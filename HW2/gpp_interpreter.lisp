(setf outputList nil)
(setf lexertokens nil)
(setf tokens nil)
(setf paranthesis nil)
(setf elementCounter 0)
(setf parserErrorFlag 0)

;;--------------------------------------------------------------PARSER------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------PARSER------------------------------------------------------------------------------------------------------

(defun parser(parseList)
    
	(string-to-list lexertokens)
	;(paranthesisCounter tokens)

	
	(let ((result 0)(errorr 0)(firstValForSub 0)(firstValForDiv 0)(resultformult 1)(logicResult t)(returnFromBool t))
	;;---------------------------------------------------------------------------;;(+ num num)
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt tokens (+ 1 elementCounter)) "+"))  	
		(progn 
		    (setq elementCounter (+ 2 elementCounter))
			;;(print (elt outputList elementCounter))
			(loop
				(when (string= (elt tokens elementCounter) ")" )					
					(setq errorr 0)
					(return-from parser result)
				)
				(when (string= (elt tokens elementCounter) "(" )
					(setq errorr 0)
					(setq result  (+ result (parser elementCounter)))				
				)
				(when (string= (elt outputList elementCounter) "VALUE")
					(setq result (+ result (parse-integer (elt tokens elementCounter)))) 
					(setq errorr 0)
				)
				(when (= errorr 1)
					(setq parserErrorFlag 1)
					(return-from parser 99)
				)
				(setq errorr 1)
				(setq elementCounter (+ 1 elementCounter))
				(if (= elementCounter (length tokens)) ;denemeydi
					(progn
						(setq parserErrorFlag 1)
						(return-from parser result)
					)
				)	
			)
		)
	)
	;;--------------------------------------------------------------------------;;(- num num)
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt tokens (+ 1 elementCounter)) "-"))  
	
		(progn 
		    (setq elementCounter (+ 2 elementCounter))
			;;(print (elt outputList elementCounter))
			(loop
				(when (string= (elt tokens elementCounter) ")" )	
					(setq error 0)
					(return-from parser result)
				)
				(when (string= (elt tokens elementCounter) "(" )
					(setq errorr 0)
					(setq  res (parser elementCounter))
					(if (= firstValForSub 0)
						(setq result (+ res result))
						(setq result (- result res))
					)
					(setq firstValForSub 1)
				)
				(when (string= (elt outputList elementCounter) "VALUE")
					(setq res  (parse-integer (elt tokens elementCounter)))
					(setq errorr 0)

					(if (= firstValForSub 0)
						(setq result (+ res result))
						(setq result (- result res))
					)
					(setq firstValForSub 1)
			
				)
				(when (= errorr 1)
					(setq parserErrorFlag 1)
					(return-from parser 99)
				)
				(setq errorr 1)
                
				(setq elementCounter (+ 1 elementCounter))
				(when (= elementCounter  (length tokens)) (return result))
			)
		
		
		)
		
	)
	;;--------------------------------------------------------------------------;;(* num num)
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt tokens (+ 1 elementCounter)) "*"))  
	
		(progn 
		    (setq elementCounter (+ 2 elementCounter))
			;;(print (elt outputList elementCounter))
			(loop
				(when (string= (elt tokens elementCounter) ")" )
					
					(setq errorr 0)
					(return-from parser resultformult)
				)
				(when (string= (elt tokens elementCounter) "(" )
					(setq errorr 0)
					(setq resultformult  (* resultformult (parser elementCounter)))
					
				)
				
				(when (string= (elt outputList elementCounter) "VALUE")
					(setq resultformult (* resultformult (parse-integer (elt tokens elementCounter)))) 
					(setq errorr 0)
			
				)
				(when (= errorr 1)
					(setq parserErrorFlag 1)
					(return-from parser 99)
				)
				(setq errorr 1)
                
				(setq elementCounter (+ 1 elementCounter))
				
			)
		
		
		)
		
	)
	;;--------------------------------------------------------------------------;;(/ num num)
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt tokens (+ 1 elementCounter)) "/")) 
	
		(progn 
		    (setq elementCounter (+ 2 elementCounter))
			;;(print (elt outputList elementCounter))
			(loop
				(when (string= (elt tokens elementCounter) ")" )	
					(setq error 0)
					(return-from parser resultformult)
					
				)
				(when (string= (elt tokens elementCounter) "(" )
					(setq errorr 0)
					(setq  res (parser elementCounter))
					(if (= firstValForDiv 0)
						(setq resultformult (* res resultformult))
						(setq resultformult (/ resultformult res))
					)
					(setq firstValForDiv 1)
				)
				(when (string= (elt outputList elementCounter) "VALUE")
					(setq res  (parse-integer (elt tokens elementCounter)))
					(setq errorr 0)

					(if (= firstValForDiv 0)
						(setq resultformult (* res resultformult))
						(setq resultformult (/ resultformult res))
					)
					(setq firstValForDiv 1)
			
				)
				(when (= errorr 1)
					(setq parserErrorFlag 1)
					(return-from parser 99)
				)
				(setq errorr 1)
                
				(setq elementCounter (+ 1 elementCounter))
				(when (= elementCounter  (length tokens)) (return result))
			)
		
		
		)
		
	)
	;;--------------------------------------------------------------------------;; ‘( VALUES ) ---> List Döndürür      
    (when (string= (elt tokens elementCounter) "'(" )   
  		(progn
			
			(let ((tempList (list )))
				(setq elementCounter (+ 1 elementCounter))
				(loop
					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(return-from parser tempList)
					)
					(when (string= (elt outputList elementCounter) "VALUE" )
						(setq tempList (append tempList (list (parse-integer (elt tokens elementCounter))))) 
						(setq errorr 0)
						(setq errorr 0)
					)
					(when (string= (elt outputList elementCounter) "IDENTIFIER" )
						(setq tempList (append tempList (list (string (elt tokens elementCounter))))) 
						(setq errorr 0)
						(setq errorr 0)
					)
					(when (= errorr 1)
						(setq parserErrorFlag 1)
						(return-from parser 99)
					)
					(setq errorr 1)
                
					(setq elementCounter (+ 1 elementCounter))
				
				)

				(setq result tempList)

			)

			(return-from parser result)
		)            
    )	
	;;--------------------------------------------------------------------------;;(IDENTIFIER (List abc)) ||(IDENTIFIER '(A B C))
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt outputList (+ 1 elementCounter))  "IDENTIFIER"))
	 	(progn
			(let ((tempList (list ))(identifier 0))
				(setq identifier (elt tokens (+ 1 elementCounter) ))
				(setq elementCounter (+ 2 elementCounter))
				(loop
					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(return-from parser result)
								
					)
					(when (or (string= (elt tokens elementCounter) "(")(string= (elt tokens elementCounter) "'("))
						(setq result (parser elementCounter))
						(setq errorr 0)
						(when (typep result 'CONS)
	        				(terpri)
            				(format t "~a :~a "  identifier result)                 
            			)
						(when  (not (typep result 'CONS))
    	                	( return-from parser "SYNTAX ERROR.")               
                        )
					
					)

					(when (= errorr 1)
						(setq parserErrorFlag 1)
						(return-from parser 99)
					)
					(setq errorr 1)
                
					(setq elementCounter (+ 1 elementCounter))
				
				)
			)
			
			(return-from parser result)
		)
	)
	;;-------------------------------------------------------------------------;;AND
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt tokens (+ 1 elementCounter)) "and" ))

		(progn
		
		    (setq elementCounter (+ 2 elementCounter))
			(loop 
				(when (string= (elt tokens elementCounter) ")" )
					(setq errorr 0)
					(return-from parser logicResult)
				)
				(when (string= (elt tokens elementCounter) "true" )
					(setq logicResult (and logicResult t))
					(setq errorr 0)
				)
				(when (string= (elt tokens elementCounter) "false" )
					(setq logicResult (and logicResult nil))
					(setq errorr 0)
				)
				(when (string= (elt tokens elementCounter) "(" )
				            (setq returnFromBool (parser elementCounter))
							(when (typep returnFromBool 'BOOLEAN)
                                (setq logicResult (and logicResult returnFromBool))
								(setq errorr 0)
                            )
				)
				(when (= errorr 1)
					(setq parserErrorFlag 1)
					(return-from parser 99)
				)
				(setq errorr 1)
                
				(setq elementCounter (+ 1 elementCounter))			
			)
		
		)

	
	)
	;;-------------------------------------------------------------------------;;OR
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt tokens (+ 1 elementCounter)) "or" ))

		(progn
			
		    (setq elementCounter (+ 2 elementCounter))
			(loop 
				(when (string= (elt tokens elementCounter) ")" )
					(setq errorr 0)
					(return-from parser logicResult)
				)
				(when (string= (elt tokens elementCounter) "true" )
					(setq logicResult (or logicResult t))
					(setq errorr 0)
				)
				(when (string= (elt tokens elementCounter) "false" )
					(setq logicResult (or logicResult nil))
					(setq errorr 0)
				)
				(when (string= (elt tokens elementCounter) "(" )
				            (setq returnFromBool (parser elementCounter))
							(when (typep returnFromBool 'BOOLEAN)
                                (setq logicResult (or logicResult returnFromBool))
								(setq errorr 0)
                            )
				)
				(when (= errorr 1)
					(setq parserErrorFlag 1)
					(return-from parser 99)
				)
				(setq errorr 1)
                
				(setq elementCounter (+ 1 elementCounter))			
			)
		
		)

	
	)
	;;-------------------------------------------------------------------------;;NOT
	(if (and (string= (elt tokens elementCounter) "(" ) (string= (elt tokens (+ 1 elementCounter)) "not" ))

		(progn
			
		    (setq elementCounter (+ 2 elementCounter))
			(loop 
				(when (string= (elt tokens elementCounter) ")" )
					(setq errorr 0)
					(return-from parser logicResult)
				)
				(when (string= (elt tokens elementCounter) "true" )
					(setq logicResult nil)
					(setq errorr 0)
				)
				(when (string= (elt tokens elementCounter) "false" )
					(setq logicResult t)
					(setq errorr 0)
				)
				(when (string= (elt tokens elementCounter) "(" )
				            (setq returnFromBool (parser elementCounter))
							(when (typep returnFromBool 'BOOLEAN)
                                (setq logicResult (not returnFromBool))
								(setq errorr 0)
                            )
				)
				(when (= errorr 1)
					(setq parserErrorFlag 1)
					(return-from parser 99)
				)
				(setq errorr 1)
                
				(setq elementCounter (+ 1 elementCounter))			
			)
		
		)

	
	)
	;;-------------------------------------------------------------------------;;(LIST A B C)
	(if (and (string= (elt tokens elementCounter) "(" )(string= (elt outputList (+ 1 elementCounter))  "KW_LIST") )
		(progn
			(let ((tempList (list )))
				(setq elementCounter (+ 2 elementCounter))
				(loop
					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(return-from parser tempList)
					)
					(when (string= (elt outputList elementCounter) "VALUE" )
						(setq tempList (append tempList (list (parse-integer (elt tokens elementCounter))))) 
						(setq errorr 0)
						(setq errorr 0)
					)
					(when (string= (elt outputList elementCounter) "IDENTIFIER" )
						(setq tempList (append tempList (list (string (elt tokens elementCounter))))) 
						(setq errorr 0)
						(setq errorr 0)
					)
					(when (= errorr 1)
						(setq parserErrorFlag 1)
						(return-from parser 99)
					)
					(setq errorr 1)               
					(setq elementCounter (+ 1 elementCounter))
				)
				(setq result tempList)
			)
			(return-from parser result)
		)
	
	
	
	)

	;;--------------------------------------------------------------------------;;(concat list list)
	(if (and (string= (elt tokens elementCounter) "(" )(string= (elt outputList (+ 1 elementCounter))  "KW_CONCAT") )
		(progn
			(let ((tempList (list ))(res1 nil))
				(setq elementCounter (+ 2 elementCounter))
				(loop

					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(setq result tempList)
						(return-from parser result)
					)
					
					(when (string= (elt tokens elementCounter) "'(" )
				            (setq  res1 (parser elementCounter))
							(when (typep res1 'CONS)
                                (Setq tempList ( concatenate 'list tempList  res1))
								(setq errorr 0)
                            )
							
					)
					(when (= errorr 1)
						(setq parserErrorFlag 1)
						(return-from parser 99)
					)
					(setq errorr 1)
                
					(setq elementCounter (+ 1 elementCounter))
					
				)
				(setq result tempList)
				 

			)
			(return-from parser result)
		)
	)
	;;----------------------------------------------------------------------;;(append val list)
	(if (and (string= (elt tokens elementCounter) "(" )(string= (elt outputList (+ 1 elementCounter))  "KW_APPEND") )
		(progn
			(let ((tempList (list ))(res1 nil)(sum 0)(flag1 0)(flag2 0))
				(setq elementCounter (+ 2 elementCounter))
				(loop

					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(setq result tempList)
						(if (and (= flag1 1)(= flag2 1))
							(setq result tempList)
							(setq result "SYNTAX ERROR")
						)
						(return-from parser result)
					)
					(when (string= (elt tokens elementCounter) "(" )
						(setq  res1 (parser elementCounter))
						(when (typep res1 'CONS)
                            (setq tempList ( concatenate 'list tempList  res1))
							(setq errorr 0)
							(setq flag1 1)
                        )
					)
					(when (string= (elt tokens elementCounter) "'(" )
				            (setq  res1 (parser elementCounter))
							(when (typep res1 'CONS)
                                (setq tempList ( concatenate 'list tempList  res1))
								(setq errorr 0)
								(setq flag1 1)
                            )
							
					)
					(when (string= (elt outputList elementCounter) "VALUE") 
						(setq sum  (parse-integer (elt tokens elementCounter)))
						(setq tempList (append tempList (list sum)))
						(setq errorr 0)
						(setq flag2 1)					
					)
					(when (string= (elt outputList elementCounter) "IDENTIFIER") 
						(setq sum  (elt tokens elementCounter))
						(setq tempList (append tempList (list sum)))
						(setq errorr 0)
						(setq flag2 1)					
					)
					(when (= errorr 1)
						(setq parserErrorFlag 1)
						(return-from parser 99)
					)
					(setq errorr 1)
                
					(setq elementCounter (+ 1 elementCounter))
				
				
				
				)
				(if (and (= flag1 1)(= flag2 1))
					(setq result tempList)
					(setq result "SYNTAX ERROR")
				)			 

			)
			(return-from parser result)
		)	

	)
	;;---------------------------------------------------------------------------;;(disp )
	(if (and (string= (elt tokens elementCounter) "(")(string= (elt outputList (+ 1 elementCounter)) "KW_DISP"))
		(progn 
			(let ((resultString (list )))
				(setq elementCounter (+ 2 elementCounter))
				(loop	
					(setq resultString (concatenate 'string resultString  (elt tokens elementCounter)))
					(setq elementCounter (+ 1 elementCounter))
					(when (= elementCounter (- (length  tokens) 1 ))(return resultString))
				)
				(setq result resultString)
				(print resultString)
				(return-from parser result)
			
			
			)
		)
		
		
	)	
	;;----------------------------------------------------------------------------;;(exit)
	(if (and (string= (elt tokens elementCounter) "(") (string= (elt outputList (+ 1 elementCounter)) "KW_EXIT")(string= (elt tokens (+ 2 elementCounter)) ")") )    
            (return-from parser "EXIT IS ENTERED,PROGRAM IS TERMINATING")       
    )
	;;----------------------------------------------------------------------------;; (set,defvar Value) || (set,defvar (..))  
	(if (and (string= (elt tokens elementCounter) "(") (or (string= (elt outputList (+ 1 elementCounter)) "KW_SET")(string= (elt outputList (+ 1 elementCounter)) "KW_DEFVAR")) (string= (elt outputList (+ 2 elementCounter)) "IDENTIFIER")) 
		(progn
			(let ((tempList (list ))(res1 nil)(sum 0)(identifier 0))
				(setq identifier (elt tokens (+ 2 elementCounter) ))
				(setq elementCounter (+ 3 elementCounter))
				(loop
					(when (and (not (equal sum 2))(string= (elt tokens elementCounter) ")" ))
						(print "")
						(format t "~a :~d "  identifier result)
						(setq errorr 0)
						(return-from parser result)
					)
					(when (string= (elt tokens elementCounter) "(" )
						(setq errorr 0)
						(setq sum (+ sum 1))
						(setq res1 (parser elementCounter))
						(when ( not  (typep res1 'integer))
                            
                            (return-from parser "SYNTAX ERROR")
                        )
                        (when (and (typep res1 'integer)(not (equal sum 2)))
                           (setq result res1)

                        )
						
					)

					
					(when (string= (elt outputList elementCounter) "VALUE")
						(setq sum (+ sum 1))
						(setq result  (parse-integer (elt tokens elementCounter)))				
					)
					(when (= errorr 1)
						(setq parserErrorFlag 1)
						(return-from parser "SYNTAX ERROR")
					)
					(setq errorr 1)
					(when (not (equal sum 1)) (return-from parser "SYNTAX ERROR"))
					(setq elementCounter (+ 1 elementCounter))

				
				
				)

			)
		
		)
	
	)
	;;----------------------------------------------------------------------------;; (if expb explist)
	(if (and (string= (elt tokens elementCounter) "(") (string= (elt outputList (+ 1 elementCounter)) "KW_IF"))
		(progn
			(let ((flag nil)(tempResult 0)(selection 0))
				(setq elementCounter (+ 2 elementCounter))
				(if (string= (elt outputList elementCounter) "KW_TRUE")
					(setq selection 1)
				)
				(if (string= (elt outputList elementCounter) "KW_FALSE")
					(setq selection 2)
				)
				(when (string= (elt tokens elementCounter) "(" )
						(setq tempResult (parser elementCounter))
						(setq errorr 0)
						(when (not  (typep tempResult 'boolean))
                        	(return-from parser "SYNTAX ERROR")
                        )
						(if (equal tempResult T)
							(setq selection 1)
							(setq selection 2) 
						)
				)

				(if (= selection 1)
					(progn	
						(setq elementCounter (+ 1 elementCounter))
						
						(loop 
							(when (string= (elt tokens elementCounter) ")")
								(setq errorr 0)
								(setq result tempList)							
								(return-from parser result)
							)
							(when (string= (elt tokens elementCounter) "(")
								(setq errorr 0)
								(setq tempList (parser elementCounter))
								(when (not (typep tempList 'CONS))								
									(return-from parser "SYNTAX ERROR")
								)
								(setq elementCounter (- elementCounter 1))
							)
							(when (string= (elt tokens elementCounter) "'(")
								(setq tempList (parser elementCounter))
								(setq errorr 0)
								(setq elementCounter (- elementCounter 1))
							)
							(when (= errorr 1)
								
								(return-from parser "SYNTAX ERROR")
							)
							(setq errorr 1)
							(setq elementCounter (+ 1 elementCounter))
						)
					)
				);if end
				(if (= selection 2)
					(progn
						(let ((opened 0)(counter6 0)) 
							(loop
								(setq elementCounter (+ 1 elementCounter))
								(if (and (or (string= (elt tokens elementCounter) "'(")(string= (elt tokens elementCounter) "("))(= opened 0))
									(progn
										(setq opened 1)
										(setq counter6 (+ 1 counter6))
									)
								)
								(if (and (string= (elt tokens elementCounter) ")")(= opened 1))
									(setq opened 0)
								)
								(if (= counter6 2)
									(return)
								)
							)	
						)
						(loop 
							(when (string= (elt tokens elementCounter) ")")
								(setq errorr 0)
								
								(setq result tempList)							
								(return-from parser result)
							)
							(when (string= (elt tokens elementCounter) "(")
								(setq errorr 0)
								
								(setq tempList (parser elementCounter))
								(when (not (typep tempList 'CONS))								
									(return-from parser "SYNTAX ERROR")
								)
							)
							(when (string= (elt tokens elementCounter) "'(")
								(setq tempList (parser elementCounter))
								(setq errorr 0)
							)
							(when (= errorr 1)
								
								(return-from parser "SYNTAX ERROR")
							)
							(setq errorr 1)
							(setq elementCounter (+ 1 elementCounter))
						)					
					)
				)
				
				(if (equal flag nil)
					(return-from parser "SYNTAX ERROR")
				)				
			);let end
			(return-from parser "SYNTAX ERROR")
		);progn end       
    )
	;;----------------------------------------------------------------------------;; (for (id exp exp) explist)
	(if (and (string= (elt tokens elementCounter) "(")(string= (elt outputList (+ 1 elementCounter)) "KW_FOR")(string= (elt tokens (+ 2 elementCounter)) "(")(string= (elt outputList (+ 3 elementCounter)) "IDENTIFIER")) 
        (progn
		 	(let ((tempResult 0)(tempList 0))
			 	(setq elementCounter (+ 4 elementCounter))			
				(loop
					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(setq elementCounter (+ 1 elementCounter))
						(return tempResult)
						
					)
					(when (string= (elt tokens elementCounter) "(" )
						(setq tempResult (parser elementCounter))
						(setq errorr 0)
						(when (not  (integerp tempResult))
                        	(return-from parser "SYNTAX ERROR")
                        )
					)
					(when (string= (elt outputList elementCounter) "VALUE")
						(setq tempResult (parse-integer (elt tokens elementCounter)))
						(setq errorr 0)
					)
					(when (= errorr 1)
						(return-from parser "SYNTAX ERROR")
					)
					(setq errorr 1)

					(setq elementCounter (+ 1 elementCounter))
				)

				(loop
					(when (string= (elt tokens elementCounter) ")")
						(setq errorr 0)
						(setq result tempList)
						(return-from parser result)
					)
					(when (string= (elt tokens elementCounter) "(")
						(setq errorr 0)
						(setq tempList (parser elementCounter))
						(when (not (typep tempList 'CONS))
                            (return-from parser "SYNTAX ERROR")
                        )
					)
					(when (string= (elt tokens elementCounter) "'(")
						
						(setq tempList (parser elementCounter))
						(setq errorr 0)
					)
					(when (= errorr 1)
						(return-from parser "SYNTAX ERROR")
					)
					(setq errorr 1)

					(setq elementCounter (+ 1 elementCounter))
				
				)
			)
			(return-from parser result)
		)
    )
	;;----------------------------------------------------------------------------;; (equal expi expi) || (equal expb expb)
	(if (and (string= (elt tokens elementCounter) "(") (string= (elt outputList (+ 1 elementCounter)) "KW_EQUAL"))
		(progn
			(let ((boolCounter 0)(valCounter 0)(exprCounter 0)(valbool1 nil)(valbool2 nil)(val1 0)(val2 0)(boolflag 0)(valFlag 0)(returnFlag 0))
			  (setq elementCounter (+ 2 elementCounter))
			  (loop	
				(when (and (or (string= (elt outputList elementCounter) "VALUE") (string= (elt outputList elementCounter) "KW_TRUE") (string= (elt outputList elementCounter) "KW_FALSE"))) 
					(when (string= (elt outputList elementCounter) "KW_TRUE")
						(if (= boolCounter 0)
							(setq valbool1 t)
							(setq valbool2 t)
						)
						
						(setq boolCounter (+ 1 boolCounter))
						(setq boolflag 1)
					)
					(when (string= (elt outputList elementCounter) "KW_FALSE")
						(if (= boolCounter 0)
							(setq valbool1 nil)
							(setq valbool2 nil)
						)
						(setq boolCounter (+ 1 boolCounter))
						(setq boolflag 1)
						
					)
					(when (string= (elt outputList elementCounter) "VALUE")
						(if (= valCounter 0)
							(setq val1 (parse-integer (elt tokens elementCounter)))
							(setq val2 (parse-integer (elt tokens elementCounter)))
						)
						(setq valCounter (+ 1 valCounter))
						(setq valFlag 1)
						
					)


				)
				(when (string= (elt tokens elementCounter) ")")
					
					(when (or (equal valCounter 2)(equal boolCounter 2))
						(if (equal boolCounter 2)
							(setq result (equal valbool1 valbool2))
							(setq result (equal val1 val2))
						)
						
						(return-from parser result)
					)
					(return-from parser "SYNTAX ERROR")
				)
				(when (string= (elt tokens elementCounter) "(")
					(setq tempp (parser elementCounter))
					(when (integerp tempp)
						(if (= valCounter 0)
							(setq val1 tempp)
							(setq val2 tempp)
						)
						(setq valCounter (+ valCounter 1))
						(setq valFlag 1)
						(setq returnFlag 1)
					)
					(when (typep tempp 'BOOLEAN)
                        (if (= boolCounter 0)
							(setq valbool1 tempp)
							(setq valbool2 tempp)
						)
						(setq boolCounter (+ boolCounter 1))
						(setq boolFlag 1)
						(setq returnFlag 1)
                    )
					(when (= returnFlag 0)
						(return-from parser "SYNTAX ERROR")
					)
				)
				

				(setq elementCounter (+ 1 elementCounter))

			  )				
			)
		)
	
	)
	;;----------------------------------------------------------------------------;; (less expi expi)
	(if (and (string= (elt tokens elementCounter) "(") (string= (elt outputList (+ 1 elementCounter)) "KW_LESS"))
		(progn
			(let ((boolCounter 0)(valCounter 0)(exprCounter 0)(valbool1 nil)(valbool2 nil)(val1 0)(val2 0)(boolflag 0)(valFlag 0)(returnFlag 0))
				(setq elementCounter (+ 2 elementCounter))
				(loop	
					(when (string= (elt outputList elementCounter) "VALUE")
						
						(if (= valCounter 0)
							(setq val1 (parse-integer (elt tokens elementCounter)))
							(setq val2 (parse-integer (elt tokens elementCounter)))
						)
						(setq valFlag 1)
						(setq valCounter (+ 1 valCounter))

					)
					(when (string= (elt tokens elementCounter) ")")
						(if (= 2 valCounter)
							(setq result (< val1 val2))
							(return-from parser "SYNTAX ERROR")
						)
						(return-from parser result)
					)
					(when (string= (elt tokens elementCounter) "(")
						(setq tempp (parser elementCounter))
						(when (integerp tempp)
							(if (= valCounter 0)
								(setq val1 tempp)
								(setq val2 tempp)
							)
							(setq valCounter (+ valCounter 1))
							(setq valFlag 1)
							(setq returnFlag 1)
						)
						(when (= returnFlag 0)
							(return-from parser "SYNTAX ERROR")
						)	

					)

					(setq elementCounter (+ 1 elementCounter))
				
							
				)
			)
	
		)
	)
	;;----------------------------------------------------------------------------;; (While expb explist)
	(if (and (string= (elt tokens elementCounter) "(")(string= (elt outputList (+ 1 elementCounter)) "KW_WHILE")(string= (elt tokens (+ 2 elementCounter)) "(")) 
        (progn
		 	(let ((tempResult 0)(tempList 0))
			 	(setq elementCounter (+ 3 elementCounter))			
				(loop
					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(setq elementCounter (+ 1 elementCounter))
						(return tempResult)
						
					)
					(when (string= (elt tokens elementCounter) "(" )
						(setq tempResult (parser elementCounter))
						(setq errorr 0)
						(when (not  (typep tempResult 'boolean))
                        	(return-from parser "SYNTAX ERROR")
                        )
					)
					(if (or (string= (elt outputList elementCounter) "KW_TRUE")(string= (elt outputList elementCounter) "KW_FALSE"))
						(progn
							(when (string= (elt outputList elementCounter) "KW_TRUE")
								(setq tempResult t)
								(setq errorr 0)
							)
							(when (string= (elt outputList elementCounter) "KW_FALSE")
								(setq tempResult nil)
								(setq errorr 0)
							)
						
						)
						(if (string= (elt tokens (- elementCounter 1)) "(" )
							(progn
								
								(setq elementCounter (- elementCounter 1))
								(setq errorr 0)
								(setq tempResult (parser elementCounter))
								(print tempResult)
								(when (not  (or (equal tempResult T)(equal tempResult nil)))
									(return-from parser "SYNTAX ERROR")
								)
								(setq elementCounter (+ elementCounter 1))
								(return) 
							)	
						)
					)
						
					(when (= errorr 1)	
						(return-from parser "SYNTAX ERROR")
					)
					(setq errorr 1)
					(setq elementCounter (+ 1 elementCounter))
				)
				
				(if (equal tempResult T)
					(loop

						(when (string= (elt tokens elementCounter) ")")
							(setq errorr 0)
							(setq result tempList)
							
							(return-from parser result)
						)
						(when (string= (elt tokens elementCounter) "(")
							(setq errorr 0)
							(setq tempList (parser elementCounter))
							(when (not (typep tempList 'CONS))
								
								(return-from parser "SYNTAX ERROR")
							)
						)
						(when (string= (elt tokens elementCounter) "'(")
							
							(setq tempList (parser elementCounter))
							(setq errorr 0)
						)
						(when (= errorr 1)
							
							(return-from parser "SYNTAX ERROR")
						)
						(setq errorr 1)

						(setq elementCounter (+ 1 elementCounter))
					
					)
					(progn
						(print "SYNTAX ERROR")
					)
				)	
			)
			
			(return-from parser result)
		)
    )	
	;;----------------------------------------------------------------------------;; (deffun Id IDLIST EXPLISTI)
	(if (and (string= (elt tokens elementCounter) "(")(string= (elt outputList (+ 1 elementCounter)) "KW_DEFFUN")(string= (elt outputList (+ 2 elementCounter)) "IDENTIFIER")(string= (elt tokens (+ 3 elementCounter)) "(")) 
       (progn
		 	(let ((tempResult 0)(tempList 0)(idList (list ))(identt 0))
			 	(setq identt   (nth (+ elementCounter 2) tokens)  )
				
			 	(setq elementCounter (+ 4 elementCounter))			
				(loop
					(when (string= (elt tokens elementCounter) ")" )
						(setq errorr 0)
						(setq elementCounter (+ 1 elementCounter))
						(return idList)
				
					)	
					(when (string= (elt outputList elementCounter) "IDENTIFIER" )
						(setq errorr 0)
						(setq idList (append idList (list (nth elementCounter tokens))))
						
				
					)					
					(when (= errorr 1)	
						(return-from parser "SYNTAX ERROR")
					)
					(setq errorr 1)
					(setq elementCounter (+ 1 elementCounter))
				)
				(loop
						(when (string= (elt tokens elementCounter) ")")
							(setq errorr 0)
							(setq result tempList)
							
							(return)
						)
						(when (string= (elt tokens elementCounter) "(")
							(setq errorr 0)
							(setq tempList (parser elementCounter))
							(when (not (typep tempList 'CONS))	
								(return-from parser "SYNTAX ERROR")
							)
						)
						(when (string= (elt tokens elementCounter) "'(")
							
							(setq tempList (parser elementCounter))
							(setq errorr 0)
							(when (not (typep tempList 'CONS))	
								(return-from parser "SYNTAX ERROR")
							)
						)
						(when (= errorr 1)
							
							(return-from parser "SYNTAX ERROR")
						)
						(setq errorr 1)

						(setq elementCounter (+ 1 elementCounter))
				)
				(setq result identt)
				
			)
			
			
			(return-from parser result)
		) 
    )
	;;----------------------------------------------------------------------------;; (load "ıd")
	(if (and (string= (elt outputList elementCounter) "OP_OP")(string= (elt outputList (+ 1 elementCounter)) "KW_LOAD")(string= (elt outputList (+ 2 elementCounter)) "OP_OC")(string= (elt outputList (+ 3 elementCounter)) "IDENTIFIER")(string= (elt outputList (+ 4 elementCounter)) "OP_CC")(string= (elt outputList (+ 5 elementCounter)) "OP_CP")) 
		(progn
			(let (resultStr(list ))
				(setq sonuc (elt tokens 2))
				(setq tot (- (length sonuc) 2))
			
				(loop for i from 1 to tot
					do (setq resultStr (concatenate 'string resultStr (list (char sonuc i))))
				)
				(setq result resultStr)
				(return-from parser result)
			)
		)
        
    )
	;;----------------------------------------------------------------------------;; (comment)	
	(if  (string= (elt outputList elementCounter) "COMMENT")
		(progn
			(Setq result t)
			(return-from parser result)
		)
        
    )	
	
	(setq result "SYNTAX ERROR")
	(return-from parser result) ;;hicbir seye girmezse error donmuyor
	
	)
)

(defun paranthesisCounter (input)
	(setq i 0)
	(setq opened 0)
	(setq closed 0)	
	(loop 
		(if (string= (elt input i) "(" )  
			(setq opened (+ 1 opened))
		)
		(if (string= (elt input i) ")" )  
			(setq closed (+ 1 closed))
		)
		(setq i (+ 1 i))
	 (when (= i (length input ) ) (return input))
	)

	(setq parserErrorFlag (- opened closed))
	(return-from paranthesisCounter parserErrorFlag)

)
(defun string-to-list (str)

		(setq counter 0)
		(setq temp nil)
		(loop 
		   (if (< counter (length str))
		    (setq nowchar (char str counter))
		   )
			(if (or (equal nowchar #\Space )(= counter (length str) ))
				(progn ;;TRUE
				 (if (string/= temp nil)
				 	(setq tokens (append tokens (list temp)))
				 )
				 (setq temp nil)
				)
				(progn ;;False
				    (if (or (equal nowchar #\( )(equal nowchar #\)))
					 (progn
					   (if (string= temp nil)
					   	  (progn
						    (setq temp (concatenate 'string temp (list nowchar)))
							(setq tokens (append tokens (list temp)))
							(setq temp nil)	
						  )
						  (progn 
						  	(if (and (not (equal temp nil))(string= (elt temp 0) "'"))

								(progn 
									(setq temp (concatenate 'string temp (list nowchar)))
									(setq tokens (append tokens (list temp)))
									(setq temp nil)								
								
								)
								(progn 
									(setq tokens (append tokens (list temp)))
									(setq temp nil)
									(setq temp (concatenate 'string temp (list nowchar)))
									(setq tokens (append tokens (list temp)))
									(setq temp nil)
								)
							  
							)
						  )  
				       )
					 ) 
					 (setq temp (concatenate 'string temp (list nowchar)))
					)
					
				)
			)
			(setq counter (+ counter 1))
			(when (> counter (length str))(return str) )
		)
	
	)
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------



;;-------------------------------------------------LEXER-------------------------------------------------------------------------------------------------------------------------
;;-------------------------------------------------LEXER---------------------------------------------------------------------------------------------------------------------


(defun isDigit (nextChar)  ;;DETERMINE FUNCTIONS OF DIGIT OR LETTER
	(setq digitList '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	(if (not (equal (position nextChar digitList) nil))
		(return-from isDigit t)
		(return-from isDigit nil)
	)		
)
(defun isLetter(nextChar)
  (setq letterList '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t  #\u  #\v  #\w  #\x #\y #\z))
	(if (not (equal (position nextChar letterList) nil))
		(return-from isLetter t)
		(return-from isLetter nil)
	)  
)

(defun findkwList(str)
	  (setq kwList '( "and"  "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "while" "defvar"))
	  (setq counter2 0)
	  (setq flag 0)
	  (loop
	  	 (if (string= str (elt kwList counter2)) 
	  	 	(setq flag 1)
	  	 )
	  	(when (or (= flag 1)(= counter2 19))(return str))
	  	(setq counter2 (+ counter2 1))
	  )

	  (if (= flag 1)
	  	(return-from findkwList counter2)
	  	(return-from findkwList 99)
	  )
)
(defun findopList(str)
	(setq opList '(#\+ #\- #\/ #\*  #\( #\) "**" #\" #\, "'("))
	 (setq counter1 0)
	  (setq flag 0)
	  (loop
	  	 (if (string= str (elt opList counter1)) 
	  	 	(setq flag 1)
	  	 )
	  	(when (or (= flag 1)(= counter1 9))(return str))
	  	(setq counter1 (+ counter1 1))
	  )

	  (if (= flag 1)
	  	(return-from findopList counter1)
	  	(return-from findopList 99)
	  )
)

(defun controlCID(listStr)  ;;COMMENT,IDENT,DIGIT KONTROL
 (setq errCheck 0)
 (setq dig 0)
 (setq ident 0)
 (setq commentControl 0)
 (setq lstCounter 0)
  (if (isDigit (char listStr 0))
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
  	 (if (isLetter (char listStr 0))
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

  (if (and (= errCheck 0)(= dig 1))
  	(setf outputList  (append outputList (list "VALUE") )) 
  	(if (and (= errCheck 0)(= ident 1))
  		(setf outputList  (append outputList (list "IDENTIFIER") ))
  	)
  )

  (if (= errCheck 1)
  	(setf outputList  (append outputList (list "ERROR") ))
  )

  (if (= 1 commentControl)
  	(setf outputList  (append outputList (list "COMMENT") ))
  )

  (if (= 1 commentControl)
  	(return-from controlCID 110)
  	(return-from controlCID 55)
  )



)

(defun lexer(mergeList)      ;;STRING PARCALARININ NE OLDUGUNU ANLAMA
  

  (setq tokenOpList '("OP_PLUS" "OP_MINUS"  "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_OC" "OP_COMMA" "'OP_OP") )
  (setq kwTokenList '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" 
  					  "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_WHILE" "KW_DEFVAR"))
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
  	(if (not (= 99 (findopList (elt mergeList counter3)) ))
  		(if (string= (elt tokenOpList (findopList (elt mergeList counter3))) "OP_OC")
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
  		(if (not (= 99 (findkwList (elt mergeList counter3))))
  			(setf outputList  (append outputList (list (elt kwTokenList (findkwList (elt mergeList counter3)))) ));(print (elt kwTokenList (findkwList (elt mergeList counter3))))
  			(setq control 1)
  		)
  	)

  	(if (= control 1)
  		(setq controlcıdRes (controlCID (elt mergeList counter3))))
  	(if (= control 1)
  		(setq control 0))

  	(if (= 110 controlcıdRes)
  		(return-from lexer 999)
  	)

  	(setq counter3 (+ 1 counter3))
  	(when (= counter3 (length mergeList))(return mergeList))
  )



)

(defun readFromUser(inputString)   ;;KODLARI PARCALAMA
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
       (if (isLetter nextChar) 
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
      		(if (or (char= #\Space nextChar)(char= #\tab nextChar))
      			()
      			(setq lexeme (concatenate 'string lexeme (list nextChar)))
      		)
			(setq counter (+ counter 1))
			(if (< counter (length inputString))
			 (when (and (char= #\' nextChar)(char= (char inputString counter) #\(  ))
			 	(setq nextChar (char inputString counter))
				(setq lexeme (concatenate 'string lexeme (list nextChar)))
				(setq counter (+ counter 1))
			 )
			
			)
      		
      		
      		(if (<= counter (- (length inputString) 1) )
        	      	(setq nextChar (char inputString counter))
        	      	(setq nextChar #\Space)
            )
            (if (< counter (length inputString) )
     		  (when (not (or (and (char= nextChar #\*)(char= (char inputString counter) #\*)) ))(return  inputString))
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
    (if (< 0 (length lexeme))
    	(setq mergeList (append mergeList (list lexeme)))
    )
    
   (when (>= counter  (length inputString)  )(return inputString))
   )
;  (print "Merge List")
;  (print mergeList)
  (lexer mergeList)
;  (print outputList)
;  (print lexertokens)
  (setq parserErrorFlag (paranthesisCounter mergeList))
  (if (= parserErrorFlag 0) 
	(progn
		(setq a (parser 0))
		(if (= parserErrorFlag 0)
				(progn	
					(print "SYNTAX OK.")
					(print "Result : ")
					(print a)
				)
				(progn
					(print "SYNTAX ERROR")
					(print "Result :NIL")
				)
				
		)	
	)
	(progn
		(print "SYNTAX ERROR FOR PARANTHESIS")
	)
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
)
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun gppinterpreter(par)
	(if (= 0 (length *args*))	
	  (loop 
	     (print "$ g++")
	     (defparameter line (read-line))
	     (setq line(string-downcase line))	   
	     (when (= 0 (length line))(return par))
         (setq lexertokens line)
	     (readFromUser line)
		(setq outputList nil)
		(setq lexertokens nil)
		(setq tokens nil)
		(setq paranthesis nil)
		(setq elementCounter 0)
		(setq parserErrorFlag 0)
      );;DOSYA YOKSA OKUMA DURUMU 
   
	  (progn
		  (let ((in (open (elt *args* 0) :if-does-not-exist nil)))
  			(when in
    		(loop for line = (read-line in nil)
        		while line do (if(= 0 (length line))
        			()
        			(progn
					(setq line(string-downcase line))	   
	     			(when (= 0 (length line))(return par))
         			(setq lexertokens line)
	     			(readFromUser line)
					(setq outputList nil)
					(setq lexertokens nil)
					(setq tokens nil)
					(setq paranthesis nil)
					(setq elementCounter 0)
					(setq parserErrorFlag 0))
        			)
			)
   				(close in)
			)


	  )
	  );;DOSYA VARSA OKUMA DURUMU
   )

)												

(gppinterpreter 1)