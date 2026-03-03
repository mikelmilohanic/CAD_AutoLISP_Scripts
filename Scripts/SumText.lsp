(defun C:STX (/ *error* oldErr userSelection  numList destText sumVal sumText destTexts destEnames)
	
	(defun LM:error ( msg )
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)
	
	(princ "\n<<< Select text to get sum >>>")
	
	(if (setq userSelection (ssget '((0 . "TEXT,MTEXT"))))  ; Select texts/mtexts on screen
		(progn
			(setq entityList (vl-remove-if 'listp (mapcar 'cadr (ssnamex userSelection)))  ; Convert selection set to list
				  numList (mapcar '(lambda(x)(LM:remove-rich-format (cdr (assoc 1 (entget x))))) entityList)  ; Get content of all text objects
				  numList (mapcar '(lambda(x)(vl-string-right-trim " " (vl-string-left-trim " " (vl-string-translate "," "." x)))) numList)  ; Replace comma with dot and remove leading and trailling spaces
				  numList (vl-remove-if 'zerop (mapcar 'atof numList))  ; Convert all values to float numbers and remove zero-values/NaN values
				  sumVal  (apply '+ numList)  ; Sum all numbers
				  sumText (vl-string-translate "." "," (rtos sumVal 2 2)))  ; Convert back to string and replace dot with comma
			
			(if (not (vl-string-search "," sumText))  ; Check if sum is a whole number and doesn't have decimal part [excluded: (< (rem sumVal 1) 0.000001)]
				(setq sumText (strcat sumText ",0"))  ; Add ",0" to ensure at least one floating point digit
			)
			
			(princ (strcat "\nSum = " sumText))  ; Show the sum in terminal
			
			(while (setq destTexts (ssget ":S" '((0 . "TEXT,MTEXT"))))  ; Loop to allow selecting multiple destination text entities
				(setq destEnames (vl-remove-if 'listp (mapcar 'cadr (ssnamex destTexts))))  ; Convert selection set to enames list
				
				(mapcar '(lambda(ename)
					(entmod (subst (cons 1 sumText) (assoc 1 (entget ename)) (entget ename))))  ; Update the destination text's content with the sum as the new text
				destEnames)
				
				(princ "\nText contents updated successfully!")
			)
			
			(princ "\nNo more destination texts selected. Exiting.")  ; If the user presses Enter without selecting a destination, the loop ends
		)
	)
	
	(setq *error* oldErr)
	
	(princ)
)

(defun LM:remove-rich-format ( str / _replace rx )  ; by Lee Mac

    (defun _replace ( new old str )
        (vlax-put-property rx 'pattern old)
        (vlax-invoke rx 'replace str new)
    )
	
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
        (progn
            (setq str
                (vl-catch-all-apply
                    (function
                        (lambda ( )
                            (vlax-put-property rx 'global     actrue)
                            (vlax-put-property rx 'multiline  actrue)
                            (vlax-put-property rx 'ignorecase acfalse) 
                            (foreach pair
                               '(
                                    ("\032"    . "\\\\\\\\")
                                    (" "       . "\\\\P|\\n|\\t")
                                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                                    ("$1"      . "[\\\\]({)|{")
                                )
                                (setq str (_replace (car pair) (cdr pair) str))
                            )
                        )
                    )
                )
            )
            (vlax-release-object rx)
            (if (null (vl-catch-all-error-p str))
                str
            )
        )
    )
) ; LM:remove-rich-format end

;; "STX" to sum text numbers and paste result to another text
(setq info_SumText (list '("STX" . "zbrajanje odabranih Text/MText objekata s brojcanim vrijednostima i ispis rezultata u odabrani Text/MText objekt")))

(princ)
