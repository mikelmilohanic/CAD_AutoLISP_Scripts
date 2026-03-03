(vl-load-com)

(defun C:TNC ( / *error* oldErr osm activeDoc mainObject mainText prefixText userInput position incNum suffixText prefixList incStep targetObject targetData changedEntities)
	
	;; Rutine for increasing number in text object by chosen prefix and costum incremental step
	
	(defun LM:error ( msg )
		(if osm (setvar 'osmode osm))
		(if (= 8 (logand (getvar 'undoctl) 8))
			(vla-EndUndoMark activeDoc)
		)
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(setq *error* oldErr)
		(princ)
	)
	
	(setq activeDoc (vla-get-activedocument (vlax-get-acad-object)))
	
	(princ "\n<<< Select Text/MText object that contains numbers: >>>")
	
	(while (not (and (setq mainObject (ssget ":E:S" '((0 . "TEXT,MTEXT"))))  ; Loop while Text O MText object isn't seleceted
					 (MM:check-for-numbers "some" (LM:remove-rich-format (vla-get-TextString (vlax-ename->vla-object (ssname mainObject 0)))))))  ; Check if selected object contains numbers
		(princ "\n<!> Invalid selection, try again. <!> ")
	)  ; while end
	
	(setq mainText (vla-get-TextString (vlax-ename->vla-object (ssname mainObject 0)))  ; Get selected text object content
		  mainText (LM:remove-rich-format mainText))  ; Remove rich text format from text
	
	(if (MM:check-for-numbers "every" (MM:trim-non-numerical-from-sides mainText))  ; Check if there's only one number value in text
			(setq incNum (MM:trim-non-numerical-from-sides mainText)  ; Set number as text with trimmed non-numeric values from sides
				  position (vl-string-search incNum mainText)  ; Set position where number starts
				  prefixText (substr mainText 1 position)  ; Set prefix as everything before number
				  suffixText (substr mainText (1+ (+ position (strlen incNum)))))  ; Set suffix as everything after number
		
		;; ELSE
		(progn
			(setq position 2)  ; Set position on second character
	
			(if (vl-string-search (substr mainText 1 1) "0123456789")  ; Check if first character is number
				(setq prefixList (cons "No prefix" prefixList))  ; Add option with no prefix
			) ; if end
			
			(while (<= position (strlen mainText))  ; While not end of string
				(if (and (vl-string-search (substr mainText position 1) "0123456789")  ; Check if there's a number at current position
						 (not (vl-string-search (substr mainText (1- position) 1) "0123456789")))  ; Check if there's not a number at previous position
					(setq prefixList (cons (substr mainText 1 (1- position)) prefixList))  ; Get prefix before current number
				)
				(setq position (1+ position))  ; Go to next character in a string
			) ; while end
			
			(setq userInput (car (AT:list-select prefixList)))  ; Open dialog box with all possible prefixes and save user selection
			
			(if (not userInput) (exit))
			
			(if (= userInput "No prefix")  ; Check if user selected "No prefix" option (else means that some prefix is selected)
				(setq prefixText ""  ; Set prefix as nothing
					  suffixText (vl-string-left-trim "0123456789" mainText)  ; Set suffix as text without starting numbers
					  incNum (vl-string-subst "" suffixText mainText))  ; Set number as part without suffix
				;; ELSE
				(progn
				(setq prefixText userInput  ; Set prefix as prefix selected by user
					  suffixText (vl-string-left-trim "0123456789" (substr mainText (1+ (strlen prefixText))))  ; Set suffix as text without prefix and leading numbers
					  incNum (substr mainText (1+ (strlen prefixText)) (- (strlen mainText) (strlen suffixText) (strlen prefixText))))  ; Set number as a part after prefix
				)
			) ; if end
		) ; progn end
	) ; if end

	(setq incStep 1)
	
	(vla-StartUndoMark activeDoc)
	
	(setq oldErr *error*
          *error* LM:error)
	
	(setq osm (getvar 'osmode))
    (setvar 'osmode 0)
	
	(while
		(progn
			(setvar 'errno 0)
			(initget "Step Undo Exit")
			(setq targetObject (entsel "\nSelect destination block [Step/Undo/Exit]: "))
			(cond
				((= 7 (getvar 'errno))  ; Check if clicked on background
                    (princ "\nMissed, try again.")
                )  ; End of first condition
				
				((= "Step" targetObject)  ; Check if user want change step
					(while
						(progn
							(setq userInput (getstring (strcat "\nEnter increase step: ")))
							(cond 
								((and (vl-string-search (substr userInput 1 1) "-+0123456789")  ; Check if first character is valid
									(MM:check-for-numbers "every" (substr userInput 2)))  ; Check if rest of input is valid
									(setq incStep (atoi userInput))
									nil
								)  ; End of second condition
								(t (princ "\nInvalid input, try again."))
							) ; cond end
						) ; progn end
					) ; while end
					T ; return 'true'
                )  ; End of second condition
				
				((= "Undo" targetObject)  ; Check if user want to undo last change
                    (if changedEntities  ; Check if there are any changed objects
						(progn
							(entmod (car changedEntities))  ; Change last changed object to its previous content
							(setq changedEntities (cdr changedEntities)  ; Remove that object from a list of changed entities
								  incNum (MM:increase-number '- incNum incStep))  ; Decrease number
						)
						(princ "\nThere's no changes to undo.")
					)
                )  ; End of third condition
				
                ((= "Exit" targetObject)  ; Check if user want to exit
					(princ "\nNo more destination blocks selected. Exiting.")
                    (exit)
                )  ; End of fourth condition
				
				((or (= (cdr (assoc 0 (entget (car targetObject)))) "TEXT")(= (cdr (assoc 0 (entget (car targetObject)))) "MTEXT"))  ; Check if text or mtext is chosen
					(setq targetData (entget (car targetObject))  ; Get choosen objects data
						  changedEntities (cons targetData changedEntities)  ; Save content of target object (for future undoing)
						  incNum (MM:increase-number '+ incNum incStep))  ; Increase number
					(entmod (subst (cons 1 (strcat prefixText incNum suffixText)) (assoc 1 targetData) targetData))  ; Change object content
				)  ; End of fifth condition
				
				(t (princ "\nInvalid selection, try again: "))
			) ; cond end
		) ; progn end
	) ; while end
	
	(setvar 'osmode osm)
	(setq *error* oldErr)
	(vla-EndUndoMark activeDoc)
	
	(princ)
) ; TNC end

(defun MM:check-for-numbers ( amount targetStr )  ; Checks if string contains any or all numbers [any: amount == 'some'; all: amount == 'every']
	(eval ((read (strcat "vl-" amount))(lambda (ch)(vl-string-search (chr ch) "0123456789"))(vl-string->list targetStr)))
) ; MM:check-for-numbers end

(defun MM:trim-non-numerical-from-sides ( targetStr / start end len)  ; Removes all leading and trailing non-numeric characters
	(setq start 1) ; Set start at first character
	(while (not (vl-string-search (substr targetStr start 1) "0123456789"))  ; Check if character at current position is not a number
		(setq  start (1+ start))  ; Increase position
	) ; while end
	
	(setq end (strlen targetStr)) ; Set end at last character
	(while (not (vl-string-search (substr targetStr end 1) "0123456789"))  ; Check if character at current position is not a number
		(setq  end (1- end))  ; Decrease position
	) ; while end
	
	(setq len (1+ (- end start)))
	
	(substr targetStr start len)  ; Return string without trailing non-numerical characters
) ; MM:trim-non-numerical-from-sides end

(defun MM:increase-number ( operator numStr step / num numLen strL zeros )  ; Increments number while keeping possible leading zeros
	(setq num (apply operator (list (atoi numStr) step)))  ; Convert string to integer and increment
	(setq numLen (strlen (itoa num)))  ; Length of the incremented number
	(setq strL (strlen numStr))     ; Original string length
	(if (> strL numLen)
		(setq zeros (repeat (- strL numLen) "0"))  ; Calculate needed zeros
		(setq zeros "")
	) ; if end
	(strcat zeros (itoa num))  ; Return concatenated zeros and number
) ; MM:increase-number end


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

(defun AT:list-select ( lst / longestString title label height width multi fileName openFile dialogObject userSelection dialogValue indexValue outputList )
	
	;; Alan J. Thompson, 09.23.08 / 05.17.10 (rewrite) / 03.03.25 (remake by MM)
	;; List Select Dialog (Temp DCL list box selection, based on provided list)
	
	;; lst - list of strings to place in list box
	
	(setq longestString (apply 'max (mapcar 'strlen lst))) ; Get the length of longest string in a list
	
	(setq title "Prefix List"									; title - list box title
		  label "Select prefix:"								; label - label for list box
		  height 20												; height - height of the box
		  width (if (< longestString 25) 30 (+ longestString 5)); width - width of the box
		  multi "false")										; multi - selection method ["true": multiple, "false": single]
	
	(setq openFile (open (setq fileName (vl-filename-mktemp "" "" ".dcl")) "w")) ; Open temporarily .dcl file for writing
	(foreach text_line 
		(list 
			(strcat "list_select : dialog { label = \"" title "\"; spacer;")
			(strcat ": list_box { label = \"" label "\";" "key = \"lst\";")
			(strcat "allow_accept = true; height = " (vl-princ-to-string height) ";")
			(strcat "width = " (vl-princ-to-string width) ";")
			(strcat "multiple_select = " multi "; } spacer; ok_cancel; }")
		) ; DCL code for dialog
		(write-line text_line openFile) ; Open file and write line in it
	) ; Iterate through all lines
	(close openFile)  ; Close temporarily .dcl file
	
	(new_dialog "list_select" (setq dialogObject (load_dialog fileName)))
	
	(start_list "lst")
	(mapcar (function add_list) lst)
	(end_list)
	
	(setq userSelection (set_tile "lst" "0"))
	(action_tile "lst" "(setq userSelection $value)")
	(setq dialogValue (start_dialog))
	
	(unload_dialog dialogObject)
	(vl-file-delete fileName)
	
	(if (= dialogValue 1) ; Check 'dialogValue' value
		(progn
			(while (setq indexValue (vl-string-search " " userSelection)) ; Find space in string (for multiple selections)
				(setq outputList (cons (nth (atoi (substr userSelection 1 indexValue)) lst) outputList)) ; Find element (based on index) in original string and store it in output list
				(setq userSelection (substr userSelection (+ 2 indexValue))) ; Remove processed part of the string
			) ; while end
			(reverse (cons (nth (atoi userSelection) lst) outputList)) ; Add last element and return reversed list
		) ; progn end
	) ; if end
) ; AT:list-select end

;; "TNC" to select object, pick incremental step, and then click to paste and generate
(setq info_CustomIncrement (list '("TNC" . "generiranje povecanih/smanjenih brojcanih vrijednosti u postojece Text/MText objekte prema odabranom koraku i prefixu")))

(princ)