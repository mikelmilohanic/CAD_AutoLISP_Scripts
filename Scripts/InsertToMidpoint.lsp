(vl-load-com)

(defun C:ITM ( / *error* oldErr selection pointsList sourceEnt )
	;; Routine for pasting blocks to line midpoints
	
	(defun LM:error ( msg )
		(if (= 8 (logand (getvar 'undoctl) 8))
			(command "._UNDO" "End")
		)
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)
	
	(princ "\nSelect lines: ")
	(if (not (setq selection (ssget '((0 . "LINE")))))  ; Check if selection is made, if not then exit
		(progn
			(princ "Nothing selected, exiting!")
			(exit)
		)
	)
	
	(setq selection (mapcar 'cadr (ssnamex selection))  ; Get all entity names from the lines
		  selection (vl-remove-if 'listp selection)  ; Removes if list element is a list (only names should be elements of a list)
		  pointsList (mapcar '(lambda (ent) (vlax-curve-getPointatParam ent (/ (+ (vlax-curve-getEndParam ent) (vlax-curve-getStartParam ent)) 2.))) selection))  ; Create a list of midpoints
	
	(princ "\nSelect block for insertion: ")
	(while (or  (not (setq sourceEnt (ssget "_:S:E" '((0 . "INSERT")))))  ; Loop while 'block' entity isn't seleceted
				(vlax-property-available-p (vlax-ename->vla-object (setq sourceEnt (ssname sourceEnt 0))) 'path))  ; Or XRef is selected (block with path)
		(princ "\nInvalid selection, try again!")
	); end while
	
	(command "._UNDO" "Begin")
	
	(mapcar 'entdel selection)  ; Delete lines
	
	(setvar "cmdecho" 0)  ; Disable terminal prompt
	(mapcar '(lambda(pt)(command "_.COPY" sourceEnt "" (cdr (assoc 10 (entget sourceEnt))) pt)) pointsList)  ; Copy chosen entity an all midpoints
	(setvar "cmdecho" 1)  ; Enable terminal prompt
	
	(command "._UNDO" "End")
	
	(setq *error* oldErr)
	
	(princ)
)

;; "ITM\" to select lines and block, then block is inserted to all lines midpoints.
(setq info_InsertToMidpoint (list '("ITM" . "umetanje odabranog bloka na poloviste svih odabranih linija")))

(princ)