(defun C:D ( / *error* oldErr activeDoc )
	(defun LM:error ( msg )
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
	
	(setq oldErr *error*
          *error* LM:error)
	
	(vla-StartUndoMark activeDoc)
	(setvar "cmdecho" 0)  ; Disable terminal prompt
	(command "_.COPY" (ssget) "" 0 0 "") ; Copy all selected
	(setvar "cmdecho" 1)  ; Enable terminal prompt
	(vla-EndUndoMark activeDoc)
	
	(setq *error* oldErr)
	
	(princ)
)

;; "D" to duplicate all selected entities in-place
(setq info_DuplicateInPlace (list '("D" . "dupliciranje/kloniranje svih odabranih objekata u mjestu")))

(princ)