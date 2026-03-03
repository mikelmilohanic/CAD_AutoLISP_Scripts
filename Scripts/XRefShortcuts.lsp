(defun C:DX ( / )
	(setvar "cmdecho" 0)
	(command "._-xref" "detach" (cdr (assoc 2 (entget (car (entsel "Pick XRef to detach."))))))
	(setvar "cmdecho" 1)
	(princ)
)

(defun C:DAX ( / )
	(setvar "cmdecho" 0)
	(command "._-xref" "detach" "*")
	(setvar "cmdecho" 1)
	(princ)
)

(defun C:BAX ( / )
	(setvar "cmdecho" 0)
	(command "._-xref" "Bind" "*")
	(setvar "cmdecho" 1)
	(princ)
)

(defun C:RX ( / )
	(setvar "cmdecho" 0)
	(command "._-xref" "reload" (cdr (assoc 2 (entget (car (entsel "Pick XRef to detach."))))))
	(setvar "cmdecho" 1)
	(princ)
)

(defun C:AAX ( / *error* oldErr activeDoc xRefFolder fName )
	(defun LM:error ( msg )
		(if (= (getvar 'cmdecho) 0) (setvar 'cmdecho 1))
		(if (= 8 (logand (getvar 'undoctl) 8))
			(vla-EndUndoMark activeDoc)
		)
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)

	(setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object))) ; Get the active document
	
	(if (setq xRefFolder (car (vl-remove-if-not '(lambda(x)(= (strcase x) (strcase "XREF"))) (vl-directory-files (getvar 'dwgprefix) nil -1))))
		(progn
			(setq xRefFolderPath (strcat (vl-string-right-trim "\\" (getvar 'DWGPREFIX)) "\\" xRefFolder))
			
			(vla-StartUndoMark activeDoc)
			(setvar "cmdecho" 0)  ; Disable terminal prompt
			
			(mapcar
				(lambda(fName)
					;; Loops through all files in xRef folder, adds them folder path and attaches them on 0,0,0 coordinate in 1,1,1 scale and rotation 0
					(command "-XREF" "A" (strcat xRefFolderPath "\\" fName) '(0 0 0) 1 1 0)
					
					;; Set inserted xRef path to relative
					(vla-put-Path (vlax-ename->vla-object (entlast)) (strcat ".\\" xRefFolder "\\" fName))
				)
				(vl-directory-files xRefFolderPath "*.dwg")
			)
			
			(setvar "cmdecho" 1)  ; Enable terminal prompt
			(vla-EndUndoMark activeDoc)
		)
		(alert "\nGreska! U mapi gdje se nalazi trenutni dwg nije pronaden xRef folder.")
	)
	
	(setq *error* oldErr)
	
	(princ)
)

(setq info_XRefShortcuts 
	(list 
		'("AAX" . "ubacivanje svih xRef-ova iz 'xref' foldera")
		'("BAX" . "bindanje svih XRef-ova iz crteza")
		'("DX" . "otkvacivanje (uklanjanje) odabranog XRef-a iz crteza")
		'("DAX" . "otkvacivanje (uklanjanje) svih XRef-ova iz crteza")
		'("RX" . "osvjezavanje (reload) odabranog XRef-a iz crteza")
	)
)

(princ)