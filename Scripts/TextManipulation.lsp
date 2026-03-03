(defun C:EAT ( / textMtextSelectionSet textMtextEnames textMtextProps )
	(setq textMtextSelectionSet (ssget "_X" (list '(0 . "*TEXT")))  ; Select all text and mtext object from the database
		  textMtextEnames (vl-remove-if 'listp (mapcar 'cadr (ssnamex textMtextSelectionSet)))  ; Convert selection set to list of objects enames
		  textMtextProps (mapcar 'entget textMtextEnames)  ; Convert enames to object properties lists
	)
	
	(setvar "cmdecho" 0)  ; Disable terminal prompt
	(command "._UNDO" "Begin")  ; Start undo mark
	(mapcar '(lambda(ent)(entmod (subst (cons 1 (vl-string-translate "\U+0111" "d" (cdr (assoc 1 ent)))) (assoc 1 ent) ent))) textMtextProps)  ; Replace 'đ' to 'd' in all text and mtext objects
	(mapcar 'entupd textMtextEnames)  ; Update all text and mtext objects
	(mapcar '(lambda(ename)(command "._EXPLODE" ename)) textMtextEnames)  ; Explode all text and mtext objects
	(command "._UNDO" "End")  ; End undo mark
	(setvar "cmdecho" 1)  ; Enable terminal prompt
	(princ)
)

(defun C:SAT ( / )
	(sssetfirst nil (ssget "A" '((0 . "TEXT,MTEXT"))))
	(princ)
)

;; "EAT\" to explode all text and mtext objects in the drawing
(setq info_TextManipulation (list 	'("EAT" . "izvrsavanje naredbe EXPLODE nad svim Text/MText objektima")
									'("SAT" . "selektiranje svih Text i MText objekata na crtezu")))

(princ)