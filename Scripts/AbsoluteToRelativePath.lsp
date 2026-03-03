(vl-load-com)

(defun C:ATR     ( / ) (MM:ATR nil))
(defun C:ATR_ALL ( / ) (MM:ATR T))

(defun MM:ATR ( all / *error* oldErr selection objectsEnames VLAxRefs targetXRef )
	(defun LM:error ( msg )
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)
	
	(if (not (setq selection (ssget "_X" '((0 . "INSERT")))))  ; Get all block in the drawing (if there are any)
		(progn
			(princ "\nNo Xrefs found in the drawing.")
			(exit)
		)
	)
	
	(if all  ; Check which command is used
		(progn
			(setq objectsEnames (vl-remove-if 'listp (mapcar 'cadr (ssnamex selection)))  ; Get list of all blocks ename in a current drawing
				  VLAxRefs (vl-remove-if-not '(lambda(obj)(vlax-property-available-p obj 'Path)) (mapcar 'vlax-ename->vla-object objectsEnames))  ; Get all XRefs as VLA objects
				  VLAxRefs (MM:Unique VLAxRefs))  ; Remove duplicates with same name
			(mapcar '(lambda(xref)(vla-put-Path xref (MM:absolute-to-relative (vla-get-Path xref)))) VLAxRefs)  ; Change all XRefs path
			(command "-xref" "r" "*")  ; Reload all XRefs
		)
		
		(progn
			(princ "\nSelect XRef to change its path from absolute to relative:")
			(while (not (and (setq targetXRef (ssget "_:S:E" '((0 . "INSERT"))))  ; Check if selection is made
							 (vlax-property-available-p (setq targetXRef (vlax-ename->vla-object (ssname targetXRef 0))) 'Path)))  ; Check if selected entity is XRef
				(princ "\nInvalid selection, try again!")
			)
			(vla-put-Path targetXRef (MM:absolute-to-relative (vla-get-Path targetXRef)))  ; Change XRef path
			(command "-xref" "r" (vla-get-name targetXRef))  ; Reload target XRef
		)
	)

	(setq *error* oldErr)
	
	(princ)
)

(defun MM:absolute-to-relative ( targetPath / dwgPathLst xRefPathLst resultStr )
	(if (= (substr targetPath 1 1) ".")  ; Check if path is already in relative form
		targetPath  ; If relative return it
		(progn  ; Else make it relative
			(setq dwgPathLst (MM:split (vl-string-trim "\\" (substr (getvar 'dwgprefix) 3)) "\\")  ; Convert dwg path to list
				  xRefPathLst (MM:split (vl-string-trim "\\" (substr targetPath 3)) "\\"))  ; Convert XRef path to list
			
			(if (not (member (car xRefPathLst) (MM:gfneoal dwgPathLst 3)))  ; Check if paths start (root) is same
				(progn
					(princ "\nPaths have different root. Exiting.")
					(exit)
				)
				(progn
					(while (not (or (= dwgPathLst nil) (= (car dwgPathLst) (car xRefPathLst))))   ; Loop till first elements aren't same
						(setq dwgPathLst (cdr dwgPathLst))  ; Remove first element from dwg path
					)
					
					(if (not dwgPathLst)  ; Check if dwg path exists
						targetPath  ; If not return absolute path
						(progn  ; Else convert to relative
							(while (= (car xRefPathLst) (car dwgPathLst))  ; Loop till their path elements are same and drop first elements
								(setq dwgPathLst (cdr dwgPathLst)
									xRefPathLst (cdr xRefPathLst))
							)
							
							(setq resultStr (MM:join xRefPathLst "\\"))  ; Join remaining XRef path
							
							(if dwgPathLst  ; Check if dwg path was sub-path of XRef path
								(repeat (length dwgPathLst)
									(setq resultStr (strcat "..\\" resultStr))  ; If it was not go back as many directories as their paths differs
								)
								(strcat ".\\" resultStr)  ; Else just add prefix of directory where dwg file is located
							)
						)
					)
				)
			)
		)	
	)
)

(defun MM:split ( strVar sepVar / posVar )  ; Recursive split
	(if (setq posVar (vl-string-search sepVar strVar))
		(cons (substr strVar 1 posVar) (MM:split (substr strVar (+ posVar 2)) sepVar))
		(list strVar)
	)
)

(defun MM:join ( lstVar sepVar / resVar )  ; Recursive join
	(if (> (length lstVar) 1)
		(strcat (car lstVar) sepVar (MM:join (cdr lstVar) sepVar))
		(car lstVar)
	)
)

(defun MM:gfneoal (lst n)  ; Get first n elements of a list
	(if (or (null lst) (<= n 0))
		nil  ; Return nil if list is empty or n is non-positive
		(cons (car lst) (MM:gfneoal (cdr lst) (1- n)))  ; Cons the first element and recurse
	)
)

(defun MM:Unique ( lst )  ; 'Unique' by Lee Mac - Remake by MM - Returns a list with duplicate elements removed.
    (if lst (cons (car lst) (LM:Unique (vl-remove-if '(lambda(x)(= (vla-get-name x) (vla-get-name (car lst)))) (cdr lst)))))
)

;; "ATR" to select xRef and convert it's absolute path to relative
;; "ATR_ALL" to convert all xRefs absolute path to relative
(setq info_AbsoluteToRelativePath (list '("ATR" . "pretvaranje apsolutne putanje u relativnu za odabrani XRef")
										'("ATR_ALL" . "pretvaranje apsolutne putanje u relativnu za sve XRef-ove u crtezu")))

(princ)