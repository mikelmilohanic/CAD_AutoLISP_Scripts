(vl-load-com)

(defun C:TF ( / ) (MM:TBF T))
(defun C:TB ( / ) (MM:TBF nil))

(defun MM:TBF ( front / *error* oldErr ss enamesList )
	;; Routine to select objects and then perform 'Send to back' on all of them
	
	(defun LM:error ( msg )
		(setvar "cmdecho" 1)
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		)
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)
	
	(setvar "cmdecho" 0)
	(while (setq ss (ssget "_:S"))
		(setq enamesList (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
		(if front
			(mapcar '(lambda(obj)(command "._DRAWORDER" obj "" "F")) enamesList)
			(mapcar '(lambda(obj)(command "._DRAWORDER" obj "" "B")) enamesList)
		)
	)
	(setvar "cmdecho" 1)
	
	(setq *error* oldErr)
	
	(princ)
)

;; "TF" to select objects and perform 'Bring to front' on them
;; "TB" to select objects and perform 'Send to back' on them.
(setq info_ToBackToFront (list '("TF" . "neograniceno odabiranje objekata nad kojima treba izvesti 'Bring to front' naredbu.")
								'("TB" . "neograniceno odabiranje objekata nad kojima treba izvesti 'Send to back' naredbu.")))

(princ)