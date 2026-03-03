(defun C:QS ( / *error* oldErr pTypes entityType propType baseObject ss )
	(defun LM:error ( msg )
		(setq *error* oldErr)
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(setq *error* oldErr)
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)
	
	(setq pTypes '(("COLOR" 62) ("LAYER" 8) ("LINETYPESCALE" 48)))
	
	(initget "All Line Arc Polyline Circle Text MText Block Hatch")
	(setq entityType
		(getkword "\nSelect entity type: All/Line/Arc/Polyline/Circle/Text/MText/Block/Hatch:<All> ")
	)
	
	(cond
		((= entityType "Polyline")(setq entityType "*POLYLINE"))
		((= entityType "Block")(setq entityType "INSERT"))
		((or (= entityType "All") (not entityType))(setq entityType "*"))
		(entityType (setq entityType (strcase entityType)))
	)
	
	(initget "Color Layer LinetypeScale")
	(setq propType
		(getkword "\nSelect property: Color/Layer/LinetypeScale:<Layer> ")
	)
	
	(setq propType (if propType (cadr (assoc (strcase propType) pTypes)) 8))
	
	(princ "\nSelect entity that contains targeted property value: ")
	(while (not (setq baseObject (car (entsel))))
		(princ "\nMissed, try again!")
	)
	
	(if (setq ss
			(ssget
				"_X"
				(list
					(cons 0 entityType)
					(assoc propType (entget baseObject))
				)
			)
		)
		(sssetfirst nil ss)
		(princ "\nNo objects found!")
	)
	
	(setq *error* oldErr)
	
	(princ)
)

;; "QS" to use simpler CLI Quick Select
(setq info_QuickSelect (list '("QS" . "jednostavniji i brzi Quick Select koji bira sve iz crteza sto zadovoljava kriterije")))

(princ)