(defun C:MML ( / *error* oldErr linesSS entityList )
	
	(defun LM:error ( msg )
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(progn
				(princ (strcat "\nError: " msg))
				(setq *error* olderr)
			)
		) ; if end
		(princ)
	)
	
	(setq oldErr *error*
		  *error* LM:error)
	
	(princ "\nSelect lines: ")
	(if (not (setq linesSS (ssget '((0 . "LINE,*POLYLINE")))))  ; Check if selection is made, if not then exit
		(progn
			(princ "Nothing selected, exiting!")
			(exit)
		)
	)
	
	(setq entityList  (vl-remove-if 'listp (mapcar 'cadr (ssnamex linesSS)))  ; Creates list of all choosen entities
		  totalLength (apply '+ (mapcar (lambda(lineEnt)(vlax-curve-getdistatparam lineEnt (vlax-curve-getendparam lineEnt))) entityList)))  ; Get length of each line entity and sum all
	
	(princ (strcat "Total length: " (rtos totalLength 2 2)))
	
	(setq *error* oldErr)
	
	(princ)
)

;; "MML" to sum text numbers and paste result to another text
(setq info_MeasureMultipleLines (list '("MML" . "zbrajanje svih odabranih 'Line' i 'Polyline' objekata")))

(princ)