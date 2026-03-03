(vl-load-com)

(defun c:EB ( / userSelection entityList selectedEntity segmentsNumber pointsList segmentLength counterVar centerPoint )
	
	(princ "\n<<< Select line/curve entities to break into segments >>>")
	
	(while (not (setq userSelection (ssget '((0 . "LINE,*POLYLINE,ARC,SPLINE,CIRCLE")))))  ; Prompts user to make a selection of lines or curves on unlocked layers
		(princ "Nothing selected, try again.")
	)
		
	(while (not segmentsNumber)
		(initget 7)  ; Restricts user input as string, integer or real number
		(setq segmentsNumber (getint "\nSpecify number of divisions of curve: "))
	)
	
	(if (= segmentsNumber 1) (exit))  ; Exits if user want to break an entities into one piece (because it's already
	
	(setq entityList (vl-remove-if 'listp (mapcar 'cadr (ssnamex userSelection))))  ; Creates list of all choosen entities
	
	(setvar "cmdecho" 0)  ; Disable terminal prompt
	(command "_.UNDO" "_Begin")  ; Begin 'UNDO' command
	
	(foreach selectedEntity entityList  ; Loop through all selected entities
		(progn
			(setq pointsList (cons (trans (vlax-curve-getstartpoint selectedEntity) 0 1) pointsList)  ; Add start point to list
				  segmentLength (/ (vlax-curve-getdistatparam selectedEntity (vlax-curve-getendparam selectedEntity)) (float segmentsNumber))  ; Calculate segment length
				  counterVar 1)  ; Initialize counter
			
			(while (< counterVar segmentsNumber)
				(setq pointsList (cons (trans (vlax-curve-getpointatdist selectedEntity (* counterVar segmentLength)) 0 1) pointsList)  ; Calculate point and add to list
					  counterVar (1+ counterVar))  ; Increase counter
			)
			
			(setq pointsList (reverse pointsList))  ; Reverse the list of points (set order from first to last point)
			
			(cond
				((= (cdr (assoc 0 (entget selectedEntity))) "CIRCLE")  ; Check if entity is circle
					(setq centerPoint (cdr (assoc 10 (entget selectedEntity))))  ; Get the center point
					(command "_.BREAK" selectedEntity (last pointsList) (car pointsList))  ; Break first segment from the circle
					(foreach pt (reverse pointsList)  ; For each point in point list
						(command "_.BREAK" selectedEntity pt pt)  ; Break entity at current point
					)
					(command "_.MIRROR" selectedEntity "" centerPoint (car pointsList) "N")  ; Mirror last segment on first point and center to fill first segment space
				)
				(t  ; Break all other entities (lines/curves)
					(foreach pt (reverse pointsList)  ; For each point in point list (except first)
						(command "_.BREAK" selectedEntity pt pt)  ; Break at start of segment
					)
				)
			)
			
			(setq pointsList nil)  ; Clear points list for next object
		)
	)
	
	(command "._UNDO" "_End")  ; End 'UNDO' command
	(setvar "cmdecho" 1)  ; Enable terminal prompt
	
	(princ)
)

;; "EB" to break selected line into equal segments
(setq info_EqualBreak (list '("EB" . "podjela (lomljenje) Line, Polyline, Arc, Spline ili Circle objekta na zeljeni broj jednakih dijelova")))

(princ)