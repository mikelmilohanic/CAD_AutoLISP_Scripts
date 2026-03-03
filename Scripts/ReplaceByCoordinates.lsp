;; Script for:
;;		- exporting coordinates from HDKS5/HDKS6 coordinate system, all linked to their enities
;;		- importing coordinates converted to HTRS96 and transforming entities to new coordinates

(vl-load-com)

(defun LM:error ( msg )
	(if (= (getvar 'cmdecho) 0) (setvar 'cmdecho 1))
	(if (= 8 (logand (getvar 'undoctl) 8))
		(vla-EndUndoMark activeDoc)
	)
	(if file (close file))
	(if (not (member msg '("Function cancelled" "quit / exit abort")))
		(princ (strcat "\nError: " msg))
	) ; if end
	(princ)
)

(defun C:EXPORT_COORDINATES ( / *error* oldErr activeDoc mSpace coorList entCnt outsiders idHandle startPoint endPoint coors elevation basePoint pointsList file )
	(setq
		oldErr *error*
        *error* LM:error
	)
	
	(setq
		activeDoc (vla-get-ActiveDocument (vlax-get-acad-object))  ; Get active document
		mSpace (vla-get-ModelSpace activeDoc)  ; Get modelspace
		entCnt 0
		outsiders 0
	)
	
	(vlax-for entity mSpace  ; Iterate through all entities in model space
		(cond
			((= (vla-get-objectname entity) "AcDbLine")  ; Check if entity is line
				(setq 
					startPoint (vlax-curve-getstartpoint entity)  ; Get line starting point
					endPoint (vlax-curve-getendpoint entity)  ; Get line end point
					entity (vlax-vla-object->ename entity)  ; Convert entity to ename
					idHandle (cdr (assoc 5 (entget entity)))  ; Get entity handle
					entCnt (1+ entCnt)  ; Count the entity to total sum
				)
				(setq 
					coorList (cons (strcat "L" idHandle " " (rtos (car startPoint) 2 6) " " (rtos (cadr startPoint) 2 6) " " (rtos (caddr startPoint) 2 6)) coorList)  ; Add line start point coordinates to list of all coordinates
					coorList (cons (strcat "L" idHandle " " (rtos (car endPoint) 2 6) " " (rtos (cadr endPoint) 2 6) " " (rtos (caddr endPoint) 2 6)) coorList)  ; Add line end point coordinates to list of all coordinates
				)
			)
			
			((= (vla-get-objectname entity) "AcDbPolyline")  ; Check if entity is polyline
				(setq
					coors (vlax-get entity 'coordinates)  ; Get polyline vetices coordinates
					elevation (vla-get-elevation entity)  ; Get polyline elevation value (z-axis)
					entity (vlax-vla-object->ename entity)  ; Convert entity to ename
					idHandle (cdr (assoc 5 (entget entity)))  ; Get entity handle
					entCnt (1+ entCnt)  ; Count the entity to total sum
				)
				(while coors  ; Loop while all coordinates are converted to 3D points
					(setq coorList (cons (strcat "P" idHandle " " (rtos (car coors) 2 6) " " (rtos (cadr coors) 2 6) " " (rtos elevation 2 6)) coorList)  ; Add point coordinates to list of all coordinates
						  coors (cddr coors))  ; Remove its x-coor and y-coor from the beggining
				)
			)
			
			((= (vla-get-objectname entity) "AcDb2dPolyline")  ; Check if entity is 2Dpolyline
				(setq
					coors (vlax-get entity 'coordinates)  ; Get polyline vetices coordinates
					entity (vlax-vla-object->ename entity)  ; Convert entity to ename
					idHandle (cdr (assoc 5 (entget entity)))  ; Get entity handle
					entCnt (1+ entCnt)  ; Count the entity to total sum
				)
				(while coors  ; Loop while all coordinates are converted to 3D points
					(setq coorList (cons (strcat "P" idHandle " " (rtos (car coors) 2 6) " " (rtos (cadr coors) 2 6) " " (rtos (caddr coors) 2 6)) coorList)  ; Add point coordinates to list of all coordinates
						  coors (cdddr coors))  ; Remove its x-coor and y-coor from the beggining
				)
			)
			
			((= (vla-get-objectname entity) "AcDbBlockReference")  ; Check if entity is block reference
				(setq
					entity (vlax-vla-object->ename entity)  ; Convert entity to ename
					basePoint (cdr (assoc 10 (entget entity)))  ; Get entity base point
					idHandle (cdr (assoc 5 (entget entity)))  ; Get entity handle
					entCnt (1+ entCnt)  ; Count the entity to total sum
				)
				
				(if (and (>= (car basePoint) 5250000) (<= (car basePoint) 6850000) (>= (cadr basePoint) 4700000) (<= (cadr basePoint) 5170000))  ; Check if base point is in HDKS5/HDKS6 zones
					(setq coorList (cons (strcat "B" idHandle " " (rtos (car basePoint) 2 6) " " (rtos (cadr basePoint) 2 6) " " (rtos (caddr basePoint) 2 6)) coorList))  ; Add block base point coordinates to list of all coordinates
					(setq outsiders (1+ outsiders))
				)
			)
			
			((or (= (vla-get-objectname entity) "AcDbText") (= (vla-get-objectname entity) "AcDbMText"))  ; Check if entity is text object
				(setq
					entity (vlax-vla-object->ename entity)  ; Convert entity to ename
					basePoint (cdr (assoc 10 (entget entity)))  ; Get entity base point
					idHandle (cdr (assoc 5 (entget entity)))  ; Get entity handle
					entCnt (1+ entCnt)  ; Count the entity to total sum
				)
				(setq coorList (cons (strcat "T" idHandle " " (rtos (car basePoint) 2 6) " " (rtos (cadr basePoint) 2 6) " " (rtos (caddr basePoint) 2 6)) coorList))  ; Add text base point coordinates to list of all coordinates
			)
			
			((= (vla-get-objectname entity) "AcDbCircle")  ; Check if entity is circle
				(setq
					entity (vlax-vla-object->ename entity)  ; Convert entity to ename
					basePoint (cdr (assoc 10 (entget entity)))  ; Get entity base point
					idHandle (cdr (assoc 5 (entget entity)))  ; Get entity handle
					entCnt (1+ entCnt)  ; Count the entity to total sum
				)
				(setq coorList (cons (strcat "C" idHandle " " (rtos (car basePoint) 2 6) " " (rtos (cadr basePoint) 2 6) " " (rtos (caddr basePoint) 2 6)) coorList))  ; Add circle base point coordinates to list of all coordinates
			)
		)
	)
	
	(if (> outsiders 0)
		(alert (strcat "\Pronadeni blokovi sa hvatistem izvan HDKS5 i HDKS6 zona, navedene blokove nije moguce prebaciti u HTRS96, njih ukupno: " (itoa outsiders)))
	)
	
	(if (setq file (open (strcat (getvar 'dwgprefix) "old_coordinates.txt") "w"))  ; Open file in the same directory where is .dwg file (create if doesn't exist)
		(progn
			(foreach coorText (reverse (cdr coorList))  ; Loop through all points except last one
				(write-line coorText file)  ; Write each point into the file
			)
			(foreach char (vl-string->list (car coorList))  ; Loop through list of all characters of last point record (to avoid creating newline character)
				(write-char char file)  ; Write character into the file
			)
			(close file)  ; Make changes, save and close file
			(alert (strcat "\nAll entities points exported to csv file. Total of " (itoa entCnt) " entities."))
			(princ (strcat "File saved in " (getvar 'dwgprefix) "old_coordinates.txt"))
		)
		(alert "\nCSV file Currenty running, Close it first.")
	)
	
	(setq *error* olderr)
	
	(princ)
)

(defun C:IMPORT_COORDINATES ( / *error* file oldErr activeDoc newCoors cntVar sortedCoors helpLst )
	(setq
		oldErr *error*
        *error* LM:error
	)
	
	(setq
		activeDoc (vla-get-ActiveDocument (vlax-get-acad-object))  ; Get active document
		newCoors (MM:get-file (getfiled "Choose text file with transformed coordinates" "" "txt" 33))  ; Read new coordinates from file
		cntVar 0
	)
	
	(while (< cntVar (length newCoors))  ; Loop through all coordinates
		(cond 
			((= (substr (car (nth cntVar newCoors)) 1 1) "L")  ; Check if its a line
				(setq 
					sortedCoors 
						(cons 
							(list
								(car (nth cntVar newCoors))  ; Add objeck mark to a list
								(list (cadr (nth cntVar newCoors)) (caddr (nth cntVar newCoors)) 0)  ; Add start point to a list
								(list (cadr (nth (1+ cntVar) newCoors)) (caddr (nth (1+ cntVar) newCoors)) 0)  ; Add end point to a list
							)  ; Add list like (mark (start_coor) (end_coor)) to a list of all objects with coordinates
							sortedCoors
						)
					cntVar (+ cntVar 2)  ; Increase by two to skip end coordinate of a line
				)
			)
			((= (substr (car (nth cntVar newCoors)) 1 1) "P")  ; Check if its a polyline
				(setq plineHandler (car (nth cntVar newCoors)))  ; Extract polyline handler from mark
				(while (= (car (nth cntVar newCoors)) plineHandler)  ; Go through all records of a polyline vertices with same mark
					(setq
						helpLst (cons (cadr (nth cntVar newCoors)) helpLst)  ; Get x-coor
						helpLst (cons (caddr (nth cntVar newCoors)) helpLst)  ; Get y-coor
						cntVar (1+ cntVar)  ; Go to next point in a list
					)
				)
				(setq
					sortedCoors (cons (list plineHandler (reverse helpLst)) sortedCoors)  ; Add list like (mark (start_coor next_vertex ... next_vertex end_coor)) to a list of all objects with coordinates
					helpLst nil  ; Empty the helper list
				)
			)
			((vl-string-search (substr (car (nth cntVar newCoors)) 1 1) "BTC")  ; Check if its a block, text or circle
				(setq
					sortedCoors (cons (reverse (cons 0.0 (cdr (reverse (nth cntVar newCoors))))) sortedCoors)  ; Set z-coor to zero
					cntVar (1+ cntVar)  ; Go to next point in a list
				)
			)
		)
	)
	
	(vla-StartUndoMark activeDoc)  ; Start group undo
	(setvar "cmdecho" 0)  ; Disable terminal prompt
	(mapcar 'MM:transform-coordinates (reverse sortedCoors))
	(setvar "cmdecho" 1)  ; Enable terminal prompt
	(vla-EndUndoMark activeDoc)  ; End group undo
	
	(setq *error* olderr)
	
	(princ)
)

(defun MM:get-file ( pathVar / *error* file charVar mainLst helpLst valueVar )
	;; Returns list of lists where every list is a row and list elements are a segments of that row divided by space
	(setq file (open pathVar "r")
		  valueVar "")
	
	(while (setq charVar (read-char file))  ; Get code of current character
		(setq charVar (chr charVar))  ; Convert code to character
		(cond
			((= charVar " ")  ; Check if character is 'space' (separator you can change i.e. comma if CSV)
				(if (vl-every '(lambda(x)(vl-string-search (chr x) "1234567890.")) (vl-string->list valueVar))  ; Check if sequence is a coordinate (consist only of numbers and dot)
					(setq helpLst (cons (atof valueVar) helpLst))  ; If coordinate - convert to float/real and add to list
					(setq helpLst (cons valueVar helpLst))  ; Else just add to list
				)
				(setq valueVar "")  ; Set as empty string
			)  ; End of first condition
			((= charVar "\n")  ; Check if character is 'newline'
				(setq helpLst (cons (atof valueVar) helpLst)  ; Coordinate - convert to float/real and add to list
					  mainLst (cons (reverse helpLst) mainLst)  ; Add line to main list
					  helpLst '()  ; Set as empty list
					  valueVar "")  ; Set as empty string
			)  ; End of second condition
			(t
				(setq valueVar (strcat valueVar charVar))  ; Add character to build a word/sequence
			)  ; End of third condition
		)
	)
	
	(close file)
	
	(setq helpLst (cons (atof valueVar) helpLst)  ; Coordinate - convert to float/real and add last coordinate to list
		  mainLst (cons (reverse helpLst) mainLst))  ; Add last line to main list
	
	(reverse mainLst)
)

(defun MM:transform-coordinates ( newCoors / entEname entLst plineCoors plineEnt plinePoints pointsArr cntVar )
	(cond 
		((= (substr (car newCoors) 1 1) "L")  ; Check if object is line
			(setq
				entEname (handent (substr (car newCoors) 2))  ; Get entity ename by hander
				entLst (entget entEname)  ; Get entity list of all properties
				entLst (subst (cons 10 (cadr newCoors)) (assoc 10 entLst) entLst)  ; Replace start coordinate with new transformed coordinate
			)
			(entmod (subst (cons 11 (caddr newCoors)) (assoc 11 entLst) entLst))  ; Replace end coordinate with new transformed coordinate and modify the entity
			(entupd entEname)  ; Update entity in the drawing (optional)
		)
		((= (substr (car newCoors) 1 1) "P")  ; Check if object is polyline
			(setq
				entEname (handent (substr (car newCoors) 2))  ; Get entity ename by hander
				plineCoors (cadr newCoors)  ; Get polyline new transformed coordinate
				plineEnt (vlax-ename->vla-object entEname)  ; Convert ename to VLA object
			)
			
			(if (= (vla-get-objectname plineEnt) "AcDb2dPolyline")  ; Check if object is 2Dpolyline
				(progn  ; If its 2Dpolyline then add z-coor for each vertex
					(setq cntVar 1)
					(while (<= cntVar (/ (length plineCoors) 2))  ; Loop to go through all vertex coordinates from back to front
						(setq
							plinePoints (cons 0.0 plinePoints)  ; Add z-coor for each pair
							plinePoints (cons (nth (+ (- (length plineCoors) (* cntVar 2)) 1) plineCoors) plinePoints)  ; Get y-coor and add to list of vertices
							plinePoints (cons (nth (- (length plineCoors) (* cntVar 2)) plineCoors) plinePoints)  ; Get x-coor and add to list of vertices
							cntVar (1+ cntVar)  ; Increase counter to go to next vertex coordinates
						)
					)
				)
				(setq plinePoints plineCoors)  ; Else leave it as it is
			)
			
			(setq pointsArr (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length plinePoints)))))  ; Make safearray of a size of all vertex coordinates
			
			(vlax-safearray-fill pointsArr plinePoints)  ; Add points to safearray
			(vlax-put plineEnt 'Coordinates pointsArr)  ; Put new coordinates to a polyline
			(vla-update plineEnt)  ; Update entity in the drawing (optional)
		)
		((vl-string-search (substr (car newCoors) 1 1) "BTC")  ; Check  if object is a block, text or circle
			(setq
				entEname (handent (substr (car newCoors) 2))  ; Get entity ename by hander
				entLst (entget entEname)  ; Get entity list of all properties
			)
			(command "_.MOVE" entEname "" (cdr (assoc 10 entLst)) (cdr newCoors))  ; Move the entity to a new transformed coordinates
		)
	)
)

;; "EXPORT_COORDINATES\" to export coordinates from HDKS5/HDKS6 coordinate system, all linked to their enities
;; "IMPORT_COORDINATES\" to import coordinates converted to HTRS96 and transform entities to new coordinates
(setq info_ReplaceByCoordinates (list 	'("EXPORT_COORDINATES" . "spremi koordinate u HDKS5/HDKS6 koordinatnom sustavu sa pripadajucim oznakama objekata (za T7D pretvaranje)")
										'("IMPORT_COORDINATES" . "uvezi koordinate u HTRS96 koordinatnom sustavu i prebaci objekte na nove koordinate")))

(princ)