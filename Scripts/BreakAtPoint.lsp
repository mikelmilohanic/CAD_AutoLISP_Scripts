(vl-load-com)

(defun c:BAP (/ entity point)
	(princ "\nSelect source block: ")
	(while (not (setq entity (ssget ":E:S" '((0 . "LINE,*POLYLINE,ARC,SPLINE")))))  ; Loop while Line/Polyline/Arc/Spline entity isn't seleceted
		(princ "\n<!> Invalid entity. Select one of following --> Line/Polyline/Arc/Spline. <!> ")
	); end while
	
	
	(setq entity (ssname entity 0)
		  point (getpoint "\nSelect a point on entity where you want to break the entity: "))
	
	(command "_.break" entity point point)
	
	(princ)
)

(defun c:BM ( / *error* oldErr activeDoc entities entity entityVLA midpoint )

	(defun LM:error ( msg )
		(if (= 8 (logand (getvar 'undoctl) 8))
			(vla-EndUndoMark activeDoc)
		)
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(princ)
	)
	
	(princ "\nSelect source block: ")
	(while(not (setq entities (ssget ":S" '((0 . "LINE")))))  ; Loop while line entity isn't seleceted
		(princ "\n<!> Invalid entity. Please select line. <!> ")
	); end while
	
	(setq activeDoc (vla-get-activedocument (vlax-get-acad-object)))
	
	(setvar "cmdecho" 0)  ; Disable terminal prompt
	(vla-StartUndoMark activeDoc)  ; Start undo mark/group
	
	(setq oldErr *error*
		  *error* LM:error)
		  
	(mapcar
		(lambda(entity)
			(setq entityVLA (vlax-ename->vla-object entity)  ; Convert ename to VLA object
				  midpoint (vlax-curve-getPointatParam entityVLA (/ (+ (vlax-curve-getEndParam entityVLA) (vlax-curve-getStartParam entityVLA)) 2.)))  ; Get midpoint
			(command "_.break" entity midpoint midpoint)
		)
		(vl-remove-if 'listp (mapcar 'cadr (ssnamex entities)))  ; Get the list of objects enames
	)
	
	(setq *error* oldErr)
	
	(vla-EndUndoMark activeDoc)  ; End undo mark/group
	(setvar "cmdecho" 1)  ; Enable terminal prompt
	
	(princ)
)

;; "BAP" to break Line/Polyline/Arc/Spline at choosen point
;; "BM" to break line at half
(setq info_BreakAtPoint (list '("BAP" . "lomljenje Line, Polyline, Arc ili Spline objekta u odabranoj tocki")
								'("BM" . "prepolavljanje Line objekta")))

(princ)