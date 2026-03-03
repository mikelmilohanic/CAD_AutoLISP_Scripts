(defun C:RB (/ iCnt bSet cFlg nBlc cVal pLst bNam aLst aDoc nBlc aSp cAt rLst)

	(vl-load-com)
	
	(defun Set_Initial_Setenv(varLst)
		(mapcar '(lambda(v)(if(not(getenv(car v)))(setenv(car v)(cadr v))))varLst)
	); end of Set_Initial_Setenv
	
	(defun Unblock_All_Layers(/ aDoc layCol actLay outLst)
		(setq aDoc(vla-get-ActiveDocument
				(vlax-get-acad-object))
				layCol(vla-get-Layers aDoc)
				actLay(vla-get-ActiveLayer aDoc)
			); end setq
		(vlax-map-collection layCol
			(function
				(lambda(x)
					(setq outLst
						(cons (list x (vla-get-Lock x) (vla-get-Freeze x)) outLst)
					); end setq
					(vla-put-Lock x :vlax-false)
					(if(not(equal x actLay))
						(vla-put-Freeze x :vlax-false)
					); end if
				); end lambda
			); end function
		); end vlax-map-collection
		outLst
	); end of Unblock_All_Layers
	
	(defun Restore_All_Layer_States(Lst / actLay)
		(setq actLay(vla-get-ActiveLayer (vla-get-ActiveDocument (vlax-get-acad-object))))
		(mapcar
			(function
				(lambda(x)
					(vla-put-Lock(car x)(cadr x))
					(if(not(equal actLay(car x)))
						(vla-put-Freeze(car x)(last x))
					); end if
				)
			) Lst
		)
		(princ)
	); end of Restore_All_Layer_States
	
	(princ "\nSelect source block: ")
	(while(not (setq nBlc (ssget ":E:S" '((0 . "INSERT")))))  ; Loop while 'block' entity isn't seleceted
		(princ "\n<!> This isn't block <!> ")
	); end while
	
	(setq nBlc (vlax-ename->vla-object (cadr (car (ssnamex nBlc)))))  ; Convert entity to VLA object
	
	(Set_Initial_Setenv '(("xchange:layer" "Yes")("xchange:scale" "Yes")
						("xchange:rotation" "Yes")("xchange:attributes" "Yes")))
  
	;; Loop to allow selecting multiple destination block entities
    (while
        (progn
			(princ
			(strcat "\nOptions: Layer = "(getenv "xchange:layer")
					", Scale = " (getenv "xchange:scale")
					", Rotation = " (getenv "xchange:rotation")
					", Attributes = " (getenv "xchange:attributes")))
			(setvar 'errno 0)
			(initget "Multiple Options Exit")
            (setq bSet (entsel "\nSelect destination block [Multiple/Options/Exit] <Exit>: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                ); end condition #1
                (   (or (null bSet) (= "Exit" bSet))
                    nil
                ); end condition #2
				(   (= "Options" bSet)
					(initget "Yes No")
					(setq cVal(getkword(strcat "\nInherit old block layer [Yes/No] <"
								(getenv "xchange:layer")">: ")))
					(if(member cVal '("Yes" "No"))(setenv "xchange:layer" cVal))
					(initget "Yes No")
					(setq cVal(getkword(strcat "\nInherit old block scale [Yes/No] <"
								(getenv "xchange:scale")">: ")))
					(if(member cVal '("Yes" "No"))(setenv "xchange:scale" cVal))
					(initget "Yes No")
					(setq cVal(getkword(strcat "\nInherit old block rotation [Yes/No] <"
								(getenv "xchange:rotation")">: ")))
					(if(member cVal '("Yes" "No"))(setenv "xchange:rotation" cVal))
					(initget "Yes No")
					(setq cVal(getkword(strcat "\nInherit attributes with similar tags [Yes/No] <"
								(getenv "xchange:attributes")">: ")))
					(if(member cVal '("Yes" "No"))(setenv "xchange:attributes" cVal))
				); end condition #3
				(   (= "Multiple" bSet)
					(princ "\n<<< Select blocks to replace >>> ")
					(if(setq bSet(ssget '((0 . "INSERT"))))
						(progn
							(setq aDoc(vla-get-ActiveDocument(vlax-get-acad-object))
								bNam(vla-get-Name nBlc)
								aSp(vla-ObjectIdToObject aDoc(vla-get-OwnerId nBlc))
								iCnt 0
							); end setq
							
							(vla-StartUndoMark aDoc)
							
							(setq rLst(Unblock_All_Layers))
							
							(foreach b (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr(ssnamex bSet))))
								(if(= :vlax-true(vla-get-HasAttributes b))
									(setq aLst
										(mapcar '(lambda (a) (list (vla-get-TagString a) (vla-get-TextString a)))
											(vlax-safearray->list (vlax-variant-value (vla-GetAttributes b)))
										)
									)
								); end if
								
								(setq nBlc(vla-InsertBlock aSp (vla-get-InsertionPoint b)bNam 1.0 1.0 1.0 0.0))
								
								(if(= "Yes"(getenv "xchange:layer"))
									(vla-put-Layer nBlc(vla-get-Layer b)) ; Set layer as source
								); end if
								
								(if(= "Yes"(getenv "xchange:scale"))
									(progn
										(vla-put-XScaleFactor nBlc(vla-get-XScaleFactor b)) ; Set y-coor as source
										(vla-put-YScaleFactor nBlc(vla-get-YScaleFactor b)) ; Set x-coor as source
										(vla-put-ZScaleFactor nBlc(vla-get-ZScaleFactor b)) ; Set z-coor as source
									); end progn
								); end if
								
								(if(= "Yes"(getenv "xchange:rotation"))
									(vla-put-Rotation nBlc(vla-get-Rotation b)) ; Set rotation as source
								); end if
								
								(if (and
									(= "Yes"(getenv "xchange:attributes"))
									(= :vlax-true(vla-get-HasAttributes nBlc))
									); end and
									(foreach i(mapcar '(lambda (a)(list(vla-get-TagString a)a))
										(vlax-safearray->list (vlax-variant-value(vla-GetAttributes nBlc))))
										(if(setq cAt(assoc(car i)aLst))
											(vla-put-TextString(last i)(last cAt))
										); end if
									); end foreach
								); end if <--- Set attributes as source (if there's any)
								
								(vla-Delete b)
								(setq iCnt(1+ iCnt))
							); end foreach
							
							(Restore_All_Layer_States rLst)
							
							(vla-EndUndoMark aDoc)
							
							(princ(strcat "\n" (itoa iCnt) " block(s) was replaced. "))
						); end progn
						(princ "\n<!> Nothing selected <!>" )
					); end if
				); end condition #4
                (   (and (= 'LIST(type bSet)) (equal '(0 . "INSERT")(assoc 0(entget(car bSet)))))
					(setq bSet(vlax-ename->vla-object(car bSet)))
                    (setq aDoc(vla-get-ActiveDocument(vlax-get-acad-object))
						bNam(vla-get-Name nBlc)
						aSp(vla-ObjectIdToObject aDoc(vla-get-OwnerId nBlc))
					); end setq
					
					(vla-StartUndoMark aDoc)
					
					(setq rLst(Unblock_All_Layers))
					
					(if(= :vlax-true(vla-get-HasAttributes bSet))
						(setq aLst
							(mapcar '(lambda (a) (list (vla-get-TagString a) (vla-get-TextString a)))
								(vlax-safearray->list (vlax-variant-value (vla-GetAttributes bSet)))
							)
						)
					); end if
					
					(setq nBlc(vla-InsertBlock aSp (vla-get-InsertionPoint bSet)bNam 1.0 1.0 1.0 0.0))
					
					(if(= "Yes"(getenv "xchange:layer"))
					(	vla-put-Layer nBlc(vla-get-Layer bSet)) ; Set layer as source
					); end if
					
					(if (= "Yes"(getenv "xchange:scale"))
						(progn
							(vla-put-XScaleFactor nBlc(vla-get-XScaleFactor bSet)) ; Set y-coor as source
							(vla-put-YScaleFactor nBlc(vla-get-YScaleFactor bSet)) ; Set x-coor as source
							(vla-put-ZScaleFactor nBlc(vla-get-ZScaleFactor bSet)) ; Set z-coor as source
						); end progn
					); end if
					
					(if(= "Yes"(getenv "xchange:rotation"))
						(vla-put-Rotation nBlc(vla-get-Rotation bSet)) ; Set rotation as source
					); end if
					
					(if (and
						(= "Yes"(getenv "xchange:attributes"))
						(= :vlax-true(vla-get-HasAttributes nBlc))
						); end and
						(foreach i(mapcar '(lambda (a)(list(vla-get-TagString a)a))
							(vlax-safearray->list (vlax-variant-value(vla-GetAttributes nBlc))))
							(if(setq cAt(assoc(car i)aLst))
								(vla-put-TextString(last i)(last cAt))
							); end if
						); end foreach
					); end if <--- Set attributes as source (if there's any)
					
					(vla-Delete bSet)
					
					(Restore_All_Layer_States rLst)
					
					(vla-EndUndoMark aDoc)
					
					(princ(strcat "\nBlock successfully replaced."))
                ); end condition #5
                (	(= 'LIST(type nBlc))
					(princ "\n<!> This isn't block <!> ")
				); end condition #6
            )
        )
    )

    ;; If the user presses Enter without selecting a destination, the loop ends
    (princ "\nNo more destination blocks selected. Exiting.")
	(princ)
)

;; "RB" to select object and replace other objects with selected.
(setq info_ReplaceBlock (list '("RB" . "neogranicena zamjena blokova s odabranim blokom (moguca prilagodba postavki zadrzavanja layera, skale itd.)")))

(princ)
