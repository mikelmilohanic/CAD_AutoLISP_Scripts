(defun ssintersect (ss1 ss2 / e ss3)
    (setq ss3 (ssadd))
    (cond
		((not (or ss1 ss2)) ss3)
		((not ss1) ss2)
		((not ss2) ss1)
		(t
			(foreach ent (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss1)))
				(if (ssmemb ent ss2) (ssadd ent ss3))
			)
			ss3
		)
	)
)

(defun C:WS ( / *error* oldErr fPnt ssSimilarSelect ssWindow )
	(defun LM:error ( msg )
		(if (= 8 (logand (getvar 'undoctl) 8))
			(vla-EndUndoMark activeDoc)
		)
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(setq *error* oldErr)
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)
	
	(C:SELECTSIMILAR)
	(command "DESELECT")
	
	(setq ssSimilarSelect (ssget "_P"))
	
	(setq ssWindow
		(ssget
			"_W"
			(setq fPnt (getpoint "Pick first point:"))
			(getcorner fPnt "Pick second point:")
		)
	)
	
	(sssetfirst nil (ssintersect ssSimilarSelect ssWindow))
	
	(setq *error* oldErr)
	
	(princ)
)


;; "WS" to get SimilarSelect inside a targeted area
(setq info_WindowSelectSimilar (list '("WS" . "SimilarSelect unutar oznacenog podrucja")))

(princ)