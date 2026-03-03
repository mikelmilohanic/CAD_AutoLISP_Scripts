(defun C:CTX ( / *error* oldErr userSelection allText uniqueText counterVar )
	
	(defun LM:error ( msg )
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
			(princ (strcat "\nError: " msg))
		) ; if end
		(princ)
	)
	
	(setq oldErr *error*
          *error* LM:error)

	(while (not (setq userSelection (ssget '((0 . "TEXT,MTEXT")))))  ; Select Text and MText objects
		(princ "\nNo objects selected. Try again.")
	)
	
	(setq allText (mapcar '(lambda(x)(vl-string-trim " " (cdr (assoc 1 x)))) (mapcar 'entget (vl-remove-if 'listp (mapcar 'cadr (ssnamex userSelection)))))  ; Get list of all texts
		  uniqueText (LM:Unique allText))  ; Get list of unique texts
	
	(princ "\nText appearance count:")
	(foreach text uniqueText  ; Loop through all unique texts
		(setq counterVar 0)  ; Set counter to zero
		(foreach item allText  ; Loop through all unique texts
			(if (= item text)  ; Check if text is same as target unique text
				(setq counterVar (1+ counterVar))  ; Increase counter if it is
			)
		)
		(princ (strcat "\n" text " ---> " (itoa counterVar)))  ; Print total of appearances of text object
	)
	
	(setq *error* oldErr)
	
	(princ)
)

(defun LM:Unique ( lst )  ; 'Unique' by Lee Mac - Returns a list with duplicate elements removed.
    (if lst (cons (car lst) (LM:Unique (vl-remove (car lst) (cdr lst)))))
)

;; "CTX" to count text appearances
(setq info_CountTextAppearances (list '("CTX" . "prebrojavanje Text/Mtext objekata ovisno o tome sadrze li iste znakove")))

(princ)
