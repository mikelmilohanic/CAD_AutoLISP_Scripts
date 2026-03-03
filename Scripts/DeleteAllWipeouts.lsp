(defun C:DAW ( / )
	(if (setq ss (ssget "_X" '((0 . "WIPEOUT"))))
		(progn
			(mapcar 'entdel (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
			(princ (strcat "\nUspjesno obrisano " (itoa (sslength ss)) " WIPEOUT objekata."))
		)
		(princ "\U crtezu nije pronadeno WIPEOUT objekata.")
	)
	(princ)
)

;; "DAW" to delete all WIPEOUT objects from a drawing
(setq info_DeleteAllWipeouts (list '("DAW" . "brisanje svih WIPEOUT objekata iz crteza")))

(princ)