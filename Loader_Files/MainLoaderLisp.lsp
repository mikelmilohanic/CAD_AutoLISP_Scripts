;; Write text files with different charsets by VovKa
(defun vk_WriteTextStream (Stream FileName Charset / ADODBStreamObj Result)
  (if (setq ADODBStreamObj (vlax-create-object "ADODB.Stream"))
    (progn (setq Result	(vl-catch-all-apply
			  (function
			    (lambda ()
			      (vlax-put ADODBStreamObj "Charset" Charset)
			      (vlax-invoke ADODBStreamObj "Open")
			      (vlax-invoke-method ADODBStreamObj "WriteText" Stream 0)
			      (vlax-invoke ADODBStreamObj "SaveToFile" FileName 2)
			    )
			  )
			)
	   )
	   (vlax-release-object ADODBStreamObj)
	   (if (not (vl-catch-all-error-p Result))
	     FileName
	   )
    )
  )
)

;;------------------------------------------------------------;;

;; Read text files with different charsets by VovKa
(defun vk_ReadTextStream (FileName Charset / ADODBStreamObj Result)
  (if (and FileName
	   (setq ADODBStreamObj (vlax-create-object "ADODB.Stream"))
      )
    (progn (setq Result	(vl-catch-all-apply
			  (function
			    (lambda ()
			      (vlax-put ADODBStreamObj "Charset" Charset)
			      (vlax-invoke ADODBStreamObj "Open")
			      (vlax-invoke ADODBStreamObj "LoadFromFile" FileName)
			      (vlax-invoke-method ADODBStreamObj "ReadText" -1)
			    )
			  )
			)
	   )
	   (vlax-release-object ADODBStreamObj)
	   (if (not (vl-catch-all-error-p Result))
	     Result
	   )
    )
  )
)

;;------------------------------------------------------------;;

(setq dwgPath (vl-string-right-trim "\\" (getvar 'DWGPREFIX))
	  scriptsPath (strcat (vl-filename-directory dwgPath) "\\Scripts")  ; Get the path of the scripts directory
	  appLoadFilePath (findfile "../AppAutoLoad.app")  ; Get the path of ZWCAD appload startup suite file
	  filesVar (vla-get-files (vla-get-preferences (vlax-get-acad-object))))  ; Get 'files' object

(vlax-put filesVar 'supportpath (strcat (vl-string-right-trim ";" (vla-get-supportpath filesVar)) ";" scriptsPath ";"))  ; Add path with Lisp scripts to supported paths

(if appLoadFilePath
	(progn
		;; Append new content to startup suite
		(vk_WriteTextStream (strcat (vk_ReadTextStream appLoadFilePath "unicode") (strcat "\n" scriptsPath "\\MainScript.lsp\n")) appLoadFilePath "unicode")
		
		(alert "Scripts will be loaded upon next CAD opening, please save and close all open CAD drawings, and close all CAD applications instances.
				\nScripte ce biti ucitane pri sljedecem pokretanju CAD-a, molim da spremite i zatvorite sve otvorene CAD crteze, i da zatvorite sve instance CAD aplikacije.")
	)
	;; ELSE
	(progn
		(alert  "<<< Error/Greska!!! >>>\n
				\nCan't find ZWCAD appload Startup Suite file 'AppAutoLoad.app', you will need to load 'MainScript.lsp' manually, for instructions look into 'ManualAddingInstructions.txt'.
				\nPotrebne datoteke nisu pronadene stoga je potrebno rucno dodati 'MainScript.lsp' u Startup Suite, za upute mozete pronaci u tekstualnoj datoteci 'ManualAddingInstructions.txt'.")
		(exit)
	)
)

(princ)