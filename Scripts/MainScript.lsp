(vl-load-com)

(setq MM_MAIN_SCRIPT_NAME "MainScript.lsp"
	  MM_MAIN_SCRIPT_PATH (vl-filename-directory (findfile MM_MAIN_SCRIPT_NAME))
)

(defun MM:load ( / lispFiles )
	;; Get all scripts from directory where is main script, except the main script itself, and then load them all
	(setq lispFiles (vl-remove-if '(lambda(fileName)(= fileName MM_MAIN_SCRIPT_NAME)) (vl-directory-files MM_MAIN_SCRIPT_PATH "*.lsp")))
	
	(mapcar 'load lispFiles)
	
	(princ)
)

(MM:load)

(defun C:reload_lisps ( / ) (MM:load))

(defun MM:info-box ( args / )
	
	(setq openFile (open (setq fileName (vl-filename-mktemp "" "" ".dcl")) "w") ; Open temporarily .dcl file for writing
		  args (vl-sort (apply 'append (mapcar '(lambda(arg)(eval (read (strcat "info_" arg)))) args)) '(lambda(a b)(< (car a)(car b))))) ; Sort all arguments alphabetic
	
	(setq DCLcode
		(reverse
			(list 
				"info_diag : dialog {"
				"	label = \"Info Dialog\";"
				"	width = \"3\";"
				"	spacer;"
				"	: row {"
				"		fixed_width  = true;"
				"		alignment = centered;"
			)
		)
	)
	
	(setq DCLcode (cons "	: column {" DCLcode))
	
	(foreach arg args
		(foreach item
			(list
				"		spacer;"
				"		: text {"
				"			alignment = right;"
		(strcat "			label = \"" (car arg) "\";")
				"		}"
			)
			
			(setq DCLcode (cons item DCLcode))
		)
	)
	
	(setq DCLcode (cons "	}" DCLcode)
		  DCLcode (cons "	: column {" DCLcode))
	
	(foreach arg args
		(foreach item
			(list
				"		spacer;"
				"		: text {"
				"			alignment = centered;"
		(strcat "			label = \"--->\";")
				"		}"
			)
			
			(setq DCLcode (cons item DCLcode))
		)
	)
	
	(setq DCLcode (cons "	}" DCLcode)
		  DCLcode (cons "	: column {" DCLcode))
	
	(foreach arg args
		(foreach item
			(list
				"		spacer;"
				"		: text {"
				"			alignment = left;"
		(strcat "			label = \"" (cdr arg) "\";")
				"		}"
			)
			
			(setq DCLcode (cons item DCLcode))
		)
	)
	
	(setq DCLcode (cons "	}" DCLcode)
		  DCLcode (cons "	}" DCLcode)
		  DCLcode (cons "	spacer_1;" DCLcode)
		  DCLcode (cons "	ok_only;" DCLcode)
		  DCLcode (cons "	spacer_1;" DCLcode)
		  DCLcode (cons "}" DCLcode)
		  DCLcode (reverse DCLcode))
	
	(foreach text_line DCLcode (write-line text_line openFile)) ; Iterate through all lines and write each to an open file

	(close openFile)  ; Close temporarily .dcl file
	
	(new_dialog "info_diag" (setq dialogObject (load_dialog fileName)))
	
	(setq dialogValue (start_dialog))
	
	(unload_dialog dialogObject)
	
	(vl-file-delete fileName)
	
	(princ)
)

(defun C:INFO ( / )
	(MM:info-box (mapcar 'vl-filename-base (vl-remove-if '(lambda(x)(= x MM_MAIN_SCRIPT_NAME)) (vl-directory-files MM_MAIN_SCRIPT_PATH "*.lsp"))))
	(princ)
)

(defun C:, ( / ) (sssetfirst nil (ssget "_P"))(princ))

(defun C:SAQ ( / ) (command "QSAVE" "QUIT"))