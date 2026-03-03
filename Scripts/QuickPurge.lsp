(defun C:QP ( / )
	(setvar "cmdecho" 0)
	(command "-PURGE" "A" "*" "N")
	(setvar "cmdecho" 1)
	
	(princ)
)

;; "QP" to perform quick-purge
(setq info_QuickPurge (list  '("QP" . "izvodenje naredbe 'Purge' nad svim nekoristenim objektima (quick-purge)")))

(princ)