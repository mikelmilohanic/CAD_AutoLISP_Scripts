(defun C:UAL ( / )
	(setvar "cmdecho" 0)
	(command "-LAYER" "U" "*" "")
	(setvar "cmdecho" 1)
	(princ)
)

;; "UAL" to unlock all layers
(setq info_UnlockAllLayers (list '("UAL" . "otkljucavanje svih layera u crtezu")))

(princ)