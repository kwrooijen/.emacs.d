;;==============================================================================
;;== Hook
;;==============================================================================

(add-hook 'elm-mode-hook (lambda ()
    (fix-tabs 4)
    (default-language-settings)
    (god-local-mode t)
    (elm-indentation-mode 0)
    (setq doom-indent-fallback 0)
    (setq doom-region-cycle t)
    (setq-local doom-indent-fallback t)))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun elm-reactor ()
  (interactive)
  (async-shell-command "elm-reactor" "*elm-reactor*"))

(provide 'attic-elm)
