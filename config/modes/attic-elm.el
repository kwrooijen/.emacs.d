;;==============================================================================
;;== Hook
;;==============================================================================

(add-hook 'elm-mode-hook (lambda ()
    (fix-tabs 4)
    (default-language-settings)
    (attic-lock)
    (elm-indentation-mode 0)
    (setq iod--use-tab-cycle t)))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun elm-reactor ()
  (interactive)
  (async-shell-command "elm-reactor" "*elm-reactor*"))

(provide 'attic-elm)
