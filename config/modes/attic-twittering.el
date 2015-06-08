;;==============================================================================
;;== Keys
;;==============================================================================

(define-key twittering-mode-map (kbd "s") 'twittering-search)
(define-key twittering-mode-map (kbd ";") 'semi-colon-map)
(define-key twittering-mode-map (kbd "q") (lambda () (interactive) (switch-to-buffer nil)))
(define-key twittering-mode-map (kbd "w") 'delete-window)

;;==============================================================================
;;== Options
;;==============================================================================

(setq twittering-icon-mode t)
;; Use master password for twitter instead of authenticating every time
(setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")
(setq twittering-use-master-password t)

;;==============================================================================
;;== Hooks
;;==============================================================================

(add-hook 'twittering-mode-hook 'toggle-modeline)

(provide 'attic-twittering)
