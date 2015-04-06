;;==============================================================================
;;== Keys
;;==============================================================================

(define-key twittering-mode-map (kbd "s") 'twittering-search)
(define-key twittering-mode-map (kbd ";") 'semi-colon-map)

;;==============================================================================
;;== Options
;;==============================================================================

(setq twittering-icon-mode t)    
;; Use master password for twitter instead of authenticating every time
(setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")
(setq twittering-use-master-password t)

(provide 'attic-twittering)
