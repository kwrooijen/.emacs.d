;;==============================================================================
;;== Keys
;;==============================================================================

(define-key gnus-summary-mode-map (kbd ";") 'semi-colon-map)
(define-key gnus-summary-mode-map (kbd "z") 'helm-mini)

(define-key gnus-group-mode-map (kbd ";") 'semi-colon-map)
(define-key gnus-group-mode-map (kbd "z") 'helm-mini)

;;==============================================================================
;;== Advice
;;==============================================================================

(defadvice gnus (after gnus activate)
  (gnus-demon-init))

;;==============================================================================
;;== Options
;;==============================================================================

;; In Group buffer press "G p" and add this to the list (modeline-notify t)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
(setq gnus-demon-timestep 1)
(gnus-demon-add-handler 'gnus-group-get-new-news 30 3)
(setq gnus-always-read-dribble-file t)
(fmakunbound 'gnus-group-delete-articles) ;; Because no.

(provide 'attic-gnus)

