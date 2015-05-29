;;==============================================================================
;;== Keys
;;==============================================================================

(define-key erc-mode-map (kbd "C-M-m") 'erc-send-current-line)
(define-key erc-mode-map (kbd "RET") (lambda() (interactive)(message "Use C-M-m to send")))

;;==============================================================================
;;== Options
;;==============================================================================

(erc-truncate-mode 1)
(erc-scrolltobottom-mode 1)

(setq erc-nick "attichacker")
(setq erc-prompt-for-password nil)
(setq erc-ignore-list '("*Flowdock*" "Flowdock" "-Flowdock-"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;==============================================================================
;;== Hook
;;==============================================================================

;;==============================================================================
;;== Functions
;;==============================================================================

(defun flowdock ()
  (interactive)
  (let* ((email "kevin.vanrooijen@spilgames.com")
         (password (read-passwd "Flowdock Password: "))
         (full-password (concat email " " password)))
    (setq-default erc-ignore-list '("*Flowdock*" "Flowdock" "-Flowdock-"))
    (setq-default erc-hide-list '("JOIN" "PART" "QUIT"))
    (erc-ssl :server "irc.flowdock.com"
             :nick "KevinR"
             :port 6697
             :password full-password)))

;;==============================================================================
;;== Advice
;;==============================================================================

(defadvice erc (before erc activate)
  (setq erc-prompt-for-password nil)
  (load "~/.erc.gpg")
  (setq erc-password ercpass))

(provide 'attic-erc)
