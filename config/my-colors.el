(load-theme 'zenburn t)

; Some custom colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(set-face-foreground 'flymake-errline		"red"	)
(set-face-foreground 'flymake-warnline		"yellow")
(set-face-foreground 'flyspell-incorrect        "red")
(set-face-foreground 'speedbar-directory-face   "cyan"	)
(set-face-foreground 'speedbar-file-face        "white")
(set-face-foreground 'font-lock-type-face       "#145e74")
(set-face-foreground 'font-lock-comment-face    "#707070")
(set-face-foreground 'font-lock-comment-delimiter-face    "#707070")
(set-face-foreground 'linum			"#707070")

(defpowerline god-mode-bar
  (if mark-active
      (format "[VISUAL]")
      (if (and (boundp 'god-local-mode) god-local-mode)
               (format "[NORMAL]")
               (format "[INSERT]"))))

(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
    '("%e"
      (:eval
       (let* ((active (powerline-selected-window-active))
              (mode-line (if active 'mode-line 'mode-line-inactive))
              (face1 (if active 'powerline-active1 'powerline-inactive1))
              (face2 (if active 'powerline-active2 'powerline-inactive2))
              (separator-left (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (car powerline-default-separator-dir))))
              (separator-right (intern (format "powerline-%s-%s"
                                               powerline-default-separator
                                               (cdr powerline-default-separator-dir))))

              (lhs (list (powerline-raw "%*" nil 'l)
                         (powerline-raw mode-line-mule-info nil 'l)
                         (powerline-buffer-id nil 'l)
                         (god-mode-bar nil 'l)
                         (when (and (boundp 'which-func-mode) which-func-mode)
                           (powerline-raw which-func-format nil 'l))
                         (powerline-raw " ")
                         (funcall separator-left mode-line face1)
                         (when (boundp 'erc-modified-channels-object)
                           (powerline-raw erc-modified-channels-object face1 'l))
                         (powerline-major-mode face1 'l)
                         (powerline-process face1)
                         (powerline-narrow face1 'l)
                         (powerline-raw " " face1)
                         (funcall separator-left face1 face2)
                         (powerline-vc face2 'r)
))
              (rhs (list (funcall separator-right face2 face1)
                         (powerline-raw "%3l:%2c " face1 'l)
                         (funcall separator-right face1 mode-line)
                         (powerline-raw " ")
                         (powerline-raw "%7p" nil 'r)
                         (powerline-hud face2 face1)
)))
         (concat (powerline-render lhs)
                 (powerline-fill face2 (powerline-width rhs))
                 (powerline-render rhs)
))))))


; Enable my theme
(my-powerline-theme)

(provide 'my-colors)
