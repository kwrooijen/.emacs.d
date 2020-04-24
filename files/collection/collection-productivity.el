;;; collection/collection-productivity.el -*- lexical-binding: t; -*-

(use-package company
  :straight t
  :config
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (require 'yasnippet-snippets)
  (setq yas-verbosity 1
        yas-wrap-around-region t)
  (yas-reload-all))

(use-package ws-butler
  :straight t
  :config
  (ws-butler-global-mode)
  ;; Disable aftersave
  (defun ws-butler-after-save ()))

(use-package key-chord
  :straight t
  :config
  (add-hook* 'prog-mode-hook (key-chord-mode 1))
  (add-hook* 'isearch-mode-hook (key-chord-mode 1))
  (key-chord-define-global "xs" (lambda ()
                                  (interactive)
                                  (evil-normal-state)
                                  (save-buffer))))

(use-package anzu
  :straight t
  :bind* (("M-%" . anzu-query-replace))
  :init
  (global-anzu-mode))


(use-package winner
  :straight t
  :config
  (winner-mode 1))

(use-package winum
  :straight t
  :bind* (("M-1" . winum-select-window-1)
          ("M-2" . winum-select-window-2)
          ("M-3" . winum-select-window-3)
          ("M-4" . winum-select-window-4)
          ("M-5" . winum-select-window-5)
          ("M-6" . winum-select-window-6)
          ("M-7" . winum-select-window-7)
          ("M-8" . winum-select-window-8)
          ("M-9" . winum-select-window-9))
  :config
  (winum-mode))

(use-package flycheck
  :straight t
  :custom-face
  (flycheck-error ((t (:underline "#ff6c6b"))))
  (flycheck-info ((t (:underline "#98be65"))))
  (flycheck-warning ((t (:underline "#ECBE7B"))))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-point-left
    (vector #b000000000
            #b000000000
            #b000000000
            #b000000000
            #b000000010
            #b000000110
            #b000001110
            #b000011110
            #b000111110
            #b000011110
            #b000001110
            #b000000110
            #b000000010
            #b000000000
            #b000000000
            #b000000000
            #b000000000))

  (flycheck-define-error-level 'warning
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-point-left
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-error)

  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-point-left
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)

  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe))

(use-package diff-hl
  :straight t
  :init
  (setq diff-hl-margin-symbols-alist
        '((insert . "▐") (delete . "▐") (change . "▐")
          (unknown . "▐") (ignored . "▐")))

  :custom-face
  (diff-hl-margin-insert ((t (:foreground "#5be56b" :inherit nil))) )
  (diff-hl-margin-delete ((t (:foreground "#e85555" :inherit nil))) )
  (diff-hl-margin-change ((t (:foreground "#fcb75d" :inherit nil))) )
  :config
  ;; (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package magit
  :straight t
  :config
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil))

(use-package projectile
  :straight t
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode))

(use-package web-mode
  :straight t
  :init
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4))

(use-package simpleclip
  :straight t
  :bind* (("M-c" . simpleclip-copy)
          ("M-v" . simpleclip-paste))
  :init
  (simpleclip-mode 1))

(use-package vc-hooks
  :init
  ;; follow symlinks and don't ask
  (setq vc-follow-symlinks t)
  ;; Don't use version control for all files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package highlight-numbers
  :straight t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package counsel
  :straight t
  :bind* (("M-x" . counsel-M-x))
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)))

(use-package highlight-symbol
  :straight t
  :init
  (setq highlight-symbol-idle-delay 0.1)
  :config
  (add-hook* 'prog-mode-hook (highlight-symbol-mode 1)))

(use-package rainbow-mode
  :straight t)

(use-package markdown-mode :straight t)
(use-package yaml-mode :straight t)
(use-package yaml-mode :straight t)
(use-package dockerfile-mode :straight t)
(use-package gitignore-mode :straight t)

(provide 'collection-productivity)
