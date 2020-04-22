;;; collection/collection-productivity.el -*- lexical-binding: t; -*-

(use-package company
  :straight t
  :config
  (define-key company-active-map (kbd "<return>") nil))

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
  :config
  ;; TODO Make this smaller
  (define-fringe-bitmap 'flycheck-fringe-bitmap-point-left
    (vector #b00000000
            #b00000001
            #b00000011
            #b00000111
            #b00001111
            #b00011111
            #b00111111
            #b01111111
            #b11111111
            #b01111111
            #b00111111
            #b00011111
            #b00001111
            #b00000111
            #b00000011
            #b00000001
            #b00000000))

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
  (add-hook* 'magit-status-mode-hook (set-window-fringes (selected-window) 0 0 nil))
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

(use-package markdown-mode
  :straight t)

(use-package simpleclip
  :straight t
  :bind* (("M-c" . simpleclip-copy)
          ("M-v" . simpleclip-paste))
  :init
  (simpleclip-mode 1))


(provide 'collection-productivity)
