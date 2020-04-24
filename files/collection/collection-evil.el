;;; collection/collection-evil.el -*- lexical-binding: t; -*-

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (require 'evil-collection)
  ;; This breaks company mode
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-visual-state-map (kbd "TAB") #'indent-region)

  (defun evil-recenter (&rest x)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (advice-add 'evil-scroll-page-down :after #'evil-recenter)
  ;; This prevents the screen from completely scrolling up
  ;; (advice-add 'evil-scroll-page-up :after #'evil-recenter)
  (advice-add 'evil-search-next :after #'evil-recenter)
  (advice-add 'evil-search-previous :after #'evil-recenter)
  (evil-mode 1))

(use-package evil-magit
  :straight t
  :after magit)

(use-package evil-nerd-commenter
  :straight t
  :bind* (("M-/" . evilnc-comment-or-uncomment-lines)))

(provide 'collection-evil)
