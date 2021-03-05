;;; collection/collection-editing.el -*- lexical-binding: t; -*-

(use-package wrap-region
  :straight t
  :config
  (wrap-region-global-mode))

(use-package expand-region
  :straight t
  :bind* (("M-@" . er/expand-region)))

(use-package multiple-cursors
  :straight (multiple-cursors
             :host github
             :repo "kwrooijen/mc"
             :files (:defaults (:exclude "*.el.in")))
  :bind* (("M-K" . mc/mark-previous-like-this)
          ("M-J" . mc/mark-next-like-this))
  :config
  (multiple-cursors-mode t))

(use-package undo-tree
  :straight t
  :bind (("M-u" . undo-tree-redo))
  :config
  (global-undo-tree-mode)
  (add-hook* 'prog-mode-hook (undo-tree-mode 1)))

(provide 'collection-editing)
