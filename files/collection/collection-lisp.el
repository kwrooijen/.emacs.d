;;; collection/collection-lisp.el -*- lexical-binding: t; -*-

(use-package paredit
  :straight t)

(use-package lispyville
  :straight t
  :config
  (lispyville--define-key 'normal (kbd "M-J") #'mc/mark-next-like-this)
  (lispyville--define-key 'normal (kbd "M-K") #'mc/mark-previous-like-this))

(use-package lispy
  :straight t
  :config
  (defun lispy--mode-p ()
    (or (lispy-left-p)
        (lispy-right-p)))

  (defun lispy-brackets-or-barf (arg)
    (interactive "P")
    (if (lispy--mode-p)
        (lispy-barf 1)
      (lispy-brackets arg)))

  (defun lispy-left-insert ()
    (interactive)
    (when (not (lispy--mode-p))
      (lispy-left 1))
    (when (not (lispy--mode-p))
      (beginning-of-defun))
    (when (lispy--mode-p)
      (evil-insert-state 1)))

  (defun lispy-o ()
    (interactive)
    (when (lispy-left-p)
      (lispy-different))
    (lispy-newline-and-indent-plain))

  (defun lispy-global-teleport (arg)
    (interactive "p")
    (let ((lispy-teleport-global t))
      (lispy-teleport arg)))

  ;; This breaks company mode
  (define-key lispy-mode-map (kbd "C-k") nil)

  (evil-define-key 'insert lispy-mode-map "]" #'lispy-slurp)
  (evil-define-key 'insert lispy-mode-map "[" #'lispy-brackets-or-barf)
  (evil-define-key 'insert lispy-mode-map "{" #'lispy-braces)
  (evil-define-key 'insert lispy-mode-map (kbd "C-d") #'lispy-describe-inline)
  (evil-define-key 'insert lispy-mode-map (kbd "C-e") #'lispy-arglist-inline)
  (evil-collection-define-key 'normal 'lispy-mode-map (kbd "M-d") #'lispy-kill-word)
  (evil-collection-define-key 'normal 'lispy-mode-map (kbd "D") #'paredit-kill)
  (lispy-define-key lispy-mode-map "o" 'lispy-o)
  (lispy-define-key lispy-mode-map "d" 'lispy-different)
  (lispy-define-key lispy-mode-map "i" 'indent-sexp)
  (lispy-define-key lispy-mode-map "x" 'lispy-delete)
  (lispy-define-key lispy-mode-map "A" 'lispy-ace-symbol-replace)
  (lispy-define-key lispy-mode-map "H" 'special-lispy-move-left)
  (lispy-define-key lispy-mode-map "J" 'special-lispy-down-slurp)
  (lispy-define-key lispy-mode-map "K" 'special-lispy-up-slurp)
  (lispy-define-key lispy-mode-map "L" 'special-lispy-move-right)
  (lispy-define-key lispy-mode-map "I" 'evil-insert-state)
  (lispy-define-key lispy-mode-map "T" 'lispy-global-teleport)
  (define-key lispy-mode-map (kbd "M-a") 'lispy-left-insert)

  (defface paren-face
    '((((class color) (background dark))
       (:foreground "gray20"))
      (((class color) (background light))
       (:foreground "gray80")))
    "Face used to dim parentheses.")

  (add-hook* 'lispy-mode-hook
             (font-lock-add-keywords nil '((")" . 'paren-face)))
             (font-lock-add-keywords nil '(("}" . 'paren-face)))
             (font-lock-add-keywords nil '(("]" . 'paren-face))))

  (advice-add 'special-lispy-down :after #'kwrooijen/recenter)
  (advice-add 'special-lispy-up :after #'kwrooijen/recenter)
  (add-hook 'lispy-mode-hook #'show-paren-mode)
  (add-hook 'lispy-mode-hook #'paredit-mode)
  (add-hook 'lispy-mode-hook #'lispyville-mode))

(provide 'collection-lisp)
