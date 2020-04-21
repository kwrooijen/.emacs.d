;;
;; Init
;;

(setq gc-cons-threshold most-positive-fixnum)
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;
;; Helpers
;;

(defmacro add-hook* (mode &rest body)
  `(add-hook ,mode (lambda () ,@body)))

;;
;; Packages
;;

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (require 'evil-collection)
  ;; This breaks company mode
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (defun evil-recenter (&rest x)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (advice-add 'evil-scroll-page-down :after #'evil-recenter)
  (advice-add 'evil-scroll-page-up :after #'evil-recenter)
  (advice-add 'evil-search-next :after #'evil-recenter)
  (advice-add 'evil-search-previous :after #'evil-recenter)
  (evil-mode 1))

(use-package vi-tilde-fringe
  :straight t
  :config
  (add-hook 'text-mode-hook #'vi-tilde-fringe-mode)
  (add-hook 'prog-mode-hook #'vi-tilde-fringe-mode))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package evil-magit
  :straight t)

(use-package magit
  :straight t
  :config
  (add-hook* 'magit-status-mode-hook (set-window-fringes (selected-window) 0 0 nil))
  (evil-collection-init 'magit)
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil)
  (require 'evil-magit))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package wrap-region
  :straight t
  :config
  (wrap-region-global-mode))

(use-package expand-region
  :straight t
  :bind* (("M-@" . er/expand-region)))

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

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(use-package helm
  :straight t
  :config
  (evil-collection-init 'helm)
  (define-key global-map (kbd "M-x") #'helm-M-x)
  (define-key helm-map (kbd "C-j") #'helm-next-line)
  (define-key helm-map (kbd "C-k") #'helm-previous-line)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key helm-map (kbd "<escape>") #'helm-keyboard-quit))

(use-package helm-ag
  :straight t
  :after evil-collection
  :config
  (define-key helm-ag-map (kbd "C-j")
    (lambda ()
      (interactive)
      (helm-next-line)
      (helm-execute-persistent-action)))
  (define-key helm-ag-map (kbd "C-k")
    (lambda
      (interactive)
      (helm-previous-line)
      (helm-execute-persistent-action))))

(use-package helm-swoop
  :straight t)

(use-package helm-ag
  :straight t)

(use-package projectile
  :straight t
  :config
  (projectile-mode))

(use-package winner
  :straight t
  :config
  (winner-mode 1))

(use-package solaire-mode
  :straight t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package helm-projectile
  :after projectile
  :straight t)

(use-package anzu
  :straight t
  :bind* (("M-%" . anzu-query-replace))
  :init
  (global-anzu-mode))

(use-package key-chord
  :straight t
  :config
  (add-hook* 'prog-mode-hook (key-chord-mode 1))
  (add-hook* 'isearch-mode-hook (key-chord-mode 1))
  (key-chord-define-global "xs" (lambda () (interactive)
                                  (evil-normal-state)
                                  (save-buffer))))

(use-package multiple-cursors
  :straight (mc :host github
                :repo "kwrooijen/mc"
                :files (:defaults (:exclude "*.el.in")))
  :bind* (("M-K" . mc/mark-previous-like-this)
          ("M-J" . mc/mark-next-like-this))
  :config
  (multiple-cursors-mode t))

(use-package ws-butler
  :straight t
  :config
  (ws-butler-global-mode)
  ;; Disable aftersave
  (defun ws-butler-after-save ()))

(use-package undo-tree
  :straight t
  :bind (("M-u" . undo-tree-redo)))

(use-package paredit
  :straight t)

(use-package lispyville
  :straight t
  :config
  (lispyville--define-key 'normal (kbd "M-J") #'mc/mark-next-like-this)
  (lispyville--define-key 'normal (kbd "M-K") #'mc/mark-previous-like-this))

(use-package general
  :straight t
  :config
  (general-override-mode))

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
    (evil-insert-state 1))

  (defun lispy-left-insert ()
    (interactive)
    (when (not (lispy--mode-p))
      (lispy-left 1))
    (when (not (lispy--mode-p))
      (beginning-of-defun))
    (evil-insert-state 1))

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

  (add-hook 'lispy-mode-hook #'show-paren-mode)
  (add-hook 'lispy-mode-hook #'paredit-mode)
  (add-hook 'lispy-mode-hook #'lispyville-mode))


(defconst mode-leader "'")

(general-create-definer mode-leader-def
  ;; :prefix my-leader
  ;; or without a variable
 :keymaps '(normal visual emacs motion)
  :prefix "'")

(add-to-list 'load-path (expand-file-name "lang" "~/.emacs.d"))
(require 'lang-clojure)

;;
;; Functions
;;

(defun capitalize-previous-word ()
  (interactive)
  (save-excursion
    (backward-word)
    (capitalize-word 1)))

(defun select-minibuffer ()
 "Make the active minibuffer the selected window."
 (interactive)
 (when (active-minibuffer-window)
   (select-window (active-minibuffer-window))))

;;
;; Configuration
;;

(set-frame-font "-*-Fira Mono-*-*-*-*-10-*-*-*-*-*-*-*" nil t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(global-hl-line-mode 1)

;; Smooth Scrolling
(setq scroll-margin 1
      scroll-conservatively 10000
      scroll-step 1
      auto-window-vscroll nil)

;; Don’t use tabs
(setq-default indent-tabs-mode nil)

;; Map mac's command key to meta (Alt / Option)
(setq mac-command-modifier 'meta)

;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Prompt for y/n innstead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll all the way to the bottom with C-v
(setq scroll-error-top-bottom t)

;; Default line wrapping should be 80
(setq fill-column 80)


;; Emacs temp directory
(setq temporary-file-directory "~/.emacs.d/tmp/")

(unless (file-exists-p "~/.emacs.d/tmp")
  (make-directory "~/.emacs.d/tmp"))

;; Disable error bell
(setq ring-bell-function 'ignore)

;;
;; Key bindings
;;

(bind-key* "M-+" 'align-regexp)
(bind-key* "M-C" 'capitalize-previous-word)

(general-define-key
 :states '(normal visual)
 :keymaps '(override)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "" nil

 "b" '(:ignore t :which-key "Buffers")
 "bb" 'helm-mini

 "f" '(:ignore t :which-key "Files")
 "ff" 'helm-find-files
 "fs" 'save-buffer

 "g" '(:ignore t :which-key "Git")
 "gg" 'magit-status

 "p" '(:ignore t :which-key "Project")
 "pp" 'helm-projectile-switch-project
 "pf" 'helm-projectile-find-file

 "s" '(:ignore t :which-key "Search")
 "ss" 'helm-swoop-without-pre-input
 "sp" 'helm-projectile-ag

 "w" '(:ignore t :which-key "Window")
 "ws" 'evil-window-split
 "wu" 'winner-undo
 "wu" 'winner-redo

 "r" '(:ignore t :which-key "Resume")
 "rr" 'helm-resume
 "ry" 'helm-show-kill-ring
 "rb" 'select-minibuffer)


;; ;; TODO check these
;; ;; superword-mode ???????????? Maybe
;; ;; yas-minor-mode

(setq gc-cons-threshold 800000)
