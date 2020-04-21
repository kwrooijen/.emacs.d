;;; custom/custom-options.el -*- lexical-binding: t; -*-


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

;; Backup ~ files in seperate directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; No confirmation when creating new buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; Disable error bell
(setq ring-bell-function 'ignore)

(provide 'custom-options)
