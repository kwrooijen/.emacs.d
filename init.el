(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
;; List of packages to install
(defvar my-packages
  '(
    powerline
    god-mode
    helm
    helm-ls-git
    helm-swoop
    multiple-cursors
    auto-complete
    iy-go-to-char
    window-numbering
    magit
    workgroups
    wrap-region
    yasnippet
    rainbow-delimiters
    erlang
    haskell-mode
    expand-region
    js2-mode
    web-mode
    redo+
    ace-jump-mode
    key-chord
    ))

(defun my-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        my-packages))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-to-list 'load-path "~/.emacs.d/config")

(require 'helm)
(require 'helm-ls-git)
(require 'helm-swoop)
(require 'god-mode)
(require 'multiple-cursors)
(require 'workgroups)
(require 'redo+)
(require 'auto-complete-config)

;; Modes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(multiple-cursors-mode 1)
(workgroups-mode 1)
(show-paren-mode t)
(window-numbering-mode 1)
(god-mode)
(global-rainbow-delimiters-mode)
(yas-global-mode 1)
(global-auto-complete-mode t)

;;My configurations
(require 'my-requires)
;;Make mc work better with iy-go-to-char
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

;;Always display 2 columns in linum mode (no stuttering)
'(linum-format (quote "%2d"))

;;Allow upcase-region and downcase-region functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;;No confirmation when creating new buffer
(setq confirm-nonexistent-file-or-buffer nil)

;;Backup ~ files in seperate directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;No splash screen
(setq inhibit-startup-message t)

 ;;Don't resize minibuffer
(setq resize-mini-windows nil)

;;Don't ask when creating new buffer
(setq confirm-nonexistent-file-or-buffer nil)

;;No animation when loading workgroups
(setq wg-morph-on nil)

;;y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll all the way to the bottom with C-v
(setq scroll-error-top-bottom t)

;; Helm configurations
(setq helm-ff-newfile-prompt-p nil)
(setq helm-grep-default-recurse-command "grep --exclude-dir=\"dist\" -a -d recurse %e -n%cH -e %p %f")
(setq helm-reuse-last-window-split-state t)
(setq helm-split-window-in-side-p t)
(setq helm-swoop-split-with-multiple-windows t)

;; Smooth Scrolling
(setq redisplay-dont-pause t
  scroll-conservatively 10000)

;;; Autocomplete / Yasnippet settings

;; Add auto complete to these modes
(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'js2-mode)
(add-to-list 'ac-modes 'web-mode)

;; Autocomplete default config
(ac-config-default)
;; Use auto complete menu
(setq ac-use-menu-map t)
;; Show menu instantly
(setq ac-auto-show-menu 0.0)
;; Add yasnippets to menu
(defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
    (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
      (add-to-list 'ac-sources 'ac-source-yasnippet))

;;Load mode on certain file extensions
(setq auto-mode-alist (append '(
    ("\\.tpl\\'"   . web-mode)
    ("\\.dtl\\'"   . web-mode)
    ("\\.app.src\\'"   . erlang-mode)
    ("\\.erb\\'"   . web-mode)
    ("\\.css\\'"   . css-mode)
    ("\\.scss\\'"   . sass-mode)
    ("\\.scss\\'"   . sass-mode)
    ("\\.less\\'"   . sass-mode)
    ("\\.rb$"      . enh-ruby-mode)
    ("\\.rake$"    . enh-ruby-mode)
    ("Rakefile$"   . enh-ruby-mode)
    ("\\.gemspec$" . enh-ruby-mode)
    ("\\.ru$"      . enh-ruby-mode)
    ("Gemfile$"    . enh-ruby-mode)
    ("\\.js\\'"    . js2-mode)
    ("\\.elm\\'"   . haskell-mode)
    (".splash"     . (lambda()
        (lisp-interaction-mode)
        (read-only-mode)
        (set (make-local-variable 'linum-mode) nil)
        ))
    ) auto-mode-alist))

;; Hooks

(add-hook 'erlang-mode-hook 'erlang-keys-hook)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'dired-mode-hook 'ensure-buffer-name-begins-with-exl)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(global-hl-line-mode t)
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(wg-mode-line-on nil)
 '(wrap-region-global-mode t nil (wrap-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erm-syn-errline ((t (:foreground "red" :box (:line-width 1 :color "red") :underline "red"))))
 '(erm-syn-warnline ((t (:foreground "yellow" :box (:line-width 1 :color "yellow") :underline "yellow"))))
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(helm-ff-directory ((t (:background "color-233" :foreground "cyan"))))
 '(helm-ff-file ((t (:inherit default))))
 '(hl-line ((t (:inherit highlight :background "color-234"))))
 '(region ((t (:background "color-240" :foreground "#FFF"))))
 '(show-paren-match ((t (:background "color-239" :foreground "#7CB8BB" :weight bold))))
 '(web-mode-block-attr-name-face ((t (:foreground "color-244"))))
 '(web-mode-html-attr-custom-face ((t (:foreground "color-249"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "color-249"))))
 '(web-mode-html-attr-name-face ((t (:foreground "color-249"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-244"))))
 '(web-mode-html-tag-face ((t (:foreground "color-244"))))
 '(web-mode-symbol-face ((t (:foreground "color-69")))))
