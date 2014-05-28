(add-to-list 'load-path "~/.emacs.d/config")
(require 'my-packages)
(require 'auto-complete-config)
(require 'god-mode)
(require 'helm)
(require 'helm-ls-git)
(require 'helm-swoop)
(require 'multiple-cursors)
(require 'redo+)

;; Modes
(global-auto-complete-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
(global-rainbow-delimiters-mode)
(god-mode)
(key-chord-mode 1)
(multiple-cursors-mode 1)
(show-paren-mode t)
(window-numbering-mode 1)
(wrap-region-global-mode t)
(yas-global-mode 1)

;; My configurations
(require 'my-functions)
(require 'my-powerline)
(require 'my-keys)
(require 'my-extras)
(require 'tab-of-doom)

;; Enable Zenburn Theme
(load-theme 'zenburn t)

; Enable my powerline Theme
(my-powerline-theme)

;; Font for X
(setq default-frame-alist '(
    (font . "Fira Mono-12:weight=bold")
    (vertical-scroll-bars . nil)
    (line-spacing . 0)
))

;; Disable Tool bar
(tool-bar-mode 0)
;; Disable Menu bar
(menu-bar-mode 0)
;; Disable Scroll bar
(scroll-bar-mode 0)
;; Disable Blinking cursor
(blink-cursor-mode 0)

;; Set default tab width to 4
(setq tab-width 4)

;; Set tab stop list
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68
                        72 76 80 84 88 92 96 100 104 108 112 116 120))

; Delete seleted text when typing
(delete-selection-mode 1)

;; Make mc work better with iy-go-to-char
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

;; Always display 2 columns in linum mode (no stuttering)
(setq linum-format (quote "%3d "))

;; Disable Fringe
(set-fringe-mode 0)

;; Allow upcase-region and downcase-region functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; No confirmation when creating new buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; Backup ~ files in seperate directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Don't resize minibuffer
(setq resize-mini-windows nil)

;; Don't ask when creating new buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; No animation when loading workgroups
(setq wg-morph-on nil)

;; y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

;; follow symlinks and don't ask
(setq vc-follow-symlinks t)

;; Scroll all the way to the bottom with C-v
(setq scroll-error-top-bottom t)

;; Hide startup message
(setq inhibit-startup-message t)

;; Show nothing in *scratch* when started
(setq initial-scratch-message nil)

;; Reread a TAGS table without querying, if it has changed.
(setq tags-revert-without-query t)

;; Let C-v M-v brings back where you were.
(setq scroll-preserve-screen-position t)

;;; Helm configurations START
;; Don't ask to create new file
(setq helm-ff-newfile-prompt-p nil)
(setq helm-grep-default-recurse-command
      "grep --exclude-dir=\"dist\" -a -d recurse %e -n%cH -e %p %f")
(setq helm-reuse-last-window-split-state t)
(setq helm-ff-transformer-show-only-basename nil)
;; Split window down
(setq helm-split-window-in-side-p t)
;; Split when multiple windows open
(setq helm-swoop-split-with-multiple-windows t)
;; Show relative path
(setq helm-ls-git-show-abs-or-relative 'relative)
;; Show colors in Tramp mode
(setq helm-ff-tramp-not-fancy nil)
;; Smarter completion for Helm
(setq helm-ff-smart-completion t)
;;; Helm configurations END ;;;

;; Smooth Scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-conservatively 10000)

;;; Autocomplete / Yasnippet settings START
;; Add auto complete to these modes
(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'haskell-mode)
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
;;; Autocomplete / Yasnippet settings END

;; Hooks
(defun clean-hook ()
  (interactive)
  (god-local-mode 0)
  (insert-mode 0)
  (key-chord-mode 0)
  (linum-mode 0))

(defun key-chord-force ()
  (interactive)
  (key-chord-mode 1)
  (message nil))

(defadvice ansi-term (after advice-term-line-mode activate)
  (clean-hook))

(defun fix-tabs (x)
  (tab-of-doom-mode t)
  (setq tab-width x)
  (god-local-mode t))

(add-hook 'before-save-hook      'delete-trailing-whitespace)
(add-hook 'c-initialization-hook 'c-keys-hook)
(add-hook 'dired-mode-hook       'ensure-buffer-name-begins-with-exl)
(add-hook 'elixir-mode-hook      'elixir-keys-hook)
(add-hook 'erlang-mode-hook      'erlang-keys-hook)
(add-hook 'insert-mode-hook      'key-chord-force)
(add-hook 'magit-mode-hook       'clean-hook)
(add-hook 'erlang-mode-hook     (lambda () (fix-tabs 4)))
(add-hook 'emacs-lisp-mode-hook (lambda () (fix-tabs 4)))
(add-hook 'elixir-mode-hook     (lambda () (fix-tabs 2)))
(add-hook 'ruby-mode-hook       (lambda () (fix-tabs 2)))
(add-hook 'haskell-mode-hook    (lambda () (fix-tabs 4)))

;; Doom Indent Config
(setq doom-use-standard t)

(setq my-doom '(
    (all . (
        ((and (prev 'ends-on "[") (current 'starts-with "]")) (prev 'indent))
        ((current 'starts-with "]") (prev 'indent -1))
        ((prev 'ends-on "[" "{")    (prev 'indent 1))
        ((prev 'ends-on ",")        (prev 'indent))
        ((prev 'indent-char-is ",") (prev 'indent))
    ))
))

;; Load mode on certain file extensions
(setq auto-mode-alist (append '(
    ("\\.less\\'"    . css-mode)
    ("\\.scss\\'"    . css-mode)
    ("Gemfile$"      . enh-ruby-mode)
    ("Rakefile$"     . enh-ruby-mode)
    ("\\.gemspec$"   . enh-ruby-mode)
    ("\\.rake$"      . enh-ruby-mode)
    ("\\.rb$"        . enh-ruby-mode)
    ("\\.ru$"        . enh-ruby-mode)
    ("\\.app.src\\'" . erlang-mode)
    ("\\.elm\\'"     . haskell-mode)
    ("\\.js\\'"      . js2-mode)
    ("\\.dtl\\'"     . web-mode)
    ("\\.erb\\'"     . web-mode)
    ("\\.tpl\\'"     . web-mode)
    ) auto-mode-alist))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#1c1c1c" :foreground "#e5e3e3"))))
 '(erm-syn-errline ((t (:foreground "red" :box (:line-width 1 :color "red") :underline "red"))))
 '(erm-syn-warnline ((t (:foreground "yellow" :box (:line-width 1 :color "yellow") :underline "yellow"))))
 '(flymake-errline ((t (:foreground "red" :underline "red"))) t)
 '(flymake-warnline ((((class color)) (:foreground "yellow" :underline "yellow"))) t)
 '(flyspell-incorrect ((t (:underline (:color "red" :style wave)))) t)
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "#707070"))))
 '(font-lock-comment-face ((t (:foreground "#707070"))))
 '(font-lock-constant-face ((t (:foreground "#dc1e6a"))))
 '(font-lock-doc-face ((t (:foreground "#bc853d"))))
 '(font-lock-type-face ((t (:foreground "#145e74"))))
 '(fringe ((t (:background "#383838" :foreground "#DCDCCC"))))
 '(helm-ff-directory ((t (:background "#121212" :foreground "cyan"))))
 '(helm-ff-executable ((t (:background "#1c1c1c" :foreground "#9FC59F" :weight bold))))
 '(helm-ff-file ((t (:background "#1c1c1c" :foreground "#DCDCCC" :weight bold))))
 '(helm-swoop-target-line-block-face ((t (:background "#585858" :foreground "#FFF"))))
 '(helm-swoop-target-line-face ((t (:background "#585858" :foreground "#FFF"))))
 '(helm-swoop-target-word-face ((t (:background "#7700ff" :foreground "#fff"))))
 '(hl-line ((t (:inherit highlight :background "#303030"))))
 '(linum ((t (:inherit (shadow default) :background "#383838" :foreground "#8FB28F"))))
 '(magit-branch ((t (:background "#111111"))) t)
 '(region ((t (:background "#585858" :foreground "#FFF"))))
 '(show-paren-match ((t (:background "#4e4e4e" :foreground "#7CB8BB" :weight bold))))
 '(vertical-border ((t (:background "#383838" :foreground "#383838"))))
 '(web-mode-block-attr-name-face ((t (:foreground "#808080"))))
 '(web-mode-html-attr-custom-face ((t (:foreground "#b2b2b2"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "#b2b2b2"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#b2b2b2"))) t)
 '(web-mode-html-tag-bracket-face ((t (:foreground "#808080"))))
 '(web-mode-html-tag-face ((t (:foreground "#808080"))) t)
 '(web-mode-symbol-face ((t (:foreground "#2a55d4"))) t))
