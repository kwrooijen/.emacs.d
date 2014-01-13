;; List of packages to install
(defvar my-packages
  '(
    column-enforce-mode
    powerline
    rinari
    helm-rails
    web-mode
    redo+
    ace-jump-mode
    auto-complete
    coffee-mode
    elixir-mode
    git-gutter
    expand-region
    god-mode
    haskell-mode
    helm
    helm-ls-git
    helm-swoop
    js2-mode
    linum-relative
    magit
    multiple-cursors
    rainbow-delimiters
    slime
    sr-speedbar
    undo-tree
    workgroups
    wrap-region
    yasnippet
    zencoding-mode
    git-commit-mode
    git-rebase-mode
    gitconfig-mode
    gitignore-mode
    emms
    iy-go-to-char
    erlang
    flymake-haskell-multi
    window-numbering
    ))

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defun my-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        my-packages))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-to-list 'load-path "~/.emacs.d/config")
;(let ((default-directory "~/.emacs.d/plugins/"))
;  (normal-top-level-add-subdirs-to-load-path))

(require 'column-enforce-mode)
(require 'powerline)
(require 'tramp)
(require 'undo-tree)
(require 'redo+)
(require 'linum-relative)
(require 'git-gutter)
(require 'god-mode)
(require 'zencoding-mode)
(require 'helm)
(require 'helm-ls-git)
(require 'helm-rails)
(require 'helm-swoop)
(require 'auto-complete)
(require 'misc)
(require 'haskell-mode-autoloads)
(require 'yasnippet)
(require 'sr-speedbar)
(require 'flymake)
(require 'magit)
(require 'expand-region)
(require 'ace-jump-mode)
(require 'multiple-cursors)
(require 'rainbow-delimiters)
(require 'wrap-region)
(require 'workgroups)
(require 'epa-file)
(require 'erlang-start)
(require 'elixir-mode)
(require 'coffee-mode)
(require 'slime)
(require 'js2-mode)
(require 'rinari)
(require 'emms-setup)
(require 'web-mode)
(require 'iy-go-to-char)

; My configurations
(require 'my-extras)
(require 'my-functions)
(require 'my-modes)
(require 'my-colors)
(require 'my-keys)
(require 'my-hooks)

 ;;Don't move speedbar
(sr-speedbar-window-dedicated-only-one-p)
;;Open speedbar at startup
(sr-speedbar-open)
;;Close it again or else it will duplicate when loading workgroup
(sr-speedbar-close)
;;Load my custom workgroup
(wg-load "~/.emacs.d/workgroups/Attic")

;;Make mc work better with iy-go-to-char
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

;;Always display 2 columns in linum mode (no stuttering)
'(linum-format (quote "%2d"))
'(linum-relative-format (quote "%2d"))

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

;;Use relative
(setq linum-format 'linum-relative)

;;Don't ask when creating new buffer
(setq confirm-nonexistent-file-or-buffer nil)

;;No animation when loading workgroups
(setq wg-morph-on nil)

;;y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

;;ido for rinari until I find a helm alternative
(setq
  ido-ignore-buffers
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
    "^\*compilation" "^\*GTAGS" "^session\.*" ".newsrc-dribble"
    "^\*scr" "^\*" "notes.org" "todos.org" "*.hi" "*.o")
  ido-enable-flex-matching t)

;;Load mode on certain functions
(setq auto-mode-alist (append '(
    ("\\.tpl\\'" . html-mode)
    ("\\.erb\\'" . web-mode)
    ("\\.js\\'"  . js2-mode)
    ("\\.elm\\'" . haskell-mode)
    ("Gemfile" . ruby-mode)
    ) auto-mode-alist))


;And all the emacs auto adjustments
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#111111" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(auth-source-save-behavior nil)
 '(coffee-tab-width 4)
 '(custom-safe-themes (quote ("0a1eb1fb7c716c0eced08572fa5477e6cf80d145167590ab5a00697678b14c30" "dc3d6a554b626775e02c17da54e7b7f9378ccfd3cbadab62397f8a6ddf33490f" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "eecdec02a43c2dfdf388d7d27cb00af310b350d4ea6f923ebc82197d348cf383" default)))
 '(dirtree-windata (quote (frame left 0.15 delete)))
 '(fci-rule-color "#383838")
 '(global-hl-line-mode t)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-where-post-offset 4)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-font-lock turn-on-haskell-doc-mode auto-complete-mode wrap-region-mode imenu-add-menubar-index flymake-haskell-multi-load)))
 '(helm-grep-default-recurse-command "grep --exclude-dir=\"dist\" -a -d recurse %e -n%cH -e %p %f")
 '(helm-reuse-last-window-split-state t)
 '(helm-split-window-in-side-p t)
 '(helm-swoop-split-with-multiple-windows t)
 '(js2-strict-missing-semi-warning nil)
 '(scroll-error-top-bottom t)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(speedbar-default-position (quote left))
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks t)
 '(wg-mode-line-on nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(helm-ff-directory ((t (:background "color-233" :foreground "cyan"))))
 '(helm-ff-file ((t (:inherit default))))
 '(hl-line ((t (:inherit highlight :background "color-234"))))
 '(linum-relative-current-face ((t (:inherit linum :background "color-234" :foreground "#707070" :weight bold))))
 '(region ((t (:background "color-240" :foreground "#FFF"))))
 '(show-paren-match ((t (:background "color-239" :foreground "#7CB8BB" :weight bold)))))
