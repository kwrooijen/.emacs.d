(if (getenv "DISPLAY")
    (global-hl-line-mode t))

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

;; Personal Info
(setq user-full-name "Kevin W. van Rooijen")

;; Emacs temp directory
(setq temporary-file-directory "~/.emacs.d/tmp/")

;; Delete seleted text when typing
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

;; Set xml tabs to 4
(setq nxml-child-indent 4)

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

;; Don't use deep indent in Ruby
(setq ruby-deep-indent-paren nil)

;; Highlight delay for multiple occurences
(setq highlight-symbol-idle-delay 0)

;; Enable interactive behavior for Tempo
(setq tempo-interactive t)

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
;; Helm-dash should use W3m for showing documentation
(setq helm-dash-browser-func 'w3m-goto-url)
;;; Helm configurations END ;;;

;;; Tramp Mode
;; Set Tramp backup file location
(setq tramp-backup-directory-alist backup-directory-alist)

;; Don't use version control for all files
(setq vc-ignore-dir-regexp
    (format "\\(%s\\)\\|\\(%s\\)"
        vc-ignore-dir-regexp
        tramp-file-name-regexp))

(setq remote-file-name-inhibit-cache nil)
(setq tramp-completion-reread-directory-timeout nil)

;; Smooth Scrolling
(setq redisplay-dont-pause t
    scroll-margin 1
    scroll-conservatively 10000)

;; Keyboard scroll one line at a time
(setq scroll-step 1)

(setq auto-window-vscroll nil)

;; Add final new line for Dennis
(setq require-final-newline t)

;;; Autocomplete / Yasnippet settings START
;; Add auto complete to these modes
(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'elixir-mode)
(add-to-list 'ac-modes 'haskell-mode)
(add-to-list 'ac-modes 'js2-mode)
(add-to-list 'ac-modes 'web-mode)

;; Autocomplete default config
(ac-config-default)
;; Use auto complete menu
(setq ac-use-menu-map t)
;; Show menu instantly
(setq ac-auto-show-menu 0.0)
;; Show help menu in 0.5 sec
(setq ac-quick-help-delay 0.5)
;; Add yasnippets to menu
(defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
    (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
      (add-to-list 'ac-sources 'ac-source-yasnippet))
;;; Autocomplete / Yasnippet settings END

;; Web mode
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)

;; Modes with no Linum
(setq linum-disabled-modes-list '(
    mu4e-compose-mode
    mu4e-headers-mode
    mu4e-main-mode
))

;; Buffers to be ignored by Winner
(setq winner-boring-buffers '(
    "*Completions*"
    "*Compile-Log*"
    "*inferior-lisp*"
    "*Fuzzy Completions*"
    "*Apropos*"
    "*dvc-error*"
    "*Help*"
    "*cvs*"
    "*Buffer List*"
    "*Ibuffer*"
))

;; Git gutter
(setq git-gutter+-unchanged-sign " ")

;; Create 6 initial screens and move to number 1
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-goto-screen-1)

;; Prompt for eshell, a bit buggy but it will do for now...
(setq eshell-prompt-function
  (lambda nil
      (if (equal (car (s-split "/scpc:" (eshell/pwd))) (eshell/pwd))
        (setq tramp-prompt "")
        (setq tramp-prompt "/scpc:"))
      (let ((split (s-split "/" (eshell/pwd))))
          (let ((bot (car (last split)))
                (top (car (last (butlast split)))))
        (concat tramp-prompt top "/" bot " $ ")
      ))))

(provide 'my-options)
