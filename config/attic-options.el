(if window-system (progn
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)))))

;; Font for X
(setq default-frame-alist '(
    (font . "Fira Mono OT-11:weight=bold")
    (vertical-scroll-bars . nil)
    (line-spacing . 0)))

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

;; Don't copy to clipboard using X please.
(setq x-select-enable-clipboard nil)

;; Set tab stop list
(setq tab-stop-list-4 '(
    4   8   12  16  20
    24  28  32  36  40
    44  48  52  56  60
    64  68  72  76  80
    84  88  92  96  100
    104 108 112 116 120
))

(setq tab-stop-list-2 '(
    0  2  4  6  8
    10 12 14 16 18
    20 22 24 26 28
    30 32 34 36 38
    40 42 44 46 48
    50 52 54 56 58
    60 62 64 66 68
    70 72 74 76 78
    80 82 84 86 88
    90 92 94 96 98
))

(setq tab-stop-list tab-stop-list-4)

;; Personal Info
(setq user-full-name "Kevin W. van Rooijen")
(setq user-mail-address "kevin.van.rooijen@attichacker.com")
(setq mail-signature "
--------------------
Kevin W. van Rooijen
@attichacker
")

;; Emacs temp directory
(setq temporary-file-directory "~/.emacs.d/tmp/")

;; Delete seleted text when typing
(delete-selection-mode 1)

;; Make mc work better with iy-go-to-char
(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

;; Always display 2 columns in linum mode (no stuttering)
(setq linum-format (quote "%3d"))
(setq nlinum-format (quote "%3d"))

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

;; Highlight delay for multiple occurences
(setq highlight-symbol-idle-delay 0)

;; Enable interactive behavior for Tempo
(setq tempo-interactive t)

;; C-u C-SPC will repeat if C-SPC is pressed again
(setq set-mark-command-repeat-pop t)

;; Make sure the file ends with a newline
(setq require-final-newline t)

;; Default alignment should be 80
(setq fill-column 80)

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

;; Org Mode

;; Log time
(setq org-log-done 'time)
(setq org-capture-templates '())
;; Add Capture Templates
(setq org-capture-templates
      '(("d" "Done" entry
         (file+headline "~/Documents/notes/Org/Done.org" "Done")
         (file "~/.emacs.d/Templates/Done.orgtpl"))
        ("r" "Retro" entry
         (file+headline "~/Documents/notes/Org/Retro.org" "Done")
         (file "~/.emacs.d/Templates/Retro.orgtpl"))
        ("t" "Todo" entry
         (file+headline "~/Documents/notes/Org/Todo.org" "Todo")
         (file "~/.emacs.d/Templates/Todo.orgtpl"))))

;; Projectile
; Enable Caching
(setq projectile-enable-caching t)
(setq projectile-projects-cache (make-hash-table))

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

;; Sauron
(setq sauron-hide-mode-line t)
(setq sauron-separate-frame nil)
(setq sauron-max-line-length (- (window-total-width) 10))

;; Made up variable
(setq sauron-max-line-height 4)

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
    "*Ibuffer*"))

;; Git gutter
(setq git-gutter+-unchanged-sign " ")

;; Macro is not active at boot, setting variable
(setq macro-active nil)

;; Create 6 initial screens and move to number 1
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-create-screen)
(escreen-goto-screen-1)

;; Gnus
;; In Group buffer press "G p" and add this to the list (modeline-notify t)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
(setq gnus-demon-timestep 1)
(gnus-demon-add-handler 'gnus-group-get-new-news 30 3)
(setq gnus-always-read-dribble-file t)
(fmakunbound 'gnus-group-delete-articles) ;; Because no.

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-separator " | ")

;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; God modes
(add-to-list 'god-exempt-major-modes 'gnus-summary-mode)
(add-to-list 'god-exempt-major-modes 'gnus-group-mode)
(add-to-list 'god-exempt-major-modes 'term-mode)
(add-to-list 'god-exempt-major-modes 'help-mode)
(add-to-list 'god-exempt-major-modes 'grep-mode)
(add-to-list 'god-exempt-major-modes 'doc-view-mode)
(add-to-list 'god-exempt-major-modes 'top-mode)
(add-to-list 'god-exempt-major-modes 'dired-mode)

;; Electric pair
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\{ . ?\})))

(provide 'attic-options)

