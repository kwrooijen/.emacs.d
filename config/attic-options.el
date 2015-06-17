(if window-system (progn
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)))))

;; Font for X
(setq default-frame-alist '(
    (font . "-unknown-Fira Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
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

;; Electric pair
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\{ . ?\})))


;; TODO create a toggle function for this
;; ;; Disable Mouse when Emacs has focus
;; (defun turn-off-mouse (&optional frame)
;;   (interactive)
;;   (shell-command "xinput --disable \"ETPS/2 Elantech Touchpad\""))

;; (defun turn-on-mouse (&optional frame)
;;   (interactive)
;;   (shell-command "xinput --enable \"ETPS/2 Elantech Touchpad\""))

;; (add-hook 'focus-in-hook #'turn-off-mouse)
;; (add-hook 'focus-out-hook #'turn-on-mouse)
;; (add-hook 'delete-frame-functions #'turn-on-mouse)

(provide 'attic-options)

