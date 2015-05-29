;;==============================================================================
;;== Keys
;;==============================================================================

(key-chord-define helm-map ";j" 'helm-keyboard-quit)

(define-key helm-map (kbd "C-b") 'nil)
(define-key helm-map (kbd "C-f") 'nil)
(define-key helm-map (kbd "M-b") 'nil)
(define-key helm-map (kbd "M-f") 'forward-word)
(define-key helm-map (kbd "M-s") 'helm-select-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-buffer-map (kbd "C-a") 'helm-buffers-toggle-show-hidden-buffers)
(define-key helm-swoop-map (kbd "M-e") 'helm-swoop-edit)

(define-key helm-map (kbd "C-n")
    (lambda() (interactive) (if (or (boundp 'helm-swoop-active)
                                    (boundp 'helm-register-active))
        (progn (helm-next-line) (helm-execute-persistent-action))
        (helm-next-line))))

(define-key helm-map (kbd "C-p")
    (lambda() (interactive) (if (or (boundp 'helm-swoop-active)
                                    (boundp 'helm-register-active))
        (progn (helm-previous-line) (helm-execute-persistent-action))
        (helm-previous-line))))

(define-key helm-map                (kbd "M-?") 'helm-help)
(define-key helm-map                (kbd "M-g") 'helm-keyboard-quit)
(define-key helm-find-files-map     (kbd "M-g") 'helm-keyboard-quit)
(define-key helm-generic-files-map  (kbd "M-g") 'helm-keyboard-quit)
(define-key helm-buffer-map         (kbd "M-g") 'helm-keyboard-quit)

;;==============================================================================
;;== Options
;;==============================================================================

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
(setq helm-ls-git-ls-show-abs-or-relative 'relative)

;; Don't show colors in Tramp mode
(setq helm-ff-tramp-not-fancy t)

;; Smarter completion for Helm
(setq helm-ff-smart-completion t)

;; Helm-dash should use W3m for showing documentation
(setq helm-dash-browser-func 'eww)

;; Don't add delay when choosing
(setq helm-exit-idle-delay 0)

;; Don't display header
(setq helm-display-header-line nil)

;; Try to hide source header as much as possible
(set-face-attribute 'helm-source-header nil :height 0.1 :background "#000"  :foreground "#000")
;; Set a min / max height of 30% of current buffer
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)


(setq ;; truncate long lines in helm completion
      helm-truncate-lines t
      ;; may be overridden if 'ggrep' is in path (see below)
      helm-grep-default-command
      "grep -a -d skip %e -n%cH -e %p %f"
      helm-grep-default-recurse-command
      "grep -a -d recurse %e -n%cH -e %p %f"
      ;; do not display invisible candidates
      helm-quick-update t
      ;; be idle for this many seconds, before updating in delayed sources.
      helm-idle-delay 0.01
      ;; be idle for this many seconds, before updating candidate buffer
      helm-input-idle-delay 0.01
      ;; open helm buffer in another window
      helm-split-window-default-side 'other
      ;; limit the number of displayed canidates
      helm-candidate-number-limit 200
      ;; don't use recentf stuff in helm-ff
      helm-ff-file-name-history-use-recentf nil
      ;; move to end or beginning of source when reaching top or bottom
      ;; of source
      helm-move-to-line-cycle-in-source t
      ;; fuzzy matching
      helm-buffers-fuzzy-matching t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      ;; Here are the things helm-mini shows, I add `helm-source-bookmarks'
      ;; here to the regular default list
      ;; helm-mini-default-sources '(helm-source-buffers-list
      ;;                             helm-source-recentf
      ;;                             helm-source-bookmarks)
      )

(setq helm-ls-git-project-source
      '((name . "Helm git ls project")
        (candidates . helm-ls-git-project-list)
        (action . helm-ls-git-project-action)
        (default . "")))

(setq helm-ls-git-project-list-file "~/.emacs.d/.helm-ls-git-project-list")
;;==============================================================================
;;== Hook
;;==============================================================================

(setq helm-ls-git-project-list nil)
(if (file-exists-p helm-ls-git-project-list-file)
    (load-file helm-ls-git-project-list-file))

;;==============================================================================
;;== Advice
;;==============================================================================

(defadvice helm-register (before helm-register activate)
  (setq helm-register-active t))

(defadvice helm-register (after helm-register activate)
  (makunbound 'helm-register-active))

(defadvice helm-swoop (before helm-swoop activate)
  (set-mark-command nil)
  (deactivate-mark)
  (setq helm-swoop-active t))

(defadvice helm-swoop (after helm-swoop activate)
  (makunbound 'helm-swoop-active))

(defadvice helm-ls-git-ls (before helm-ls-git-ls activate)
  (let ((dir (magit-get-top-dir)))
    (if dir
        (unless (member dir helm-ls-git-project-list)
          (progn
            (add-to-list 'helm-ls-git-project-list dir)
            (save-project-list-to-file))
          (helm-ls-git-project)))))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun helm-swoop-emms ()
    (interactive)
    (let ((current (current-buffer)))
    (split-window)
    (other-window 1)
    (emms-playlist-mode-go)
    (funcall (lambda ()
        (helm-swoop :$query "")
        (if (equal helm-exit-status 0) (emms-playlist-mode-play-smart))
        (delete-window)
        (switch-to-buffer current)
        (makunbound 'current)))))

(defun helm-swoop-find-files-recursively ()
    (interactive)
    (let ( (current (current-buffer))
           (current-dir default-directory))
        (switch-to-buffer "*helm-find-files-recursively*")
        (erase-buffer)
        (shell-command (find-files-recursively-shell-command (home-directory)) -1)
        (helm-swoop :$query "")
        (if (equal helm-exit-status 0)
            (setq final-location (buffer-substring
            (line-beginning-position) (line-end-position))))
        (switch-to-buffer current)
        (if (boundp 'final-location)
            (find-file final-location))
        (makunbound 'final-location)
        (makunbound 'current)))

(defun helm-ls-git-project ()
  (interactive)
  (helm :sources '(helm-ls-git-project-source)))

(defun helm-ls-git-project-action (candidate)
  (find-file candidate))

(defun save-project-list-to-file ()
  (with-current-buffer (find-file-noselect helm-ls-git-project-list-file)
    (erase-buffer)
    (insert (format "(setq helm-ls-git-project-list '%S)" helm-ls-git-project-list))
    (save-buffer)))

(provide 'attic-helm)
