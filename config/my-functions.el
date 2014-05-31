(defun zsh (buffer-name)
    "Start a terminal and rename buffer."
    (interactive "sbuffer name: ")
    (eshell)
    (rename-buffer (format "%s%s" "$" buffer-name) t))

(defun zsht (buffer-name)
    "Start a terminal and rename buffer."
    (interactive "sbuffer name: ")
    (ansi-term "/bin/zsh")
    (rename-buffer (format "%s%s" "$" buffer-name) t))

(defun get-current-buffer-major-mode ()
    (interactive)
    (message "%s" major-mode))

(defun hoogle-search (query)
    "Search with hoogle commandline"
    (interactive "sHoogle query: ")
    (if (get-buffer "*Hoogle*") (kill-buffer "*Hoogle*"))
    ; get the version of hoogle so I don't have to manually adjust it for each update
    (shell-command (format "version=`hoogle --version | head -n 1 | awk '{print $2}' |
        cut -c 2- | rev | cut -c 2- | rev`;
        data=\"/databases\";
        two=$version$data;
        hoogle \"%s\" --data=$HOME/.lazyVault/sandboxes/hoogle/cabal/share/hoogle-$two" query))
    (switch-to-buffer "*Shell Command Output*")
    (rename-buffer "*Hoogle*")
    (haskell-mode)
    (linum-mode 0)
    (previous-buffer))

(defun ensure-buffer-name-begins-with-exl ()
    "change buffer name to end with slash"
    (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "!" name) t))))

(defun erlang-get-error ()
    (interactive)
    (shell-command (format "~/.emacs.d/scripts/erlangscript %s" buffer-file-name)))

(defun run-haskell-test ()
    (interactive)
    (async-shell-command "~/.emacs.d/scripts/cabal-test" "[Haskell Tests]"))

(defun run-make (args)
    (interactive)
    (async-shell-command (format "~/.emacs.d/scripts/make-script %s" args) "[Make Project]"))

(defun guard ()
    (interactive)
    (async-shell-command "~/.emacs.d/scripts/my-guard" "[Guard]"))

(defun underscores-to-camel-case (str)
    "Converts STR, which is a word using underscores, to camel case."
    (interactive "S")
    (apply 'concat (mapcar 'capitalize (split-string str "_"))))

; God functions

(defun god-mode-disable () (interactive)
    (god-local-mode-pause)
    (key-chord-mode 1)
    (if window-system
        (set-cursor-color "green")
        (if (getenv "TMUX")
            (send-string-to-terminal "\033Ptmux;\033\033]12;Green\007\033\\")
            (send-string-to-terminal "\033]12;Green\007"))))

(defun god-mode-enable () (interactive)
    (god-local-mode-resume)
    (if window-system
        (set-cursor-color "white")
        (if (getenv "TMUX")
            (send-string-to-terminal "\033Ptmux;\033\033]12;White\007\033\\")
            (send-string-to-terminal "\033]12;White\007"))))

(defun escape-key () (interactive)
    (god-mode-enable)
    (unless multiple-cursors-mode
        (progn
            (call-interactively (key-binding (kbd "C-g")))
            (keyboard-escape-quit))))

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
    (let (orig-one-window-p)
        (fset 'orig-one-window-p (symbol-function 'one-window-p))
        (fset 'one-window-p (lambda (&optional nomini all-frames) t))
        (unwind-protect
            ad-do-it
            (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

;; Vim's o key
(defun vim-o (&optional up)
    (interactive)
    (if up
        (progn
            (beginning-of-line)
            (open-line 1))
        (progn
        (end-of-line)
        (newline)))
    (if indent-of-doom-mode
        (indent-of-doom)
        (indent-for-tab-command))
    (god-mode-disable))

(defun copy-to-clipboard ()
    (interactive)
    (if (display-graphic-p)
        (progn
            (message "Yanked region to x-clipboard!")
            (call-interactively 'clipboard-kill-ring-save))
        (if (region-active-p)
            (progn
                (shell-command-on-region (region-beginning)
                    (region-end) "xsel -i -b")
                (message "Yanked region to clipboard!")
                (deactivate-mark))
        (message "No region active; can't yank to clipboard!"))))

(defun paste-from-clipboard ()
    (interactive)
    (if (display-graphic-p)
        (progn
            (clipboard-yank)
            (message "graphics active"))
        (insert (shell-command-to-string "xsel -o -b"))))

(defun command-repeater (list)
    (interactive)
    (setq char (string (read-event)))
    (setq repeater-command (cdr (assoc char list)))
    (if (or (not (boundp 'repeated-char)) (equal char repeated-char))
        (progn
            (if repeater-command
                (funcall repeater-command)
                (keyboard-quit))
            (setq repeated-char char)
            (command-repeater list))
        (progn
            (makunbound 'repeated-char)
            (call-interactively (key-binding (kbd char))))))

(defun god-g ()
    (interactive)
    (setq char (string (read-event)))
    (if (or (not (boundp 'repeated-char)) (equal char repeated-char))
        (progn
            (call-interactively (key-binding (kbd (format "M-%s" char))))
            (setq repeated-char char)
            (god-g))
        (progn
            (makunbound 'repeated-char)
            (call-interactively (key-binding (kbd char))))))

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

(defun default-directory-full ()
    (if (equal (substring default-directory 0 1) "~")
        (format "/home/%s%s" (user-login-name) (substring default-directory 1))
        default-directory))

(defun home-directory ()
    (interactive)
    (format "/home/%s/" (user-login-name)))

(defun shell-command-string (x)
    (interactive)
    (format "find %s | grep -v '#' | grep -v '/\\.' | grep -v '/Downloads' |
        grep -v '/Dropbox' | grep -v '/Music' | grep -v '/Videos' | grep -v '/Pictures' |
        grep -v 'ebin' | grep -v 'deps' | grep -v 'dist'" x))

(defun helm-swoop-find-files-recursively ()
    (interactive)
    (let ( (current (current-buffer))
           (current-dir default-directory))
        (switch-to-buffer "*helm-find-files-recursively*")
        (erase-buffer)
        (shell-command (shell-command-string (home-directory)) -1)
        (helm-swoop :$query "")
        (if (equal helm-exit-status 0)
            (setq final-location (buffer-substring
            (line-beginning-position) (line-end-position))))
        (switch-to-buffer current)
        (if (boundp 'final-location)
            (find-file final-location))
        (makunbound 'final-location)
        (makunbound 'current)))

(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
    "Minor mode to use big fringe in the current buffer."
    :init-value nil
    :global t
    :variable bzg-big-fringe-mode
    :group 'editing-basics
    (setq default-indicate-buffer-boundaries nil)
    (if (not bzg-big-fringe-mode)
        (set-fringe-style nil)
        (set-fringe-mode
        (/ (- (frame-pixel-width) (* 100 (frame-char-width))) 3)))
    (redraw-display))

(defun toggle-big-fringe ()
    (interactive)
    (if bzg-big-fringe-mode
        (progn
            (bzg-big-fringe-mode 0)
            (set-fringe-mode 0))
        (progn
            (bzg-big-fringe-mode 1)
            (setq default-indicate-buffer-boundaries nil)))
    (split-window)
    (delete-other-windows))

(defun execute-c () (interactive)
    (if (buffer-file-name)
        (progn
            (shell-command (format "gcc -g -o %s %s"
            (file-name-sans-extension (buffer-name))
            (buffer-file-name)))
            (async-shell-command
                (format " ./%s" (file-name-sans-extension (buffer-name)))))))

(defun iex-compile ()
    (interactive)
    (let ((current (buffer-name)))
        (elixir-mode-iex)
        (kill-line 0)
        (insert (format "c(\"%s\")" current))
        (comint-send-input)))

(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring"
    (interactive "p")
    (kill-ring-save (line-beginning-position)
        (line-beginning-position (+ 1 arg))))

(defun copy-line-fun (up)
    (let ((current (current-column)))
        (copy-line 1)
        (beginning-of-line)
        (yank)
        (if up (previous-line))
        (move-to-column current)))

(defun copy-line-up ()
    (interactive)
    (copy-line-fun t))

(defun copy-line-down ()
    (interactive)
    (copy-line-fun nil))

(provide 'my-functions)
