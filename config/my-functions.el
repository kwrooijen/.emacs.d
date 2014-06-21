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
    (shell-command (format "~/.emacs.d/scripts/erlang/erlang-flymake %s" buffer-file-name)))

(defun run-haskell-test ()
    (interactive)
    (async-shell-command "~/.emacs.d/scripts/haskell/cabal-test" "[Haskell Tests]"))

(defun run-make (args)
    (interactive)
    (async-shell-command (format "~/.emacs.d/scripts/other/make-script %s" args) "[Make Project]"))

(defun run-mix (args)
    (async-shell-command (format "~/.emacs.d/scripts/elixir/mix-script %s" args) "[Mix Execution]"))

(defun guard ()
    (interactive)
    (async-shell-command "~/.emacs.d/scripts/other/my-guard" "[Guard]"))

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
        (if (getenv "DISPLAY")
            (if (getenv "TMUX")
            (send-string-to-terminal "\033Ptmux;\033\033]12;Green\007\033\\")
            (send-string-to-terminal "\033]12;Green\007")))))

(defun god-mode-enable () (interactive)
    (god-local-mode-resume)
    (if window-system
        (set-cursor-color "white")
        (if (getenv "DISPLAY")
            (if (getenv "TMUX")
                (send-string-to-terminal "\033Ptmux;\033\033]12;White\007\033\\")
                (send-string-to-terminal "\033]12;White\007")))))

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

(defun find-files-recursively-shell-command (x)
    (interactive)
    (format "find %s | grep -v '#' | grep -v '/\\.' | grep -v '/Downloads' |
        grep -v '/Dropbox' | grep -v '/Music' | grep -v '/Videos' | grep -v '/Pictures' |
        grep -v '/Mail' | grep -v 'ebin' | grep -v 'deps' | grep -v 'dist'" x))

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

(defun execute-rust () (interactive)
    (if (buffer-file-name)
        (let ((result (shell-command-to-string (format "rustc %s ; echo $?" (buffer-file-name))))
              (origin (buffer-name)))
          (if (equal (get-return-code result) "0")
              (async-shell-command (format " ./%s" (file-name-sans-extension (buffer-name))) "[Rust Compile]")
              (progn
                  (async-shell-command "" "[Rust Compile]")
                  (switch-to-buffer "[Rust Compile]")
                  (insert result)
                  (switch-to-buffer origin))))))

(defun test-rust () (interactive)
    (if (buffer-file-name)
        (let ((result (shell-command-to-string (format "rustc --test %s ; echo $?" (buffer-file-name))))
              (origin (buffer-name)))
          (if (equal (get-return-code result) "0")
              (async-shell-command (format " ./%s" (file-name-sans-extension (buffer-name))) "[Rust Compile]")
              (progn
                  (async-shell-command "" "[Rust Compile]")
                  (switch-to-buffer "[Rust Compile]")
                  (insert result)
                  (switch-to-buffer origin))))))

(defun get-return-code (s)
    (nth 1 (reverse (split-string s "\n"))))

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
        (if (region-active-p)
            (progn
                (copy-region-as-kill (region-beginning) (region-end))
                (goto-char (if up (region-beginning) (region-end)))
                (if up (open-line 1) (newline 1))
                (yank))
            (progn
                (copy-line 1)
                (beginning-of-line)
                (yank)
                (if up (previous-line))
                (move-to-column current)))))

(defun copy-line-up ()
    (interactive)
    (copy-line-fun t))

(defun copy-line-down ()
    (interactive)
    (copy-line-fun nil))

(defun my-comment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun tv-require (feature &optional filename noerror)
  (message "Loading %s..." (symbol-name feature))
  (condition-case err
      (if (require feature filename noerror)
          (message "Loading %s done" (symbol-name feature))
          (message "Loading %s Failed" (symbol-name feature)))
    (error
     (signal 'error (list feature (car err) (cadr err))))))

(when (tv-require 'winner)
  (defvar winner-boring-buffers-regexp "\\*[hH]elm.*")
  (defun winner-set1 (conf)
    ;; For the format of `conf', see `winner-conf'.
    (let* ((buffers nil)
           (alive
            ;; Possibly update `winner-point-alist'
            (cl-loop for buf in (mapcar 'cdr (cdr conf))
                     for pos = (winner-get-point buf nil)
                     if (and pos (not (memq buf buffers)))
                     do (push buf buffers)
                     collect pos)))
      (winner-set-conf (car conf))
      (let (xwins) ; to be deleted

        ;; Restore points
        (dolist (win (winner-sorted-window-list))
          (unless (and (pop alive)
                       (setf (window-point win)
                             (winner-get-point (window-buffer win) win))
                       (not (or (member (buffer-name (window-buffer win))
                                        winner-boring-buffers)
                                (string-match winner-boring-buffers-regexp
                                              (buffer-name (window-buffer win))))))
            (push win xwins))) ; delete this window

        ;; Restore marks
        (letf (((current-buffer)))
              (cl-loop for buf in buffers
                       for entry = (cadr (assq buf winner-point-alist))
                       for win-ac-reg = (winner-active-region)
                       do (progn (set-buffer buf)
                                 (set-mark (car entry))
                                 (setf win-ac-reg (cdr entry)))))
        ;; Delete windows, whose buffers are dead or boring.
        ;; Return t if this is still a possible configuration.
        (or (null xwins)
            (progn
              (mapc 'delete-window (cdr xwins)) ; delete all but one
              (unless (one-window-p t)
                (delete-window (car xwins))
                t)))
        )))

  (defalias 'winner-set 'winner-set1))
(winner-mode 1)

(defun is-tramp-mode ()
    (tramp-tramp-file-p (buffer-file-name (current-buffer))))

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(provide 'my-functions)
