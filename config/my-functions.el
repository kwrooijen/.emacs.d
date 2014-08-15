(defun sh (buffer-name)
    "Start a terminal and rename buffer."
    (interactive "sbuffer name: ")
    (eshell)
    (rename-buffer (format "%s%s" "$" buffer-name) t))

(defun sht (buffer-name)
    "Start a terminal and rename buffer."
    (interactive "sbuffer name: ")
    (ansi-term "/bin/sh")
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
    (async-shell-command (format "~/.emacs.d/scripts/erlang/erlang-flymake %s" buffer-file-name) "[Erlang Errors]"))

(defun run-haskell-test ()
    (interactive)
    (my-up-to-script "*.cabal" "cabal build ; cabal test --log=/dev/stdout" "[Haskell Tests]"))

(defun run-make (arg)
    (interactive)
    (my-up-to-script "Makefile" (concat "make " arg) "[Make Project]"))

(defun guard ()
    (interactive)
    (my-up-to-script "Guardfile" "bundle exec guard start --clear" "[Guard]"))

(defun run-mix (arg)
    (interactive)
    (my-up-to-script "mix.exs" (concat "mix " arg) "[Mix]"))

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
            (send-string-to-terminal "\033]12;Green\007"))))
            (message nil))

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

(when (require 'winner)
  (defvar winner-boring-buffers-regexp
    "\*[hH]elm.*\\|\*xhg.*\\|\*xgit.*")
  (defun winner-set1 (conf)
    ;; For the format of `conf', see `winner-conf'.
    (let* ((buffers nil)
           (alive
            ;; Possibly update `winner-point-alist'
            (loop for buf in (mapcar 'cdr (cdr conf))
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
            (push win xwins)))          ; delete this window

        ;; Restore marks
        (letf (((current-buffer)))
          (loop for buf in buffers
             for entry = (cadr (assq buf winner-point-alist))
             do (progn (set-buffer buf)
                       (set-mark (car entry))
                       (setf (winner-active-region) (cdr entry)))))
        ;; Delete windows, whose buffers are dead or boring.
        ;; Return t if this is still a possible configuration.
        (or (null xwins)
            (progn
              (mapc 'delete-window (cdr xwins)) ; delete all but one
              (unless (one-window-p t)
                (delete-window (car xwins))
                t))))))

  (defalias 'winner-set 'winner-set1))

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

(defadvice erlang-compile (after erlang-compile activate)
    "Load ebin beam files after compile"
        (let ((deps (upward-find-file "deps"))
              (ebin (upward-find-file "ebin"))
              (source-files '()))
        (if ebin (setq source-files (cons (concat ebin "ebin") source-files)))
        (if deps (progn
            (setq source-files (append source-files (mapcar (lambda(x) (concat (upward-find-file "deps") "deps/" x "/ebin"))
                (-filter (lambda(x) (and (not (equal x "..")) (not (equal x "."))))
                    (directory-files (concat (upward-find-file "deps") "/deps"))))))))
        (setq result (mapconcat (lambda (x) (format "\"%s\"" x)) source-files ", "))
        (unless (boundp 'node-is-set) (erl-choose-nodename))
        (setq node-is-set t)
        (erl-eval-expression (make-symbol (concat "emacs@" (car (split-string system-name "\\.")))) (format "code:add_paths([%s])." result))))

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"
  (interactive)
  (let ((dirname (expand-file-name
          (if startdir startdir ".")))
    (found nil) ; found is set as a flag to leave loop if we find it
    (top nil))  ; top is set when we get
            ; to / so that we only check it once

    ; While we've neither been at the top last time nor have we found
    ; the file.
    (while (not (or found top))
      ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
      (setq top t))

      ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
      (setq found t)
    ; If not, move up a directory
    (setq dirname (expand-file-name ".." dirname))))
    ; return statement
    (if found (concat dirname "/") nil)))

(defun capitalize-previous-word ()
  (interactive)
  (let ((old-point (point)))
      (backward-word)
      (capitalize-word 1)
      (goto-char old-point)))

(defadvice backward-sentence (before backward-sentence activate)
    (unless (region-active-p) (set-mark-command nil)))

(defadvice forward-sentence (before forward-sentence activate)
    (unless (region-active-p) (set-mark-command nil)))

(defadvice helm-register (before helm-register activate)
    (setq helm-register-active t))

(defadvice helm-register (after helm-register activate)
    (makunbound 'helm-register-active))

(defadvice helm-swoop (before helm-swoop activate)
    (setq helm-swoop-active t))

(defadvice helm-swoop (after helm-swoop activate)
    (makunbound 'helm-swoop-active))

(defun swap-lines-at-points (point1 point2)
    (goto-line point1)
    (beginning-of-line)
    (kill-line)
    (goto-line point2)
    (beginning-of-line)
    (yank)
    (kill-line)
    (goto-line point1)
    (beginning-of-line)
    (yank)
    (pop-mark))

(defun transpose-lines-at-point ()
  (interactive)
  (beginning-of-line)
  (let ((current (what-line-int)))
      (pop-to-mark-command)
      (let ((next (what-line-int)))
      (swap-lines-at-points current next))))

(defun what-line-int (&optional p)
    "Get the current line number as an int"
    (interactive)
    (save-restriction
        (widen)
        (save-excursion
            (beginning-of-line)
            (if p
                (1+ (count-lines 1 p))
                (1+ (count-lines 1 (point)))))))

(setq reg-num 0)
(defun inc-register ()
  (interactive)
  (setq reg-num (+ 1 reg-num))
  (point-to-register reg-num)
  (message nil))

(defun screenshot-frame ()
  (interactive)
  (shell-command-to-string
   (concat "sleep 1; "
           "import -window 0x2a00003 "
           "-crop 958x523+0+0 +repage /tmp/frames/`date +%s`.png")))

(defun camelcase-region (start end)
  "Changes region from snake_case to camelCase"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

(defun camelcase-word-or-region ()
  "Changes word or region from snake_case to camelCase"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (camelcase-region pos1 pos2)))

(defun camelcase-region+ (start end)
  "Changes region from snake_case to camelCase"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
      (capitalize-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

(defun camelcase-word-or-region+ ()
  "Changes word or region from snake_case to camelCase"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (camelcase-region+ pos1 pos2)))

(defun snakecase-word-or-region ()
  (interactive)
  (if mark-active (message "Don't have snakecase-region yet")
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds))))))

(provide 'my-functions)
